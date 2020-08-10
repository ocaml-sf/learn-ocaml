(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   simplify.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Simplifying OCaml types and typed expressions into RAML types and
 *   expressions.
 *)

(* ************************************************************************* *
 *   Implementation Details                                                  *
 *                                                                           *
 * This module takes the AST of OCaml modules in Typedtree format and        *
 * converts them to RAML expressions in five phases: "simplify," "uncurry    *
 * builtin," "instantiate polymorphism," "segment arrows," and "instantiate  *
 * constructors."  We summarize each phases here, further details can be     *
 * found in individual phase comments.                                       *
 *                                                                           *
 *  Simplify: filters out unsupperted syntax and produces the initial RAML   *
 *            expression.                                                    *
 *                                                                           *
 *  Uncurry Builtin: In OCaml standard and modules under raml_runtime        *
 *            folder, builtin functions and operators all have curried       *
 *            types for uniformity.  This phose corrects them to tuples as   *
 *            per RAML language definition.                                  *
 *                                                                           *
 *  Instantiate Polymorphism: duplicates polymorphic definitions to          *
 *            multiple monomorphic ones that RAML supports.                  *
 *                                                                           *
 *  Segment Arrows: rectifies the arrow types to match partial applications  *
 *            i.e. in [(f x y)], [f] must have type of form [[t1;t2] -> t3]  *
 *            even if [f] takes three arguments.                             *
 *                                                                           *
 *  Instantiate Constructors: further eliminates polymorphisms on user       *
 *            defined types by instantiating the constructors as if          *
 *            different instances of a polymorphic data type are different   *
 *            types.                                                         *
 *                                                                           *
 *   Design Rationale                                                        *
 *                                                                           *
 * Why branch off OCaml compilation process at Typedtree instead of the      *
 * prior (Parsetree) or latter (Lambda) stages?                              *
 *                                                                           *
 *  Type information is the main reason.  Parsetree format is too primitive  *
 *  and we do not want to re-implement the OCaml type checking monstrosity.  *
 *  Lambda does a lot of desugaring, which we would like to make use of,     *
 *  however, the type information has also been stripped off.  Although we   *
 *  can assume the program is well-typed and reconstruct them, it is         *
 *  considered an inferior choice especially when it comes to user defined   *
 *  data types.                                                              *
 *                                                                           *
 * Why this specific order of phases?                                        *
 *                                                                           *
 *  First of all, Typedtrees are too complicated to operate on, hence we do  *
 *  Simplify first.  Rest of the phases have the following dependencies:     *
 *                                                                           *
 *   Uncurry Builtin before Instantiate Polymorphism so that polymorphic     *
 *   builtin functions are marked "free" at appropriate places.              *
 *                                                                           *
 *   Instantiate Polymorphism before Segment Arrows so that a function that  *
 *   is used both partially applied to different number of argmuntes at      *
 *   different usages has been duplicated and renamed.                       *
 *                                                                           *
 *   Instantiate Polymorphism and Segment Arrows before Instantiate          *
 *   Constructors so that data constructors' types are determined.           *
 *                                                                           *
 * How are the phases glued together and how to use them?                    *
 *                                                                           *
 *  See the Driver section below and the accompanying mli file.              *
 * ************************************************************************* *)

(* OCaml core modules *)
open Core

(* OCaml modules *)
open Predef

(* RAML modules *)
open Expressions
open Rtypes

open Patterns

(* We open the Expressions and Rtypes modules, both defined in files under
   raml/, but access definitions in the Typedtree and Types module, both under
   typing/, by their full names because there are some name collisions. *)

exception Eunsupported_type     of Location.t * Types.type_expr * string
exception Eunsupported_pattern  of Typedtree.pattern
exception Eunsupported_constant of Location.t * Asttypes.constant
exception Eunsupported_expr     of Typedtree.expression
exception Eunsupported_primitive of string
exception Eno_main_expression
exception Enot_an_instance      of raml_type * raml_type
exception Emonomorphic_var      of string * raml_type * Location.t
                                          * raml_type * Location.t


(* ************************************************************************* *
 *   Constant Declarations                                                   *
 *                                                                           *
 * new_var_split_char: translation introduced variables, both program and    *
 *            type, are suffixed with the character followed by numbers.     *
 *                                                                           *
 * new_constr_split_str: instantiated data constructors are suffixed with    *
 *            their types separated by the character.                        *
 *                                                                           *
 * ident_*: identifiers for builtin modules.                                 *
 * ************************************************************************* *)
let new_var_split_char = '#'
let new_constr_split_str = "|"

let ident_pervasive = Ident.create_persistent "Pervasives"
let ident_raml      = Ident.create_persistent Rconfig.ocaml_raml_module
let ident_array     = Ident.create_persistent Rconfig.ocaml_array_module
let ident_nat       = Ident.create_persistent Rconfig.ocaml_nat_module


(* ************************************************************************* *
 *   Mutable States                                                          *
 *                                                                           *
 * Imparative states shared between multiple phases.  They are still private *
 * in the sense that they are initialized in the driver and different        *
 * sessions do not interfece with each other.                                *
 * ************************************************************************* *)

(* Number suffix for translation introduced variables *)
let next_var_num = ref 0

(* Maps type constructor names (string) to their type parameters and [Tind]
   types.  Roughly correspond to [Env.find_type_descrs]. *)
let tycon_map : (type_var list * raml_type) String.Map.t ref =
  ref String.Map.empty

(* cstr_id (string) -> tycon (string).  Result can be used to look up
   [tycon_map].  *)
let constr_map : string String.Map.t ref =
  ref String.Map.empty

(* [Map.find !constr_deg_map cstr_id = Some (n, deg)] means that [cstr_id]
   originally has type [(t_1, t_2, ... , t_n, t, t, ...,  t) -> t]
   in OCaml (a special case when [n = deg = 0], there is no arguments).
   Corresponding RAML constructor type when [n, deg] equals
     [0, 0]: [Tbase Tghost_unit -> t]
     [1, 0]: [t_1 -> t]
     [n, 0]: [Ttuple [t_1, t_2, ... , t_n] -> t]
     [0, d]: [Ttuple [Tbase Tghost_unit, t, t, ... , t] -> t]
     [1, d]: [Ttuple [t_1, t, t, ... , t] -> t]
     [n, d]: [Ttuple [Ttuple [t_1, t_2, ... , t_n], t, t, ... , t] -> t]

   Ref: [Rtypes.unfold].  *)
let constr_deg_map : (int * int) String.Map.t ref =
  ref String.Map.empty

(* Ensuring the same OCaml type variables are mapped to the same RAML
   variables.  *)
let type_var_map : raml_type Int.Map.t ref =
  ref Int.Map.empty

(* [Map.find !instance_map var_id = Some state] means that during instantiate
   polymorphism, the declaration of [var_id] needs type information from the
   use site.  When [state] equals
     [Inst_poly _]: variable should be duplicated with the new name and the
                    type added to the list.
     [Inst_mono (_, None)]: variable is considered monomorphic in RAML, no
                    duplication required, but update the option with the type.
     [Inst_mono (_, Some _)]: again, variable is monomorphic, but there is
                    already another usage, needs to check type equality.
 *)
type instantiation_state =
  | Inst_poly of (string * raml_type) list
  | Inst_mono of string * (raml_type * Location.t) option

let instance_map : instantiation_state String.Map.t ref =
  ref String.Map.empty

let initialize_simplify_state () =
    next_var_num := 0
  ; tycon_map := String.Map.empty
  ; constr_map := String.Map.empty
  ; constr_deg_map := String.Map.empty
  ; type_var_map := Int.Map.empty
  ; instance_map := String.Map.empty


(* ************************************************************************* *
 *   Phase 1: Simplify                                                       *
 *                                                                           *
 * Translating data structures defined in modules under typing/, espacially  *
 * [Types.type_expr], [Typedtree.pattern], and [Typedtree.expression].  See  *
 * how module-level structures are translated in the Driver section.         *
 *                                                                           *
 * [make_var_id] and [new_var_id] are used to generate fresh names for both  *
 * program and type variables, e.g. [tvar] for the latter.                   *
 *                                                                           *
 * [simplify_type] is the lone translator of type expressions.               *
 *                                                                           *
 * [simplify_pattern*] and [simplify_*_pattern] translate patterns of        *
 * different kinds and raise exceptions on unexpected ones.                  *
 *                                                                           *
 * The next group of functions, [simplify_constant], [simplify_pervasives],  *
 * [simplify_raml], [simplify_array], and [simplify_nat], translate builtin  *
 * names in each OCaml module to RAML builtins.                              *
 *                                                                           *
 * Finally, [simplify_exp], and the mutually recursive [simplify_clause],    *
 * translate a full OCaml expression to RAML.                                *
 * ************************************************************************* *)
let make_var_id base suffix =
  Printf.sprintf "%s%c%s" base new_var_split_char suffix

let new_var_id base =
  incr next_var_num; make_var_id base (Int.to_string !next_var_num)

let tvar tid nam =
    match Map.find !type_var_map tid, nam with
    | Some rty, _ -> rty
    | None, Some name -> let rty = Tvar { var_num = tid; var_name = "'" ^ name }
                         in type_var_map := Map.set !type_var_map tid rty
                          ; rty
    | None, None -> let rty = Tvar { var_num = tid; var_name = new_var_id "'" }
                    in type_var_map := Map.set !type_var_map tid rty
                     ; rty

let simplify_type : Location.t -> Env.t -> Types.type_expr -> raml_type =
  fun loc env ->
  let seen = ref Int.Set.empty in
  let rec rec_type t =
    (* Prevent infinite loop from unsupported recursive types, including
       mutually recursively defined data types (e.g. forest & tree) or types
       with nontrivial recursive structure (e.g. finger tree). *)
    if Set.mem !seen t.Types.id then
      raise (Eunsupported_type (loc, t, "unsupported recursive type"))
    else
      let orig = !seen in
      let t' = seen := Set.add orig t.Types.id; rec_type' t
      in seen := orig; t'

  and rec_type' t =
    match t.Types.desc with
    (* Under specified type which is never used, hence not instanciated *)
    | Types.Tvar _ when t.Types.level <> Btype.generic_level ->
    (* raise (Fail ((string_of_int t.Types.id) ^ ": " ^ (string_of_int t.Types.level))) *)
    Tbase Tghost_unit
    | Types.Tvar nam              -> tvar t.Types.id nam

    | Types.Tarrow (_, t1, t2, _) -> tarrow (rec_type t1) (rec_type t2)
    | Types.Ttuple ls             -> Ttuple (List.map ls rec_type)

    | Types.Tconstr (path, [], _) when Path.same path path_int
                                    -> Tbase Tint
    | Types.Tconstr (path, [], _) when Path.same path path_float
                                    -> Tbase Tfloat
    | Types.Tconstr (path, [], _) when Path.same path path_bool
                                    -> Tbase Tbool
    | Types.Tconstr (path, [], _) when Path.same path path_unit
                                    -> Tbase Tunit
    | Types.Tconstr (Path.Pdot (Path.Pident mod_ident, "ref", _), [ty], _)
                                  when Ident.same mod_ident ident_pervasive
                                    -> Tref (rec_type ty)
    | Types.Tconstr (Path.Pdot (Path.Pident mod_ident, "t", _), [], _)
                                  when Ident.same mod_ident ident_nat
                                    -> Tnat
    | Types.Tconstr (Path.Pdot (Path.Pident mod_ident, "t", _), [ty], _)
                                  when Ident.same mod_ident ident_array
                                    -> Tarray (rec_type ty)

    (* Always get ignored, but because we parse the [raise] in [raise e]
       alone first, we need to translate [exn] to something. *)
    | Types.Tconstr (path, [], _) when Path.same path path_exn
                                    -> Tbase Tghost_unit

    | Types.Tconstr (tycon, tys, _) ->
      let vars, ty = simplify_tycon t tycon in
      let subst = Toolbox.fold2_exn vars tys TVMap.empty
                    (fun m v t -> Map.set m v (rec_type t))
      in type_substitute subst ty

    | Types.Tlink ty -> rec_type ty
    | _ -> raise (Eunsupported_type (loc, t, "unrecognized construct"))

  and simplify_tycon t tycon =
    let tycon_str = Path.name tycon in
    match Map.find !tycon_map tycon_str with
    | Some p -> p
    | None ->
      let constrs, _ =
        try (* may raise Not_found, though should never on typedtree *)
            Env.find_type_descrs tycon env
        with Not_found ->
             raise (Eunsupported_type (loc, t, "unknown type constructor")) in
      match constrs with
      | [] -> raise (Eunsupported_type (loc, t,
                      "no data constructor (a record?)"))
      | { Types.cstr_res = { Types.desc = Types.Tconstr (_, params, _) } } :: _
        ->
        let vars = List.map params (fun ty -> match rec_type ty with
          | Tvar v -> v
          | _ -> raise (Eunsupported_type (loc, t,
                         "constructor returns complex type (GADT?)"))) in
        let p = vars, Tind (List.map constrs (simpl_constr t))
         in tycon_map := Map.set !tycon_map tycon_str p
          ; constr_map := Toolbox.fold constrs !constr_map
                            (fun m c -> Map.set m c.Types.cstr_name tycon_str)
          ; p
      | _ -> raise (Eunsupported_type (loc, t,
                      "constructor returns unrecognized type"))
  and simpl_constr t constr =
    let rec split_args (n, acc) = function
          | [] -> n, acc, 0
          | a :: args ->
            if Ctype.equal env true [a] [constr.Types.cstr_res] then
              (* start recursive fields *)
              if List.for_all args (fun t -> Ctype.equal env true [t] [a]) then
                n, acc, 1 + List.length args
              else
                raise (Eunsupported_type
                        (loc, t, "constructor " ^ constr.Types.cstr_name ^
                                 " has mixed order recursive fields"))
            else split_args (n + 1, rec_type a :: acc) args in
    let n, rargs, deg = split_args (0, []) constr.Types.cstr_args in
    let rarg = match rargs with
      | []    -> Tbase Tghost_unit
      | [rty] -> rty
      | _     -> Ttuple (List.rev rargs)
    in constr_deg_map := Map.set !constr_deg_map constr.Types.cstr_name (n, deg)
     ; { cstr_id = constr.Types.cstr_name
       ; cstr_type = rarg
       ; cstr_deg = deg }

  in rec_type

let simplify_pattern : Env.t -> Typedtree.pattern -> var_bind option =
  fun env p -> match p.Typedtree.pat_desc with
    | Typedtree.Tpat_any           ->
      None
    | Typedtree.Tpat_var (name, _) ->
      Some (Ident.name name,
            simplify_type p.Typedtree.pat_loc env p.Typedtree.pat_type)
    | Typedtree.Tpat_construct  (_, { Types.cstr_name = "()" }, [], _) ->
      None
    | Typedtree.Tpat_alias ({ Typedtree.pat_desc = Typedtree.Tpat_any },
                            name, _) ->
      Some (Ident.name name,
            simplify_type p.Typedtree.pat_loc env p.Typedtree.pat_type)
    | _ -> raise (Eunsupported_pattern p)

let simplify_pattern_exn : Env.t -> Typedtree.pattern -> var_bind =
  fun env p -> match simplify_pattern env p with
    | None -> raise (Eunsupported_pattern p)
    | Some p' -> p'

let simplify_pattern_anon : Env.t -> Typedtree.pattern -> var_bind =
  fun env p -> match simplify_pattern env p with
    | None ->
      new_var_id "pat", simplify_type p.Typedtree.pat_loc env p.Typedtree.pat_type
    | Some vt -> vt

let simplify_tuple_pattern : Env.t -> Typedtree.pattern -> var_bind list =
  fun env p -> match p.Typedtree.pat_desc with
    | Typedtree.Tpat_tuple ps -> List.map ps (simplify_pattern_anon env)
    | _ -> raise (Eunsupported_pattern p)

let simplify_construct_pattern
    : Env.t -> Typedtree.pattern -> constr_id * var_bind list =
  fun env p -> match p.Typedtree.pat_desc with
    | Typedtree.Tpat_construct (_, { Types.cstr_name = cons_id }, ps, _) ->
      cons_id, List.map ps (simplify_pattern_anon env)
    | _ -> raise (Eunsupported_pattern p)

let simplify_constant : Location.t -> Asttypes.constant -> constant =
  fun loc -> function
  | Asttypes.Const_int i -> Cint i
  | Asttypes.Const_char c -> Cint (Char.to_int c)
  | Asttypes.Const_float s -> Cfloat (Float.of_string s)
  | Asttypes.Const_int32 i -> Cint (Int32.to_int_exn i)
  | Asttypes.Const_int64 i -> Cint (Int64.to_int_exn i)
  | Asttypes.Const_nativeint i -> Cint (Nativeint.to_int_exn i)
  | c -> raise (Eunsupported_constant (loc, c))

let int_cmp_type   = Tarrow ([Tbase Tint;   Tbase Tint],   Tbase Tbool, ())
let nat_cmp_type   = Tarrow ([Tnat;   Tnat],   Tbase Tbool, ())
let float_cmp_type = Tarrow ([Tbase Tfloat; Tbase Tfloat], Tbase Tbool, ())

let mark_dummy e = { e with exp_info = fst3 e.exp_info, snd3 e.exp_info, true }
let is_dummy e = trd3 e.exp_info

let simplify_pervasives : ('a, 'b) expression -> string -> ('a, 'b) expression_desc =
  (* e serves as a dummy expression as well as carries the type *)
  let type_is_int_nat e =
    e.exp_type = int_cmp_type || e.exp_type = nat_cmp_type
  in
  fun e -> function
  | "not" -> Ebase_op Un_not
  | "~-"  -> Ebase_op Un_iminus
  | "~-." -> Ebase_op Un_fminus

  | "+"   -> Ebase_op Bin_iadd
  | "-"   -> Ebase_op Bin_isub
  | "*"   -> Ebase_op Bin_imult
  | "mod" -> Ebase_op Bin_imod
  | "/"   -> Ebase_op Bin_idiv

  | "+."  -> Ebase_op Bin_fadd
  | "-."  -> Ebase_op Bin_fsub
  | "*."  -> Ebase_op Bin_fmult
  | "/."  -> Ebase_op Bin_fdiv

  | "&&"  -> Ebase_op Bin_and
  | "&"   -> Ebase_op Bin_and
  | "||"  -> Ebase_op Bin_or
  | "or"  -> Ebase_op Bin_or

  | "="   -> Ebase_op Bin_eq
  | "<=" when type_is_int_nat e -> Ebase_op Bin_iless_eq
  | ">=" when type_is_int_nat e -> Ebase_op Bin_igreater_eq
  | "<"  when type_is_int_nat e -> Ebase_op Bin_iless
  | ">"  when type_is_int_nat e -> Ebase_op Bin_igreater
  | "<=" when e.exp_type = float_cmp_type -> Ebase_op Bin_fless_eq
  | ">=" when e.exp_type = float_cmp_type -> Ebase_op Bin_fgreater_eq
  | "<"  when e.exp_type = float_cmp_type -> Ebase_op Bin_fless
  | ">"  when e.exp_type = float_cmp_type -> Ebase_op Bin_fgreater
  | ("<=" | ">=" | "<" | ">") as s ->
    raise (Eunsupported_primitive
      ("Non-integral or floating-point comparison (" ^ s ^ ")"))

  (* See the Typedtree.Texp_apply case in simplify_exp *)
  | "ref" -> Eref (mark_dummy e)
  | "!"   -> Eref_deref (mark_dummy e)
  | ":="  -> Eref_assign (mark_dummy e, mark_dummy e)

  (* See the Typedtree.Texp_apply case in simplify_exp *)
  | "raise"       -> Eundefined

  | s     -> raise (Eunsupported_primitive s)

let simplify_raml : string -> int -> ('a, 'b) expression_desc =
  fun name line_num ->
    match name with
    | "undefined" -> Eundefined
    | "tick"      -> Etick 0.
    | "ref_swap"  -> Ebase_fun Ref_swap
    | "consume"   -> Ebase_fun (Res_consume line_num)
    | s     -> raise (Eunsupported_primitive (Rconfig.ocaml_raml_module ^ "." ^ s))

let simplify_array : string -> builtin_fun =
  function
  | "make"   -> Arr_make
  | "create" -> Arr_make
  | "set"    -> Arr_set
  | "get"    -> Arr_get
  | "length" -> Arr_length
  | s     -> raise (Eunsupported_primitive (Rconfig.ocaml_array_module ^ "." ^ s))

let simplify_nat : ('a, 'b) expression -> string -> ('a, 'b) expression_desc =
  fun e -> function
  | "zero"   -> Ebase_const Czero
  | "succ"   -> Ebase_fun Nat_succ
  | "to_int" -> Ebase_fun Nat_to_int
  | "of_int" -> Ebase_fun Nat_of_int
  | "add"    -> Ebase_fun Nat_add
  | "mult"   -> Ebase_fun Nat_mult
  | "minus"  -> Ebase_fun Nat_minus

  (* See the Typedtree.Texp_apply case in simplify_exp *)
  | "minusc" -> Ebase_fun (Nat_minusc (-1))

  | "div_mod" -> Ebase_fun Nat_div_mod

  (* See the Typedtree.Texp_apply case in simplify_exp *)
  | "ifz" ->
    let tmp = new_var_id "ifz" in
    let tmpt = Tnat in
    Enat_match (mark_dummy e, mark_dummy e, (tmp, tmpt),
               { e with exp_type = tmpt; exp_desc = Evar tmp })

  | s -> raise (Eunsupported_primitive (Rconfig.ocaml_nat_module ^ "." ^ s))

let unit_exp info =
  { exp_type = Tbase Tunit
  ; exp_kind = Enormal
  ; exp_info = info
  ; exp_desc = Ebase_const Cunit
  }

(* Note that the return type is NOT [typed_expression], but with the original
   [Typedtree.expression] and an extra [bool] in its [exp_info], which marks if
   this expression is a dummy used by [simplify_pervasives] or [simplify_nat].

   There is another (non-recursive) [simplify_exp] defined later that overrides
   this one and strips two extra fields away. *)

let rec simplify_exp : Typedtree.expression ->
		       (Location.t * Typedtree.expression * bool, unit) expression =
  let construct_pat p =
    match p.Typedtree.pat_desc with
    | Typedtree.Tpat_construct  (_, { Types.cstr_name = "()" }, [], _) ->
       false
    | Typedtree.Tpat_construct _ -> true
    | _ -> false
  in

  fun e ->
    let env = e.Typedtree.exp_env in
    let template = { exp_type = simplify_type e.Typedtree.exp_loc
                                  env e.Typedtree.exp_type
                   ; exp_kind = Enormal
                   ; exp_info = e.Typedtree.exp_loc, e, false
                   ; exp_desc = Eundefined } in
    let simplify_exp_desc desc =
          simplify_exp { e with Typedtree.exp_desc = desc } in
    { template with exp_desc = match e.Typedtree.exp_desc with

    | Typedtree.Texp_ident (Path.Pdot (Path.Pident mod_ident, op, _)
        as fullname, _, _) -> begin
      try
        if Ident.same mod_ident ident_pervasive
          then simplify_pervasives template op
        else if Ident.same mod_ident ident_raml
          then simplify_raml op e.Typedtree.exp_loc.Location.loc_start.Lexing.pos_lnum
        else if Ident.same mod_ident ident_array
          then Ebase_fun (simplify_array op)
        else if Ident.same mod_ident ident_nat
          then simplify_nat template op
        else
          raise Not_found
      with Not_found -> Evar (Path.name fullname)
      end

    | Typedtree.Texp_ident (name, _, _) -> Evar (Path.name name)

    | Typedtree.Texp_constant c ->
      Ebase_const (simplify_constant e.Typedtree.exp_loc c)

    | Typedtree.Texp_let (_,
        [{ Typedtree.pat_desc = Typedtree.Tpat_tuple _ } as p, e], body) ->
      Etuple_match (simplify_exp e,
                    simplify_tuple_pattern env p, simplify_exp body)

    | Typedtree.Texp_let (Asttypes.Recursive, ls, body) ->
      let ps, es = List.unzip ls
      in Eletrec (List.map ps (simplify_pattern_exn env),
                  List.map es simplify_exp,
                  simplify_exp body)

    | Typedtree.Texp_let (_, [pat, e], body) ->
      Elet (simplify_pattern env pat, simplify_exp e, simplify_exp body)

    | Typedtree.Texp_let (rflag, (pat, e) :: ls, body) ->
      Elet (simplify_pattern env pat, simplify_exp e,
            simplify_exp_desc (Typedtree.Texp_let (rflag, ls, body)))

    | Typedtree.Texp_function (_, [{ Typedtree.pat_desc = Typedtree.Tpat_tuple _ } as p, e_body], _) ->
       let rty = simplify_type p.Typedtree.pat_loc env p.Typedtree.pat_type in
       let tup_var = new_var_id "pat" in
       let tup_exp = { exp_desc = Evar tup_var
		     ; exp_kind = Enormal
		     ; exp_type = rty
		     ; exp_info = p.Typedtree.pat_loc, e_body, false
		     }
       in
       let body_simple = simplify_exp e_body in
       let body_simple =
	 { body_simple with
	   exp_desc = Etuple_match (tup_exp,
				    simplify_tuple_pattern env p, body_simple)
	 }
       in
       Elambda ((tup_var,rty), body_simple)

    | Typedtree.Texp_function (_, ((p, _) :: _ as clauses), _) when construct_pat p ->
      let rty = simplify_type p.Typedtree.pat_loc env p.Typedtree.pat_type in
      let template' = { template with exp_type = tapp template.exp_type rty } in
      let es = List.map clauses (simplify_clause env template') in
      let var = new_var_id "fun" in
      let evar = { exp_desc = Evar var; exp_type = rty; exp_kind = Enormal
                 ; exp_info = p.Typedtree.pat_loc, e, false }
      in Elambda ((var, rty), { template' with exp_desc = Ematch (evar, es) })

    | Typedtree.Texp_function (_, [p, e], _) ->
      Elambda (simplify_pattern_anon env p, simplify_exp e)

    | Typedtree.Texp_apply (f, ls)
      when List.for_all ls (Fn.compose is_some snd3) ->
      let f' = simplify_exp f in

      (* See simplify_pervasives, dealt separately to avoid examining the
         exception being raised. *)
      if f'.exp_desc  = Eundefined then Eundefined else

      let ls' = List.map ls (Fn.compose simplify_exp
                            (Fn.compose uw snd3)) in begin
      match f'.exp_desc, ls' with
      (* See simplify_pervasives *)
      | Eref _,        [a]    -> Eref a
      | Eref_deref d,  [a]       when is_dummy d -> Eref_deref a
      | Eref_assign _, [a; b] -> Eref_assign (a, b)

      (* See simplify_nat *)
      | Ebase_fun (Nat_minusc _),
        [{exp_desc = Ebase_const (Cint c)}; a] ->
        Eapp ((*call_name:*) None,
              { f' with exp_desc = Ebase_fun (Nat_minusc c)
                      ; exp_type = tapp f'.exp_type (Tbase Tint) },
              [a])

      | Enat_match (_, _, xt, n), a :: then_ :: else_ :: res ->
        let zero_branch = match then_.exp_desc with
          | Elambda ((x, _), lam) -> substitute lam x (fun _ _ i -> unit_exp i)
          | _ -> { then_ with
                   exp_desc = Eapp (None, then_, [unit_exp template.exp_info])
                 ; exp_type = tapp then_.exp_type (Tbase Tunit)
                 } in
        let succ_branch = match else_.exp_desc with
          | Elambda ((x, _), lam) -> subst_var lam x (fst xt)
          | _ -> { else_ with
                   exp_desc = Eapp (None, else_, [n])
                 ; exp_type = tapp else_.exp_type n.exp_type
                 } in
        if List.is_empty res
          then Enat_match (a, zero_branch, xt, succ_branch)
          else Eapp (None,
                 { f' with exp_type = zero_branch.exp_type
                 ; exp_desc = Enat_match (a, zero_branch, xt, succ_branch) },
                 res)


      (* See simplify_raml *)
      | Etick _, [{exp_desc = Ebase_const (Cfloat f)}] -> Etick f

      (* dereference can legitimately be at function position:
         let f = ref id in (!f) 1 *)
      | Eref_deref _,  _      -> Eapp ((*call_name:*) None, f', ls')

      (* ls' malformed for special cases from simplify_pervasives,
         simplify_nat, and simplify_raml. *)
      | (Eref _ | Eref_assign _ | Ebase_fun (Nat_minusc _) |
         Enat_match _ | Etick _),
        _ -> raise (Eunsupported_expr e)

      (* Collapse [(f x) y] into [f x y] *)
      | Eapp (_, f'', ls''), _ -> Eapp (None, f'', ls'' @ ls')

      (* simple application case *)
      | _ -> Eapp ((*call_name:*) None, f', ls')
      end

    | Typedtree.Texp_match (e, [{ Typedtree.pat_desc = Typedtree.Tpat_tuple _ } as p, body], _) ->
      Etuple_match (simplify_exp e,
                    simplify_tuple_pattern env p, simplify_exp body)

    | Typedtree.Texp_match (e', clauses, _) -> begin
      let e_ = simplify_exp e'  (* Have to happen first: process data type *)
      in try Ematch (e_, List.map clauses (simplify_clause env template))
         with Not_found -> raise (Eunsupported_expr e)
      end

    | Typedtree.Texp_tuple ls -> Etuple (List.map ls simplify_exp)

    | Typedtree.Texp_construct ({ Asttypes.txt = Longident.Lident "()" },
        _, _, _) -> Ebase_const Cunit
    | Typedtree.Texp_construct ({ Asttypes.txt = Longident.Lident "false" },
        _, _, _) -> Ebase_const (Cbool false)
    | Typedtree.Texp_construct ({ Asttypes.txt = Longident.Lident "true" },
        _, _, _) -> Ebase_const (Cbool true)

    | Typedtree.Texp_construct (_, { Types.cstr_name = cons_id }, ls, _) ->
      let n, deg = match Map.find !constr_deg_map cons_id with
        | Some p -> p
        | None -> raise (Eunsupported_expr e) in
      let es1, es2 = List.split_n (List.map ls simplify_exp) n in
      let ts1, ts2 = List.map es1 get_type, List.map es2 get_type in
      let non_rec = match n with
        | 0 -> { template with exp_type = Tbase Tghost_unit
                             ; exp_desc = Ebase_const Cunit
                             ; exp_kind = Efree }
        | 1 -> List.hd_exn es1
        | _ -> { template with exp_type = Ttuple ts1
                             ; exp_desc = Etuple es1
                             ; exp_kind = if deg > 0 then Efree else Enormal } in
      let arg = match deg with
        | 0 -> non_rec
        | _ -> { template with exp_type = Ttuple (non_rec.exp_type :: ts2)
                             ; exp_desc = Etuple (non_rec :: es2) } in
      Econst (cons_id, arg)

    | Typedtree.Texp_ifthenelse (if_, then_, else_) ->
      let else' = match else_ with
        | None -> { template with exp_desc = Ebase_const Cunit; exp_kind = Efree }
        | Some e' -> simplify_exp e'
      in Econd (simplify_exp if_, simplify_exp then_, else')

    | Typedtree.Texp_sequence (e1, e2) ->
      Elet (None, simplify_exp e1, simplify_exp e2)

    (* XXX: translating try as if it doesn't cost anything runtime *)
    | Typedtree.Texp_try (e, _) -> (simplify_exp e).exp_desc

    | _ -> raise (Eunsupported_expr e)
    }

and simplify_clause env template (pat, body) =
  let cons_id, ps = simplify_construct_pattern env pat in
  let n, deg = Map.find_exn !constr_deg_map cons_id in
  let body' = simplify_exp body in
  let prefix = "pat" in
  match n, deg with
  | 0, 0 -> cons_id, [new_var_id prefix, Tbase Tghost_unit], body'
  | 1, 0 -> cons_id, ps, body'
  | _, 0 ->
    let tmp = new_var_id prefix in
    let tmpt = Ttuple (List.map ps snd) in
    cons_id, [tmp, tmpt],
    { template with exp_kind = Efree; exp_desc =
      Etuple_match ({ template with exp_desc = Evar tmp; exp_type = tmpt
                                  ; exp_kind = Efree},
                    ps, body') }
  | 0, _ -> cons_id, (new_var_id prefix, Tbase Tghost_unit) :: ps, body'
  | 1, _ -> cons_id, ps, body'
  | _, _ ->
    let ps1, ps2 = List.split_n ps n in
    let tmp = new_var_id prefix in
    let tmpt = Ttuple (List.map ps1 snd) in
    cons_id, (tmp, tmpt) :: ps2,
    { template with exp_kind = Efree; exp_desc =
      Etuple_match ({ template with exp_desc = Evar tmp; exp_type = tmpt
                                  ; exp_kind = Efree},
                    ps1, body') }

(* Overwrites the recursive [simplify_exp] to remove the dummy-indicating
   boolean in the resulting tree. *)
let simplify_exp : Typedtree.expression -> typed_expression =
  let remove_info_bool rty k (info, e, dummy) =
        if dummy then raise (Eunsupported_expr e) else rty, k, info
  in fun e ->
(*     let _ = Printtyped.expression Format.std_formatter e in  *)
    let e' = unnest e in
(*     let _ = Printf.printf "unnested: \n\n" in *)
(*     let _ = Printtyped.expression Format.std_formatter e' in  *)
    (e_map remove_info_bool) (simplify_exp e')

(* (Completely) unsupported typedtree constructs
  | Texp_variant of label * expression option
  | Texp_record of
      (Longident.t loc * label_description * expression) list *
        expression option
  | Texp_field of expression * Longident.t loc * label_description
  | Texp_setfield of
      expression * Longident.t loc * label_description * expression
  | Texp_array of expression list
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t * string loc * expression * expression * direction_flag *
        expression
  | Texp_when of expression * expression
  | Texp_send of expression * meth * expression option
  | Texp_new of Path.t * Longident.t loc * Types.class_declaration
  | Texp_instvar of Path.t * Path.t * string loc
  | Texp_setinstvar of Path.t * Path.t * string loc * expression
  | Texp_override of Path.t * (Path.t * string loc * expression) list
  | Texp_letmodule of Ident.t * string loc * module_expr * expression
  | Texp_assert of expression
  | Texp_assertfalse
  | Texp_lazy of expression
  | Texp_object of class_structure * string list
  | Texp_pack of module_expr
*)


(* ************************************************************************* *
 *   Uncurry Builtin                                                         *
 *                                                                           *
 * Example:                                                                  *
 *   let a = RArray.make (RNat.succ RNat.zero) 1 in                          *
 *   let set = RArray.set a RNat.zero  (* Note the partial application *)    *
 *   in set 2                                                                *
 *                                                                           *
 * Is translated to                                                          *
 *   let a = (fun n d -> RArray.make (n, d)) (RNat.succ RNat.zero) 1 in      *
 *   let set = (fun a' n d -> RArray.set (a', n, d)) RNat.zero RNat.zero     *
 *   in set 2                                                                *
 * ************************************************************************* *)
let uncurry_builtin_exp =
  let eta_expand_builtin ?n_args e = match e.exp_type with
    | Tarrow ([], _, _) | Tarrow ([_], _, _) -> e  (* no need to uncurry *)
    | Tarrow (ts, rty, _) ->
       let (ps,ts,ts') =
	 let (ts1,ts2) =
	   match n_args with
	   | None -> (ts,[])
	   | Some n -> List.split_n ts n
	 in
	 let ps = List.map ts1 (fun t -> new_var_id "cy", t) in
	 (ps,ts1,ts2)
      in
      let arg = { e with exp_type = Ttuple ts; exp_kind = Efree
                       ; exp_desc = Etuple (List.map ps (fun (v, t) ->
                                      { e with exp_type = t; exp_kind = Efree
                                               ; exp_desc = Evar v })) } in
      let rty =
	match ts' with
	| [] -> rty
	| _ -> Tarrow (ts',rty,())
      in
      let e_typed = { e with exp_type = Tarrow ([arg.exp_type], rty, ()) } in
      let e' = { e with exp_type = rty; exp_kind = Efree
                      ; exp_desc = Eapp (None, e_typed, [arg]) }
      in Toolbox.fold_right ps e'
           (fun (v, t) body -> { body with exp_kind = Efree
                               ; exp_type = tarrow t body.exp_type
                               ; exp_desc = Elambda ((v, t), body) })
    | _ -> assert false in
  let expand_builtin e = match e.exp_desc with
    | Ebase_fun Arr_get -> eta_expand_builtin e ~n_args:2
    | Ebase_fun _ | Ebase_op _ -> eta_expand_builtin e
    | _ -> e
  in e_transform expand_builtin


(* ************************************************************************* *
 *   Instantiate Polymorphism                                                *
 *                                                                           *
 * Example:                                                                  *
 *   let id x = x in (id 1, id ())                                           *
 *                                                                           *
 * Is translated to                                                          *
 *   let id (x : 'a) : 'a = x in                                             *
 *   let id#1 (x : int) : int = x in                                         *
 *   let id#2 (x : unit) : unit = x                                          *
 *   in (id#1 1, id#2 ())                                                    *
 *                                                                           *
 * [evar] gives [id#1] and [id#2] their names; while [get_type_substitution] *
 * and [merge_type_substitution] (which may belong to rtypes.ml) produce and *
 * potentially merge (not need in this example) the mapping from ['a] to     *
 * [int] and [unit].                                                         *
 *                                                                           *
 * The criteria for duplicating a definition or not are based on its type    *
 * and syntax, encoded in [polymorphic_type] and [polymorphic_syntax],       *
 * respectively.  To actually duplicate one, usage information is collected  *
 * in [instance_map] and used in [duplicate_instances], except for pure      *
 * functions, like [id] in the example, which we optimized for in            *
 * [instantiate_polymorphism], the main loop of this phase.                  *
 * ************************************************************************* *)
let evar name rty loc =
  let res = ref (Evar name)
  in instance_map := Map.change !instance_map name begin function
       | None -> None
       | Some (Inst_poly ls) ->
         let name' = new_var_id name in
         let _ = res := Evar name'
         in Some (Inst_poly ((name', rty) :: ls))
       | Some (Inst_mono (x, None)) -> Some (Inst_mono (x, Some (rty, loc)))
       | Some (Inst_mono (x, Some (rty', loc'))) as ent ->
         if rty <> rty' then
         let _ = Printf.printf "rty: %s\n" (rtype_to_string rty) in
         let _ = Printf.printf "rty': %s\n" (rtype_to_string rty')
         in raise (Emonomorphic_var (x, rty', loc', rty, loc))
                        else ent
       end
   ; !res

let get_type_substitution'
    : raml_type TVMap.t -> raml_type -> raml_type -> raml_type TVMap.t =
  fun m gen inst ->
  let fail () = raise (Enot_an_instance (gen, inst)) in
  let rec get_rec m g i = match g, i with
    | Tbase a, Tbase b   when a = b -> m
    | Tnat,    Tnat      -> m
    | Tvar v,  ty        -> Map.change m v begin function
                            | None -> Some ty
                            | Some ty' -> if ty <> ty' then fail () else Some ty
                            end
    | Tarray a, Tarray b -> get_rec m a b
    | Tref a,  Tref b    -> get_rec m a b
    | Ttuple a, Ttuple b -> Toolbox.fold2_exn a b m get_rec
    | Tarrow (la, a, ()), Tarrow (lb, b, ()) ->
      let a_len = List.length la in
      let b_len = List.length lb in
      if a_len < b_len then
        let lb, lb_tail = List.split_n lb a_len in
        Toolbox.fold2_exn la lb (get_rec m a (Tarrow (lb_tail, b, ()))) get_rec
      else if a_len > b_len then
        let la, la_tail = List.split_n la b_len in
        Toolbox.fold2_exn la lb (get_rec m (Tarrow (la_tail, a, ())) b) get_rec
      else
        Toolbox.fold2_exn la lb (get_rec m a b) get_rec
    | Tind a, Tind b     -> Toolbox.fold2_exn a b m get_constr
    | _,      _          -> fail ()
    (* | _,      _          -> raise GotHere *)
  and get_constr m g i =
    if g.cstr_id <> i.cstr_id || g.cstr_deg <> i.cstr_deg then
      fail ()
    else get_rec m g.cstr_type i.cstr_type
  in get_rec m gen inst

let get_type_substitution
    : raml_type -> raml_type -> raml_type TVMap.t =
  get_type_substitution' TVMap.empty

let merge_type_substitution
    : raml_type TVMap.t -> raml_type TVMap.t -> raml_type TVMap.t =
  Map.merge ~f:(fun ~key -> function
    | `Both (t1, t2) -> if t1 <> t2 then raise (Enot_an_instance (t1, t2))
                                    else Some t1
    | `Left t | `Right t -> Some t)

let exp_deep_free = e_map (fun rty _ info -> rty, Efree, info)
let exp_shallow_free e = { e with exp_kind = Efree }

let rec polymorphic_type = function
  (* a.k.a. has Tvar and/or Tarrow w/ more than one argument*)
  | Tbase _ | Tnat -> false
  | Tarrow ([t1], t2, _) -> polymorphic_type t1 || polymorphic_type t2
  | Tvar _ | Tarrow (_, _, _) -> true
  | Tarray rty | Tref rty -> polymorphic_type rty
  | Ttuple ls -> List.exists ls polymorphic_type
  | Tind [] -> false  (* Nothing we can do here, should not happen *)
  | Tind cs -> List.exists cs (fun c -> polymorphic_type c.cstr_type)

let polymorphic_syntax =
  let rec f e = match e.exp_desc with
    | Elet (_, _, e') | Eletrec (_, _, e') -> f e'
    | Econd (_, e1, e2) | Enat_match (_, e1, _, e2) -> f e1 && f e2
    | Ematch (_, ls) -> List.for_all ls (fun (_, _, e') -> f e')

    | Ebase_const _
    | Evar _ | Elambda _ | Econst _ | Etuple _ | Eundefined | Etick _ -> true

    | _ -> false
  in f

(* Used by [instantiate_polymorphism].  Takes the body of a polymorphic
   definition (i.e. satisfies [polymorphic_type] and [polymorphic_syntax])
   and a list of types that are the concrete instantiations of the polymorphic
   type of the body.

   For a body of form

     [let x = ... in let (y, z) = ... in e]

   it returns

     [let x = ... in let (y, z) = ... in (e, e1, e2, ... en)]

   such that [e1] through [en] have types equivalent to each of
   [concrete_rtypes] and expressions are marked as free properly.
   (Note that the non-instantiated version [e] still exists.)
*)
let duplicate_instances e concrete_rtypes =
  let duplicated_type = Ttuple concrete_rtypes in
  let substitutions = List.map concrete_rtypes
                               (get_type_substitution e.exp_type) in
  let rec f e = match e.exp_desc with
    (* Should handle all the potentially true cases in [polymorphic_syntax]. *)
    (* Nested constructs *)
    | Elet (p, e1, e2) ->
      let e2' = f e2
      in { e with exp_desc = Elet (p, e1, e2'); exp_type = duplicated_type }
    | Eletrec (p, e1, e2) ->
      let e2' = f e2
      in { e with exp_desc = Eletrec (p, e1, e2'); exp_type = duplicated_type }
    | Econd (e1, e2, e3) ->
      let e2' = f e2 and e3' = f e3
      in { e with exp_desc = Econd (e1, e2', e3'); exp_type = duplicated_type }
    | Enat_match (e1, e2, p, e3) ->
      let e2' = f e2 and e3' = f e3
      in { e with exp_desc = Enat_match (e1, e2', p, e3')
                ; exp_type = duplicated_type }
    | Ematch (e1, ls) ->
      let ls' = List.map ls (fun (c, p, e2) -> c, p, f e2)
      in { e with exp_desc = Ematch (e1, ls')
                ; exp_type = duplicated_type }

    (* Simple duplication with top-level expression marked free. *)
    | Ebase_const _ | Evar _ | Elambda _ | Eundefined | Etick _ ->
      { e with exp_type = duplicated_type; exp_kind = Efree
      ; exp_desc = Etuple ((*e ::*)
          List.map substitutions (fun subst -> exp_shallow_free
                   (e_map_type (type_substitute subst) e)))
      }

    (* For constructors and tuples, we have to A-normalize the components.
       The polymorphic parts (e.g. first of [((fun x -> x), 1)]) will be
       instantiate later in a recursive call of [instantiate_polymorphism]
       because it is given a name. *)
    | Econst (c, e1) ->
      let v = new_var_id "cstr" in
      let p = Some (v, e1.exp_type) in
      let e1' = { e1 with exp_desc = Evar v; exp_kind = Efree } in
      let e' = { e with exp_desc = Econst (c, e1') } in
      let body =
        { e with exp_type = duplicated_type; exp_kind = Efree
        ; exp_desc = Etuple (
          List.map substitutions (fun subst -> exp_deep_free
                   (e_map_type (type_substitute subst) e')))
        }
      in { body with exp_kind = Efree; exp_desc = Elet (p, e1, body) }
    | Etuple ls ->
      let ps, ls' = List.unzip (List.map ls
                      (fun e1 -> let v = new_var_id "tpl" in
                         (Some (v, e1.exp_type), e1),
                          { e1 with exp_desc = Evar v; exp_kind = Efree })) in
      let e' = { e with exp_desc = Etuple ls' } in
      let body =
        { e with exp_type = duplicated_type; exp_kind = Efree
        ; exp_desc = Etuple (
          List.map substitutions (fun subst -> exp_deep_free
                   (e_map_type (type_substitute subst) e')))
        }
      in Toolbox.fold ps body
           (fun body (p, e1) -> { body with exp_kind = Efree
                                ; exp_desc = Elet (p, e1, body) })

    | _ -> raise (Invalid_argument "duplicate_instances")
  in f e

let get_poly_instantiation = function
  | Inst_poly ls -> ls
  | Inst_mono _  -> []
let get_mono_instantiation rty = function
  | Inst_poly _ | Inst_mono (_, None)
                            -> rty
  | Inst_mono (_, Some rty) -> rty

(* Duplicate polymorphic functions, instantiate the types, and rename each
   usage.  Not using [e_transform] because we need better control over the
   order (and the side effect) when processing let bindings. *)
let instantiate_polymorphism : typed_expression -> typed_expression =
  let rec inst_rec exp =
    { exp with exp_desc = match exp.exp_desc with
    | Evar name -> evar name exp.exp_type exp.exp_info

    | Eapp (cname, e, es) -> Eapp (cname, inst_rec e, List.map es inst_rec)
    | Elambda (p, e) -> Elambda (p, mask_vars [p] e)

    (* Optimized for the most common case [let f x = ... in ...] with [f]
       polymorphic: instantiate to multiple [let]-bindings instead of the
       more involved pairing and matching.  *)
    | Elet (Some (name, rty), ({ exp_desc = Elambda _ } as e), body)
      when polymorphic_type e.exp_type ->
      (* enter reader *)
      let orig_entry = ref None in
      let _ = instance_map := Map.change !instance_map name
                (fun ent -> orig_entry := ent; Some (Inst_poly [])) in

      (* rec body *)
      let body' = inst_rec body in

      (* leave reader *)
      let instances = ref [] in
      let _ = instance_map := Map.change !instance_map name
                (fun o -> instances := get_poly_instantiation (uw o);
                          !orig_entry) in
      let e' = inst_rec e in
      Elet (Some (name, rty), e',
        Toolbox.fold !instances body'
          (fun body' (name', rty') ->
            let subst = get_type_substitution rty rty' in
            { exp with exp_kind = Efree
            ; exp_desc =
              Elet (Some (name', rty'),
                { (inst_rec (e_map_type (type_substitute subst) e))
                  with exp_kind = Efree },
                body') }))

    | Elet (Some (name, rty), e, body)
      when polymorphic_type e.exp_type && polymorphic_syntax e ->
      (* enter reader *)
      let orig_entry = ref None in
      let _ = instance_map := Map.change !instance_map name
                (fun ent -> orig_entry := ent; Some (Inst_poly [])) in

      (* rec body *)
      let body' = inst_rec body in

      (* leave reader *)
      let instances = ref [] in
      let _ = instance_map := Map.change !instance_map name
                (fun o -> instances := get_poly_instantiation (uw o);
                          !orig_entry) in begin
      match !instances with
      | [] -> Elet (Some (name, rty), inst_rec e, body')
      | [name', rty'] ->
        let subst = type_substitute (get_type_substitution rty rty')
        in Elet (Some (name', rty'), inst_rec (e_map_type subst e), body')
      | duplicated_pattern ->
        let concrete_types = List.map duplicated_pattern snd in
        let duplicated_type = Ttuple concrete_types in
        Elet (Some (name, duplicated_type)
             , inst_rec (duplicate_instances e concrete_types)
             , { exp with exp_kind = Efree
               ; exp_desc = Etuple_match
                   ({ exp with exp_kind = Efree; exp_type = duplicated_type
                             ; exp_desc = Evar name }
                   , duplicated_pattern
                   , body')
               })
      end

    | Elet (Some (name, rty), e, body)
      when polymorphic_type e.exp_type ->
      let (body', subst) = mono_vars [name, rty] body
      in Elet (Some (name, type_substitute subst rty)
              , inst_rec (e_map_type (type_substitute subst) e)
              , body')

    | Elet (Some p, e1, e2) -> Elet (Some p, inst_rec e1, mask_vars [p] e2)
    | Elet (None, e1, e2) -> Elet (None, inst_rec e1, inst_rec e2)

    | Eletrec (xts, es, body)
      when List.exists xts (fun (_, rty) -> polymorphic_type rty) ->
      (* enter reader *)
      let orig_map = !instance_map in
      let xs = List.map xts fst in
      let _ = instance_map := Toolbox.fold xs orig_map
                (fun m x -> Map.set m x (Inst_poly [])) in

      (* rec body *)
      let body' = inst_rec body in

      (* leave reader & enter 2nd reader: removing all recursion entries for
         processing recursive bodies while memorizing the updates *)
      let instances = ref [] in
      let _ = instance_map := Toolbox.fold xs !instance_map
                (fun m x -> Map.change m x
                     (fun o -> instances := get_poly_instantiation (uw o) @
                                              !instances; None)) in

      let xts' = xts in  (* first set is copied unmodified *)
      let es' = List.map es inst_rec in

      let poly_def = Toolbox.fold2_exn xts es String.Map.empty
                       (fun m (x, t) e -> Map.set m x (t, e)) in
      (* This is where all the duplication happens *)
      let xts'', es'' = List.unzip (List.map
            (List.concat_map !instances
              (Fn.compose Map.to_alist (instantiate_mutrec poly_def)))
            (fun (x, (t, e)) -> (x, t), e)) in
      let body'' = match xts'' with
        | [] -> body'
        | _ -> { exp with exp_desc = Eletrec (xts'', es'', body')
                        ; exp_kind = Efree } in

      (* leave 2nd reader: restore from [orig_map] *)
      let _ = instance_map := Toolbox.fold xs !instance_map
                (fun m x -> match Map.find orig_map x with
                            | None -> m
                            | Some l -> Map.set m x l) in

      Eletrec (xts', es', body'')

    (* Optimized case: non-polymorphic, all single argument recursions don't
       need to be duplicated, e.g. [fib : int -> int]. *)
    | Eletrec (xts, es, body) ->
      Eletrec (xts, List.map es (mask_vars xts), mask_vars xts body)

    | Econd (e, e1, e2) -> Econd (inst_rec e, inst_rec e1, inst_rec e2)
    (* Eshare does not exist yet *)

    | Econst (c, e) -> Econst (c, inst_rec e)
    | Ematch (e, ls) ->
      let (cs, pss', es', subst) = Toolbox.fold ls ([], [], [], TVMap.empty)
        begin fun (cs, pss', es', full_subst) (c, ps, e2) ->

        let pattern_type = match ps with
            | [_, t] -> t
            | _ -> Ttuple (List.map ps snd) in
        let subst = get_type_substitution pattern_type (unfold e.exp_type c) in

        let ps' = List.map ps (fun (x, t) -> x, type_substitute subst t) in
        let (e2', subst') = mono_vars ps' (if Map.is_empty subst
                              then e2
                              else e_map_type (type_substitute subst) e2)
        in c :: cs, ps' :: pss', e2' :: es'
         , merge_type_substitution full_subst subst'

        end in
      let pss'' = List.map pss' (List.map ~f:(fun (c, t) ->
                                                   c, type_substitute subst t))
      in Ematch (inst_rec (e_map_type (type_substitute subst) e),
                 List.rev_map3_exn cs pss'' es' Tuple3.create)

    | Enat_match (e1, e2, p, e3) ->
      Enat_match (inst_rec e1, inst_rec e2, p, mask_vars [p] e3)

    | Eref e -> Eref (inst_rec e)
    | Eref_deref e -> Eref_deref (inst_rec e)
    | Eref_assign (e1, e2) -> Eref_assign (inst_rec e1, inst_rec e2)

    | Etuple es -> Etuple (List.map es inst_rec)
    | Etuple_match (e1, ps, e2) ->
      let (e2', subst) = mono_vars ps e2 in
      let ps' = List.map ps (fun (x, rty) -> x, type_substitute subst rty)
      in Etuple_match (inst_rec (e_map_type (type_substitute subst) e1)
                      , ps', e2')

    (* leaf nodes *)
    | d -> d
    }
  and mask_vars ps e =
      let orig_map = !instance_map in
      let _ = instance_map := Toolbox.fold ps orig_map
                                           (fun m (x, _) -> Map.remove m x) in
      let e' = inst_rec e in
      let _ = instance_map := Toolbox.fold ps !instance_map
                (fun m (x, _) -> match Map.find orig_map x with
                                 | None -> m
                                 | Some ent -> Map.set m x ent)
      in e'
  and mono_vars ps e =
      let orig_map = !instance_map in
      let _ = instance_map := Toolbox.fold ps orig_map
                (fun m (x, _) -> Map.set m x (Inst_mono (x, None))) in

      let e' = inst_rec e in

      let post_map = !instance_map in
      let subst = Toolbox.fold ps TVMap.empty (fun m (x, gen) ->
        match Map.find post_map x with
        | Some (Inst_mono (_, Some (rty, _))) -> get_type_substitution' m gen rty
        | _ -> m) in
      let _ = instance_map := Toolbox.fold ps post_map
                (fun m (x, _) -> match Map.find orig_map x with
                                 | None -> Map.remove m x
                                 | Some ent -> Map.set m x ent)
      in e', subst
  and instantiate_mutrec (poly_def : (raml_type * typed_expression) String.Map.t)
        (start_name, start_rty) =
    let start_base_name, suffix =
          String.lsplit2_exn start_name ~on:new_var_split_char in

    (* DFS through all mutually recursively defined functions from start_name
       (tail-recursive on the second argument, only process each function
       once, time and space linear to poly_def). *)
    let rec subst_rec acc = function
      | [] -> acc
      | (base_name, new_rty) :: ls ->
        let new_name = make_var_id base_name suffix in
        match Map.find acc new_name with
        | Some (rty, _) when new_rty = rty -> subst_rec acc ls
        | Some (rty, _) -> raise (Enot_an_instance (new_rty, rty))

        | None ->  (* Actually process (new_name, new_rty) *)
          let def_rty, def_body = Map.find_exn poly_def base_name in

          let rty_subst = get_type_substitution def_rty new_rty in
          let new_body_with_type =
                e_map_type (type_substitute rty_subst) def_body in

          let additional_subst = ref [] in
          let new_body = { (inst_rec (Map.fold poly_def
                ~init:new_body_with_type
                ~f:(fun ~key:name ~data:(rty, _) e ->
                    let d = Evar (make_var_id name suffix) in
                    substitute e name (fun rty' k info ->
                      additional_subst := (name, rty') :: !additional_subst
                      ; { exp_desc = d
                        ; exp_type = rty'
                        ; exp_kind = k
                        ; exp_info = info })
                   )
                 )) with exp_kind = Efree }
          in subst_rec (Map.set acc new_name (new_rty, new_body))
                       (!additional_subst @ ls)
    in subst_rec String.Map.empty [start_base_name, start_rty]

  in inst_rec


(* ************************************************************************* *
 *   Segment Arrows                                                          *
 *                                                                           *
 * We apply a varient of control flow analysis using annotated types to      *
 * figure out how fully functions are applied.  Since [rtype] is defined so  *
 * that we can easily put the annotations on the arrow types, we simply      *
 * instantiate it with annotations being lists of [segment_marker]s.         *
 *                                                                           *
 * Example: a function [f] with annotated arrow type ([tint = Tbase Tint])   *
 *   [Tarrow ([tint; tint; tint], tint, [m1; m2; m3)] means                  *
 *                                                                           *
 *                     m1    m2    m3                                        *
 *            f : [tint; tint; tint] -> tint                                 *
 *                                                                           *
 * where [m1] annotates the potential arrow between the first two [tint],    *
 * [m2] the second and the third, and [m3], for consistency, the last arrow. *
 * For a [marker : segment_marker], only [marked_segmenting] is relevent to  *
 * the analysis.  When [!(find_unifiable marker.marked_segmenting)] equals:  *
 *   [false]: there is no evidence that the arrow has to be segmented at     *
 *            this point (default)                                           *
 *   [true]: the arrow should be segmented into two separate [Tarrow]s       *
 *           at this point.  E.g. tint -> [tint; tint] -> tint for [m1]      *
 *                                                                           *
 * The core of the marker is a [unifiable], which is a disjoint-set data     *
 * structure implemented with references with standard union-find algorithms *
 * defined without path compression.                                         *
 *                                                                           *
 * [segment_exp_arrows], the main function of the phase, first mark all the  *
 * types in the AST with annotations using [mark_raml_type], unify markers   *
 * that have to be equal, and assign to markers at each function application.*
 * The intermediate expression has type [segment_marked_expression], and is  *
 * walked through again to remove the annotations as well as segmenting      *
 * arrows using [strip_markers].                                             *
 * ************************************************************************* *)
type 'a unifiable_desc =
  | Uhere of 'a
  | Uchain of 'a unifiable
and 'a unifiable = 'a unifiable_desc ref

let create_unifiable a = ref (Uhere a)

let rec find_unifiable' u = match !u with
  | Uhere _ -> u
  | Uchain u' -> find_unifiable' u'
let find_unifiable u = match !(find_unifiable' u) with
  | Uhere a -> a
  | _ -> assert false
let unify_unifiable a b join =
  let ra = find_unifiable' a in
  let rb = find_unifiable' b
  in if phys_equal ra rb then ()
       else match !ra, !rb with
            | Uhere va, Uhere vb -> ra := Uhere (join va vb); rb := Uchain ra
            | _ -> assert false

type segment_marker =
  { mutable marked_segmenting : bool ref unifiable
  (* No longer used
    (* rest of the fields are all for better debug messages *)
  ; source_symbol : string
  ; mutable marked_location : Location.t
  ; mutable marked_type_full : segment_marked_type
  *)
  }
and segment_markers =  segment_marker list
and segment_marked_type = segment_markers rtype

type segment_marked_expression = (typed, segment_markers) expression

let create_marker symbol =
  { marked_segmenting = create_unifiable (ref false)
  (*
  ; source_symbol = symbol
  ; marked_location = Location.none
  ; marked_type_full = Tbase Tghost_unit
  *)
  }

let mark_raml_type symbol : raml_type -> segment_marked_type =
  let new_marker _ = create_marker symbol in
  let rec mark_rec = function
    | Tbase b             -> Tbase b
    | Tnat                -> Tnat
    | Tvar v              -> Tvar v
    | Tarray rty          -> Tarray (mark_rec rty)
    | Tref rty            -> Tref (mark_rec rty)
    | Ttuple ls           -> Ttuple (List.map ls mark_rec)
    | Tind _ as rty       -> mark_ind rty
    | Tarrow (ls, rty, _) ->
      Tarrow (List.map ls mark_rec, mark_rec rty, List.map ls new_marker)
  and mark_ind = function
    | Tind (c :: _) as rty ->
      let tycon = Map.find_exn !constr_map c.cstr_id in
      let vars, gen_rty = Map.find_exn !tycon_map tycon in
      let subst = get_type_substitution gen_rty rty in

      let gen_rty' = match gen_rty with
        | Tind cs -> Tind (List.map cs mark_cstr) | _ -> assert false in
      let subst' = Map.map subst ~f:mark_rec
      in type_substitute ~join:(@) subst' gen_rty'
    | _ -> assert false
  and mark_cstr c = { cstr_id = c.cstr_id; cstr_deg = c.cstr_deg
                    ; cstr_type = mark_rec c.cstr_type }
  in mark_rec

let strip_markers : segment_marked_type -> raml_type =
  let rec strip_rec = function
    | Tbase b             -> Tbase b
    | Tnat                -> Tnat
    | Tvar v              -> Tvar v
    | Tarray rty          -> Tarray (strip_rec rty)
    | Tref rty            -> Tref (strip_rec rty)
    | Ttuple ls           -> Ttuple (List.map ls strip_rec)
    | Tind cs             -> Tind (List.map cs strip_cstr)
    | Tarrow ([], rty, _) -> strip_rec rty
    | Tarrow (t1 :: ts, rty, m1 :: ms) ->
      let rty' = strip_rec (Tarrow (ts, rty, ms)) in
      let t1' = strip_rec t1
      in if !(find_unifiable m1.marked_segmenting)
           then Tarrow ([t1'], rty', ())
           else tarrow t1' rty'
    | _ -> raise (Invalid_argument "strip_markers")
  and strip_cstr c = { cstr_id = c.cstr_id; cstr_deg = c.cstr_deg
                     ; cstr_type = strip_rec c.cstr_type }
  in strip_rec

let mark_as marker to_seg loc fulltype =
  let marked = find_unifiable marker.marked_segmenting
  in marked := !marked || to_seg

let unify_marked_type =
  let unify_marker t1 t2 m1 m2 =
    unify_unifiable m1.marked_segmenting m2.marked_segmenting
      (fun marked1 marked2 -> ref (!marked1 || !marked2))
  in
  let rec unify_rec t1 t2 = match t1, t2 with
    | Tbase b1,   Tbase b2 when b1 = b2 -> ()
    | Tnat,       Tnat                  -> ()
    | Tvar v1,    Tvar v2 when v1 = v2  -> ()
    | Tarray m1,  Tarray m2             -> unify_rec m1 m2
    | Tref m1,    Tref m2               -> unify_rec m1 m2
    | Ttuple ls1, Ttuple ls2            -> List.iter2_exn ls1 ls2 unify_rec
    | Tind cs1,   Tind cs2              -> List.iter2_exn cs1 cs2 unify_cstr
    | Tarrow (ls1, m1, mk1), Tarrow (ls2, m2, mk2) ->
        unify_rec m1 m2
      ; List.iter2_exn ls1 ls2 unify_rec
      ; List.iter2_exn mk1 mk2 (unify_marker t1 t2)
    | _ ->
    let _ = Printf.printf "t1: %s\n" (rtype_to_string (t_map (fun _ -> ()) t1)) in
    let _ = Printf.printf "t2: %s\n" (rtype_to_string (t_map (fun _ -> ()) t2)) in
    raise (Invalid_argument "unify_marked_type")
  and unify_cstr c1 c2 = unify_rec c1.cstr_type c2.cstr_type
  in unify_rec

(* Just like [tarrow] and [tapp] in [Rtypes], except understand markers and
   manipulate accordingly. *)
let marked_tarrow t m = function
  | Tarrow (ts, rty, ms) -> Tarrow (t :: ts, rty, m :: ms)
  | rty                  -> Tarrow ([t], rty, [m])
let marked_tapp loc tarr args =
  let rec tapp ts mk args = match ts, mk, args with
    | t :: ts, m :: mk, [a] ->
      unify_marked_type t a; mark_as m true loc tarr; ts, mk
    | t :: ts, m :: mk, a :: args ->
      unify_marked_type t a; mark_as m false loc tarr; tapp ts mk args
    | _, _, [] -> raise (Invalid_argument "marked_tapp (empty args)")
    | _        -> raise (Invalid_argument "marked_tapp (mismatched lists)")
  in match tarr with
     | Tarrow (ts, t, mk) -> let ts', mk' = tapp ts mk args
                             in if ts' = [] then t else Tarrow (ts', t, mk')
     | _ -> raise (Invalid_argument "marked_tapp (not an arrow)")

let segment_exp_arrows exp =
  let rec mark_seg curr_symbol type_map e : segment_marked_expression =
    let desc, marked_type = match e.exp_desc with
      | Ebase_const c ->
        Ebase_const c, mark_raml_type (string_of_constant c) e.exp_type
      | Ebase_fun f ->
        Ebase_fun f,   mark_raml_type (string_of_builtin_fun f) e.exp_type
      | Ebase_op o ->
        Ebase_op o,    mark_raml_type (string_of_builtin_op o) e.exp_type
      | Eundefined ->
        Eundefined,    mark_raml_type "undefined" e.exp_type
      | Etick n ->
        Etick n,       mark_raml_type "tick" e.exp_type

      | Evar v -> Evar v, Map.find_exn type_map v

      | Eapp (nam, e1, es) ->
        let e1' = mark_seg curr_symbol type_map e1 in
        let es' = List.map es (mark_seg curr_symbol type_map)
        in Eapp (nam, e1', es')
         , marked_tapp e.exp_info e1'.exp_type (List.map es' get_type)
      | Elambda ((v, t), e1) ->
        let t' = mark_raml_type v t in
        let e1' = mark_seg curr_symbol (Map.set type_map v t') e1
        in Elambda ((v, t'), e1')
         , marked_tarrow t' (create_marker curr_symbol) e1'.exp_type

      | Elet (None, e1, e2) ->
        let e1' = mark_seg "_" type_map e1 in
        let e2' = mark_seg curr_symbol type_map e2
        in Elet (None, e1', e2'), e2'.exp_type
      | Elet (Some (v, t), e1, e2) ->
        let e1' = mark_seg v type_map e1 in
        let t' = e1'.exp_type in
        let e2' = mark_seg curr_symbol (Map.set type_map v t') e2
        in Elet (Some (v, t'), e1', e2'), e2'.exp_type
      | Eletrec (ps, es, e2) ->
        let ps' = List.map ps (fun (x, t) -> x, mark_raml_type x t) in
        let type_map' = Toolbox.fold ps' type_map
                          (fun m (x, t) -> Map.set m x t) in
        let es' = List.map2_exn ps' es (fun (x, _) -> mark_seg x type_map') in
        let e2' = mark_seg curr_symbol type_map' e2
        in List.iter2_exn ps' es'
                          (fun (_, t) e -> unify_marked_type t e.exp_type)
         ; Eletrec (ps', es', e2'), e2'.exp_type

      | Econd (e0, e1, e2) ->
        let e0' = mark_seg "<if>" type_map e0 in
        let e1' = mark_seg curr_symbol type_map e1 in
        let e2' = mark_seg curr_symbol type_map e2 in
        let _ = unify_marked_type e1'.exp_type e2'.exp_type
        in Econd (e0', e1', e2'), e1'.exp_type
      | Eshare _ -> failwith "segment_exp_arrows::annot_rec (Eshare)"

      | Econst (c, e1) ->
        let e1' = mark_seg curr_symbol type_map e1 in
        let t' = mark_raml_type curr_symbol e.exp_type in
        let _ = unify_marked_type (unfold t' c) e1'.exp_type
        in Econst (c, e1'), t'
      | Ematch (e1, ls) ->
        let e1' = mark_seg "<Cmatch>" type_map e1 in
        let t2' = mark_raml_type curr_symbol e.exp_type in
        let ls' = List.map ls (mark_clause curr_symbol type_map e1'.exp_type t2')
        in Ematch (e1', ls'), t2'

      | Enat_match (e0, e1, (x, _), e2) ->
        let e0' = mark_seg "<Nmatch>" type_map e0 in
        let e1' = mark_seg curr_symbol type_map e1 in
        let e2' = mark_seg curr_symbol (Map.set type_map x Tnat) e2 in
        let _ = unify_marked_type e1'.exp_type e2'.exp_type
        in Enat_match (e0', e1', (x, Tnat), e2'), e2'.exp_type

      | Eref e1 ->
        let e1' = mark_seg curr_symbol type_map e1
        in Eref e1', Tref e1'.exp_type
      | Eref_deref e1 ->
        let e1' = mark_seg curr_symbol type_map e1 in
        let t' = match e1'.exp_type with Tref t' -> t' | _ -> assert false
        in Eref_deref e1', t'
      | Eref_assign (e1, e2) ->
        let e1' = mark_seg "<.:=>" type_map e1 in
        let e2' = mark_seg "<:=.>" type_map e2 in
        let _ = unify_marked_type e1'.exp_type (Tref e2'.exp_type)
        in Eref_assign (e1', e2'), Tbase Tunit

      | Etuple es ->
        let es' = List.map es (mark_seg curr_symbol type_map)
        in Etuple es', Ttuple (List.map es' get_type)
      | Etuple_match (e1, ps, e2) ->
        let e1' = mark_seg "<Tmatch>" type_map e1 in
        let ts' = match e1'.exp_type with Ttuple ts' -> ts'  | _ -> assert false in
        let ps' = List.map2_exn ps ts' (fun (x, _) t' -> x, t') in
        let type_map' = Toolbox.fold ps' type_map
                          (fun m (x, t) -> Map.set m x t) in
        let e2' = mark_seg curr_symbol type_map' e2
        in Etuple_match (e1', ps', e2'), e2'.exp_type
    in { exp_desc = desc; exp_type = marked_type
       ; exp_info = e.exp_info; exp_kind = e.exp_kind }
  and mark_clause curr_symbol type_map t_ind t_res = match t_ind with
    | Tind cs -> begin function
      | c, (x, _) :: ps, e ->
        let t0 = (List.find_exn cs ~f:(fun d -> d.cstr_id = c)).cstr_type in
        let ps' = (x, t0) :: List.map ps (fun (x, _) -> x, t_ind) in
        let map' = Toolbox.fold ps' type_map (fun m (x, t) -> Map.set m x t) in
        let e' = mark_seg curr_symbol map' e in
        let _ = unify_marked_type t_res e'.exp_type
        in c, ps', e'
      | _ -> assert false
      end
    | _ -> raise (Invalid_argument "segment_exp_arrows::mark_clause") in

  let rec seg_exp e : typed_expression =
    let template = { exp_type = strip_markers e.exp_type; exp_kind = e.exp_kind
                   ; exp_info = e.exp_info; exp_desc = Ebase_const Cunit }
    in { template with exp_desc = match e.exp_desc with
    | Ebase_const c -> Ebase_const c
    | Ebase_fun f -> Ebase_fun f
    | Ebase_op op -> Ebase_op op
    | Evar x -> Evar x

    | Eapp (nam, f, es) ->
      let f' = seg_exp f in
      let es' = List.map es seg_exp in
      let markers = match f.exp_type with Tarrow (_, _, m) -> m | _ -> [] in
      let e1' = List.hd_exn es' in
      let app = seg_app markers (List.tl_exn es')
                        { template with
                          exp_desc = Eapp (nam, f', [e1'])
                        ; exp_type = tapp f'.exp_type e1'.exp_type
                        ; exp_kind = Enormal }
      in app.exp_desc

    | Elambda ((x, t), lam) -> Elambda ((x, strip_markers t), seg_exp lam)

    | Elet (xt, e1, e2) -> Elet (seg_opattern xt, seg_exp e1, seg_exp e2)
    | Eletrec (xs, es, e2) ->
      Eletrec (seg_patterns xs, List.map es seg_exp, seg_exp e2)

    | Econd (e1, e2, e3) -> Econd (seg_exp e1, seg_exp e2, seg_exp e3)
    | Eshare (e1, (x, t), (y, s), e2) ->
      Eshare (seg_exp e1, (x, strip_markers t), (y, strip_markers s), seg_exp e2)

    | Econst (c, e') -> Econst (c, seg_exp e')
    | Ematch (e', cs) ->
      let cs' = List.map cs (fun (c, ps, e1) -> (c, seg_patterns ps, seg_exp e1))
      in Ematch (seg_exp e', cs')

    | Enat_match (e1, e2, (x, t), e3) ->
      Enat_match (seg_exp e1, seg_exp e2, (x, strip_markers t), seg_exp e3)

    | Eref e' -> Eref (seg_exp e')
    | Eref_deref e' -> Eref_deref (seg_exp e')
    | Eref_assign (e1, e2) -> Eref_assign (seg_exp e1, seg_exp e2)

    | Etuple es -> Etuple (List.map es seg_exp)
    | Etuple_match (e1, ps, e2) ->
      Etuple_match (seg_exp e1, seg_patterns ps, seg_exp e2)

    | Eundefined -> Eundefined
    | Etick f -> Etick f
    }
  and seg_app markers args f =
    match markers, args with
    | [], _ | _, [] -> f
    | m :: ms, e1 :: es ->
      let new_type = tapp f.exp_type e1.exp_type in
      match !(find_unifiable m.marked_segmenting), f.exp_desc with
      | true, _ ->
        seg_app ms es { f with exp_desc = Eapp (None, f, [e1])
                             ; exp_type = new_type }
      | false, Eapp (nam, f', es') ->
        seg_app ms es { f with exp_desc = Eapp (nam, f', es' @ [e1])
                             ; exp_type = new_type }
      | _ -> raise (Invalid_argument "segment_exp_arrows::seg_app")
  and seg_patterns ps = List.map ps (fun (x, t) -> x, strip_markers t)
  and seg_opattern op = Option.map op (fun (x, t) -> x, strip_markers t) in

  let exp' = mark_seg "<toplevel>" String.Map.empty exp
  in seg_exp exp'

(* ************************************************************************* *
 *   Instantiate Constructors                                                *
 *                                                                           *
 * This phase changes the names of data constructors (in the program as well *
 * as the types) to make them monomorphic.  The by-product, a map from       *
 * constructor names ([string]) to the inductive data type it belongs        *
 * ([raml_type]), is also returned for type checking.                        *
 *                                                                           *
 * The instantiation is a two-phase process.  First all the types in the     *
 * expression (types of all sub-expressions as well as bindings) are gone    *
 * through to change all the [cstr_id] in the [Tind] types.  This results in *
 * an inconsistent state, but is rectified by copying the appropriate names  *
 * from the now-instantiated [cstr_id] in [exp_type] to where they are used  *
 * -- constructing and pattern matching.                                     *
 * ************************************************************************* *)
let instantiate_constructors
    : typed_expression -> typed_expression * raml_type String.Map.t =
  fun e ->
  let cstr_inst_map = ref String.Map.empty in
  let rec inst_type = function
    | Tarray rty           -> Tarray (inst_type rty)
    | Tref   rty           -> Tref (inst_type rty)
    | Ttuple ls            -> Ttuple (List.map ls inst_type)
    | Tarrow (ls, rty, ()) -> Tarrow (List.map ls inst_type, inst_type rty, ())
    | Tind ((c :: _) as cs) as rty ->
      let tycon = Map.find_exn !constr_map c.cstr_id in
      let vars, gen_rty = Map.find_exn !tycon_map tycon in
      let subst = get_type_substitution gen_rty rty in
      let insts = List.map vars (Fn.compose inst_type
                                            (Map.find_exn subst)) in
      let suffix = new_constr_split_str ^ rtype_list_to_string insts in
      let cs' = List.map cs (fun c ->
                  { cstr_id = c.cstr_id ^ suffix
                  ; cstr_type = inst_type c.cstr_type
                  ; cstr_deg = c.cstr_deg }) in
      let rty' = Tind cs'
      in cstr_inst_map := Toolbox.fold cs' !cstr_inst_map (fun m c ->
                            Map.set m c.cstr_id rty')
       ; rty'
    | rty -> rty in

  let inst_constr constr = function
    | Tind cs ->
      let prefix = constr ^ new_constr_split_str
      in (List.find_exn cs ~f:(fun c -> String.is_prefix c.cstr_id prefix))
           .cstr_id
    | rty -> raise (Enot_an_instance (Tind [], rty)) in
  let inst_constr_rec exp = match exp.exp_desc with
    | Econst (constr, e) ->
      { exp with exp_desc =
        Econst (inst_constr constr exp.exp_type, e) }
    | Ematch (e, ls) ->
      { exp with exp_desc =
        Ematch (e, List.map ls (fun (c, xts, e') ->
                                inst_constr c e.exp_type, xts, e')) }
    | _ -> exp in

  let e' = e_transform inst_constr_rec @@ e_map_type inst_type @@ e
   in e', !cstr_inst_map

(* ************************************************************************* *
 *   Driver                                                                  *
 *                                                                           *
 * [simplify_expression] shows how to chain all the phases together, and is  *
 * used by [simplify_structure], which treat the module as a whole program.  *
 * [simplify_module] applies the first two phases on every (potentially open)*
 * definitions, and applies the latter three only after constructed a closed *
 * definition in order to return a list of closed expressions.               *
 * ************************************************************************* *)
let simplify_expression e =
  let _ = initialize_simplify_state () in
  let e', cstr_inst_map =
        instantiate_constructors @@ segment_exp_arrows @@
        instantiate_polymorphism @@ uncurry_builtin_exp @@ simplify_exp e 
  in e', cstr_inst_map, !constr_map

let simplify_structure =
  let rec collapse_items = function
    | [] -> None

    | Typedtree.Tstr_eval e :: ls -> begin
      match collapse_items ls with
      | None -> Some e
      | Some e' ->
        Some { e' with Typedtree.exp_loc = e.Typedtree.exp_loc
             ; Typedtree.exp_desc = Typedtree.Texp_sequence (e, e') }
      end

    | Typedtree.Tstr_value (rflag, es) :: ls -> begin
      match collapse_items ls with
      | None -> let _ =
        List.map es (fun (p, _) ->
          match p.Typedtree.pat_desc with
          | Typedtree.Tpat_var (ident, _) ->
            Warnings.print Format.err_formatter
              (Warnings.Unused_value_declaration (Ident.name ident))
          | _ -> 0)
        in None

      | Some e' ->
        Some { e' with
               Typedtree.exp_loc = (fst (List.hd_exn es)).Typedtree.pat_loc
             ; Typedtree.exp_desc = Typedtree.Texp_let (rflag, es, e') }
      end

    | _ :: ls -> collapse_items ls
  in fun str ->
  match collapse_items (List.map str.Typedtree.str_items
                                 (fun si -> si.Typedtree.str_desc)) with
  | None -> raise Eno_main_expression
  | Some e -> simplify_expression e

let simplify_module =
  let merge_cstr_inst_map ~key = function
    | `Both (t1, t2) -> assert (t1 = t2); Some t1
    | `Left t | `Right t -> Some t in
  let rec all_pat_are_var = function
    | [] -> true
    | ({ Typedtree.pat_desc = Typedtree.Tpat_var  _ }, _) :: es -> all_pat_are_var es
    | _ -> false in
  let rec pat_are_all_var = function
    | [] -> true
    | { Typedtree.pat_desc = Typedtree.Tpat_var  _ } :: ps -> pat_are_all_var ps
    | _ -> false in
  let rec collect_definitions ((closed, bare, cstr_inst_map) as acc) = function
    | Typedtree.Tstr_value (_, []) :: ls -> collect_definitions acc ls

    | Typedtree.Tstr_value (Asttypes.Nonrecursive,
        ({ Typedtree.pat_desc = Typedtree.Tpat_var (id, _) }, e) :: es) :: ls ->
      let name = Ident.name id in
      let e' = uncurry_builtin_exp @@ simplify_exp e in
      let e_closed = bare e' in
      let bare' body = bare
        { exp_desc = Elet (Some (name, e'.exp_type), e', body)
        ; exp_type = body.exp_type
        ; exp_kind = Efree
        ; exp_info = e'.exp_info } in
      let e_closed', new_inst_map =
        instantiate_constructors @@ segment_exp_arrows @@
        instantiate_polymorphism @@ e_closed
      in collect_definitions
           ((name, e_closed') :: closed,
            bare',
            Map.merge cstr_inst_map new_inst_map merge_cstr_inst_map)
           (Typedtree.Tstr_value (Asttypes.Nonrecursive, es) :: ls)

    | Typedtree.Tstr_value (Asttypes.Nonrecursive,
        ({ Typedtree.pat_desc = Typedtree.Tpat_tuple pats }, e) :: es) :: ls
      when pat_are_all_var pats ->
      let names = List.map pats (function
        | { Typedtree.pat_desc = Typedtree.Tpat_var (id, _) } -> Ident.name id
        | _ -> assert false) in
      let e' = uncurry_builtin_exp @@ simplify_exp e in
      let ts' = match e'.exp_type with
        | Ttuple ts -> ts
        | _ -> assert false (* OCaml expression assigned to tuple pattern but
                               translated version not a tuple type? *) in
      let bindings' = List.zip_exn names ts' in

      let bare' body = bare
        { exp_desc = Etuple_match (e', bindings', body)
        ; exp_type = body.exp_type
        ; exp_kind = Efree
        ; exp_info = e'.exp_info } in

      let closed', new_inst_map = Toolbox.fold bindings' (closed, cstr_inst_map)
        (fun (closed, cstr_inst_map) (name, t) ->
          let var =
            { exp_desc = Evar name; exp_type = t;
              exp_kind = Efree; exp_info = e'.exp_info } in
          let e_closed', new_inst_map =
            instantiate_constructors @@ segment_exp_arrows @@
            instantiate_polymorphism @@ bare' var
          in (name, e_closed') :: closed,
             Map.merge cstr_inst_map new_inst_map merge_cstr_inst_map)

      in collect_definitions (closed', bare', new_inst_map)
           (Typedtree.Tstr_value (Asttypes.Nonrecursive, es) :: ls)

    (* recursive definitions *)
    | Typedtree.Tstr_value (recflag, bindings) :: ls
      when all_pat_are_var bindings ->
      let pats, es = List.unzip bindings in
      let names = List.map pats (function
        | { Typedtree.pat_desc = Typedtree.Tpat_var (id, _) } -> Ident.name id
        | _ -> assert false) in
      let es' = List.map es (Fn.compose uncurry_builtin_exp simplify_exp) in
      let bindings' = List.zip_exn names (List.map es' (fun e -> e.exp_type)) in

      let bare' body = bare
        { exp_desc = Eletrec (bindings', es', body)
        ; exp_type = body.exp_type
        ; exp_kind = Efree
        ; exp_info = (List.hd_exn es').exp_info } in

      let vars = List.map2_exn names es' (fun name e' ->
        name,
        { exp_desc = Evar name; exp_type = e'.exp_type;
          exp_kind = Efree; exp_info = e'.exp_info }) in
      let closed', new_inst_map = Toolbox.fold vars (closed, cstr_inst_map)
        (fun (closed, cstr_inst_map) (name, var) ->
          let e_closed', new_inst_map =
            instantiate_constructors @@ segment_exp_arrows @@
            instantiate_polymorphism @@ bare' var
          in (name, e_closed') :: closed,
             Map.merge cstr_inst_map new_inst_map merge_cstr_inst_map)

      in collect_definitions (closed', bare', new_inst_map) ls

    | Typedtree.Tstr_value (_, es) :: ls ->
      Format.fprintf Format.err_formatter
        "Module parsing encountered unsupported top-level declaration:\n";
      List.iter es (fun (p, _) ->
        Format.fprintf Format.err_formatter "  %a\n" Printtyped.pattern p
      );
      Format.fprintf Format.err_formatter "Stopped parsing to avoid error@.";
      acc

    | (Typedtree.Tstr_eval _ | Typedtree.Tstr_primitive _ |
       Typedtree.Tstr_type _ | Typedtree.Tstr_exception _ |
       Typedtree.Tstr_exn_rebind _ | Typedtree.Tstr_module _ |
       Typedtree.Tstr_recmodule _| Typedtree.Tstr_modtype (_, _, _) |
       Typedtree.Tstr_open _ | Typedtree.Tstr_class _ |
       Typedtree.Tstr_class_type _ | Typedtree.Tstr_include _) :: ls ->
      collect_definitions acc ls

    | [] -> acc in
    (* | _ -> acc in *)
  fun str ->
  let _ = initialize_simplify_state () in
  let ls, _, cstr_inst_map =
      collect_definitions
        ([], Fn.id, String.Map.empty)
        (List.map str.Typedtree.str_items (fun si -> si.Typedtree.str_desc))
  in List.rev ls, cstr_inst_map, !constr_map
