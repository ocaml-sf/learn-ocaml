(* Implementation details:
This file contains the functions for transform expressions with nested patterns
to equivalent expression without nested patterns. To briefly describe, the algorithm
converts a match expression into one that systematically matches on each possible
constructor of a variant type. Since we can freely exchange the order of different
constructors, we group the instances of each constructor together, strip away the
outer-most pattern match on the constructor, and recursively continue on the inner
patterns. In this Implementation, the function that implements the pattern compilation is "m".

We also convert all matches to let-normal form before the pattern compilation. Additional
simplications are made as well, such as converting wild cards to fresh variables and
removing type constraints.

Supported patterns:
- nested tuple and constructors
- type constraints
- wild cards
- or patterns (see below)

Only outer-most or patterns are supported:

match e with
| p1 ->
| p2 ->
...
| pn-1 ->
| pn -> e'
...

where p1...pn all binds exactly the same variables.

Here's an example of the evaluation of m:

match x with
| (nil,nil) -> e1
| ((f,x)::xs, nil) -> e2
| (nil,y::ys) -> e3
| wild -> e4

For example, the above expression would be passed to m as:

m [x] [([(nil,nil)], e1); ([((f,x)::xs, nil)], e2); ([(nil,y::ys)], e3); ([wild], e4)] e_raise_match_fail

=>

m [x] [([(nil,nil)], e1); ([((f,x)::xs, nil)], e2); ([(nil,y::ys)], e3)] (m [x] [([wild], e4)] e_raise_match_fail)

=>

m [x] [([(nil,nil)], e1); ([((f,x)::xs, nil)], e2); ([(nil,y::ys)], e3)] (m [] [([], [x/wild]e4)] e_raise_match_fail)

=>

m [x] [([(nil,nil)], e1); ([((f,x)::xs, nil)], e2); ([(nil,y::ys)], e3)] ([x/wild]e4)

=>

match x with
| (u1,u2) ->  m [u1;u2] [([nil;nil], e1); ([(f,x)::xs; nil], e2); ([nil; y::ys], e3)] ([x/wild]e4)

=>

match x with
| (u1,u2) ->
  match u1 with
  | nil -> m [u2] [([nil], e1); ([y::ys], e3)] ([x/wild]e4)
  | u3::u4 ->   m [u3;u4;u2] [([(f,x); xs; nil], e2)] ([x/wild]e4)

=>

match x with
| (u1,u2) ->
  match u1 with
  | nil ->
    match u2 with
    | nil -> m [] [([], e1)] ([x/wild]e4)
    | u5::u6 -> m [u5;u6] [([y; ys], e3)] ([x/wild]e4)
  | u3::u4 ->
    match u3 with
    | (u7,u8) -> m [u9;u10;u4;u2] [([f; x; xs; nil], e2)] ([x/wild]e4)

=>

match x with
| (u1,u2) ->
  match u1 with
  | nil ->
    match u2 with
    | nil -> e1
    | u5::u6 -> m [u6] [([ys], [u5/y]e3)] ([x/wild]e4)
  | u3::u4 ->
    match u3 with
    | (u7,u8) -> m [u10;u4;u2] [([x; xs; nil], [u9/f]e2)] ([x/wild]e4)

=>

match x with
| (u1,u2) ->
  match u1 with
  | nil ->
    match u2 with
    | nil -> e1
    | u5::u6 -> m [] [([], [u5/y;u6/ys]e3)] ([x/wild]e4)
  | u3::u4 ->
    match u3 with
    | (u7,u8) -> m [u4;u2] [([xs; nil], [u9/f;u10/x]e2)] ([x/wild]e4)

=>

match x with
| (u1,u2) ->
  match u1 with
  | nil ->
    match u2 with
    | nil -> e1
    | u5::u6 -> [u5/y;u6/ys]e3
  | u3::u4 ->
    match u3 with
    | (u7,u8) -> m [u2] [([nil], [u9/f;u10/x;u4/xs]e2)] ([x/wild]e4)

=>

match x with
| (u1,u2) ->
  match u1 with
  | nil ->
    match u2 with
    | nil -> e1
    | u5::u6 -> [u5/y;u6/ys]e3
  | u3::u4 ->
    match u3 with
    | (u7,u8) ->
      match u2 with
      | nil -> m [] [([], [u9/f;u10/x;u4/xs]e2)] ([x/wild]e4)
      | u11:u12 -> m [] [] ([x/wild]e4)

=>

match x with
| (u1,u2) ->
  match u1 with
  | nil ->
    match u2 with
    | nil -> e1
    | u5::u6 -> [u5/y;u6/ys]e3
  | u3::u4 ->
    match u3 with
    | (u7,u8) ->
      match u2 with
      | nil -> [u9/f;u10/x;u4/xs]e2
      | u11:u12 -> [x/wild]e4 *)


(* OCaml core modules *)
open Core.Std

(* OCaml modules *)
open Predef

(* RAML modules *)
open Expressions

(* copy definitions from simplify.ml *)

exception Fail of string
exception Eunsupported_type     of Location.t * Types.type_expr * string
exception Eunsupported_pattern  of Typedtree.pattern
exception Eunsupported_constant of Location.t * Asttypes.constant
exception Eunsupported_expr     of Typedtree.expression

let ident_pervasive = Ident.create_persistent "Pervasives"
let next_var_num = ref 0
let new_var_split_char = '#'

let make_var_id base suffix =
  Printf.sprintf "%s%c%s" base new_var_split_char suffix

let new_var_id base =
  incr next_var_num; make_var_id base (Int.to_string !next_var_num)

let new_var_id_1 base =
  let var = new_var_id base in
  Ident.create var

  (* printing functions *)

 module MyIteratorArgument = struct
   include TypedtreeIter.DefaultIteratorArgument

    let enter_pattern p = match p.Typedtree.pat_desc with
      | Tpat_var (id, _) ->
          Format.printf "@[<2>%s@ : %a@]@."
            (Ident.name id)
            Printtyp.raw_type_expr p.pat_type
      | _ -> ()

    (* let enter_expression e = match e.Typedtree.exp_desc with
    | Typedtree.Texp_ident (_,_,_) ->
      Printf.printf "exp type: ";
      Printtyp.raw_type_expr Format.std_formatter e.Typedtree.exp_type;
      Printtyped.expression Format.std_formatter e
    | _ -> () *)
  end

  module Iterator = TypedtreeIter.MakeIterator(MyIteratorArgument)

  module CMap = Map.Make (String)

  let print_cstr : Types.constructor_description -> unit =
    fun cstr ->
      Printf.printf "name: %s\n" cstr.Types.cstr_name;
      Printf.printf "type: ";
      Printtyp.type_expr Format.std_formatter cstr.Types.cstr_res;
      Printf.printf "\nargs: [";
      let _ = List.map (cstr.Types.cstr_args) (fun t ->
        Printtyp.type_expr Format.std_formatter t;
        Printf.printf "; ";
        ) in
      Printf.printf "]"

  let print_env : Env.t -> unit =
    fun env ->
      Printf.printf "\n\n-------- ENV ---------";
      Env.fold_constructors (fun cstr _ ->
        Printf.printf "\n***: ";
        print_cstr cstr;
        Printf.printf "\n"
        ) None env ();
      Printf.printf "-------- END ENV ---------"

  (* end printing *)

  (* convenient constructors *)

  let mk_exp : Env.t -> Typedtree.expression_desc -> Types.type_expr -> Typedtree.expression =
    fun env desc t ->
    { Typedtree.exp_desc = desc;
      Typedtree.exp_loc = Location.none;
      Typedtree.exp_extra = [];
      Typedtree.exp_type = t;
      Typedtree.exp_env = env}

  let mk_exp_var : Env.t -> Path.t -> Types.type_expr -> Typedtree.expression =
    fun env path t ->
      let val_desc =
      {
        Types.val_type = t;
        Types.val_kind = Types.Val_reg;
        Types.val_loc = Location.none;
      } in
      let desc = Typedtree.Texp_ident (path, Location.mknoloc (Longident.Lident ""), val_desc) in
      mk_exp env desc t

  let mk_pat : Env.t -> Typedtree.pattern_desc -> Types.type_expr -> Typedtree.pattern =
    fun env desc t ->
      { Typedtree.pat_desc = desc;
        Typedtree.pat_loc = Location.none;
        Typedtree.pat_extra = [];
        Typedtree.pat_type = t;
        Typedtree.pat_env = env}

  let type_raise t = Ctype.newty (Types.Tarrow ("", type_exn, t, Types.Cok))

  let rec path_to_long : Path.t -> Longident.t =
    fun path -> match path with
    | Path.Pident ident -> Longident.Lident (Ident.name ident)
    | Path.Pdot (p,s,_) -> Longident.Ldot (path_to_long p,s)
    | Path.Papply (p1,p2) -> Longident.Lapply (path_to_long p1, path_to_long p2)

  (* this is the typedtree for "raise (Match_failure ("...",begin,end))". Use when the original match
    is nonexhaustive, i.e match e with x::xs -> e1 *)
  let e_raise_match_fail : Env.t -> Types.type_expr -> Typedtree.expression = fun env t ->
    let e1 = mk_exp env (Typedtree.Texp_constant (Asttypes.Const_string "match failure")) type_string  in
    let e2 = mk_exp env (Typedtree.Texp_constant (Asttypes.Const_int 0)) type_int in
    let e3 = mk_exp env (Typedtree.Texp_constant (Asttypes.Const_int 0)) type_int in
    let e_tuple = mk_exp env (Typedtree.Texp_tuple ([e1;e2;e3])) (Ctype.newty (Types.Ttuple [type_string; type_int; type_int])) in
    let cstr_exn =
      try Env.lookup_constructor (path_to_long path_match_failure) env
      with Not_found -> raise (Fail "no match_failure constructor") in
    let e_fail = mk_exp env (Typedtree.Texp_construct (Location.mknoloc (Longident.Lident ""), cstr_exn, [e_tuple], false)) type_exn in
    let e_raise = mk_exp_var env (Path.Pdot (Path.Pident ident_pervasive, "raise", -1)) (type_raise t) in
    let e_raise_fail = mk_exp env (Typedtree.Texp_apply (e_raise, [("", Some e_fail, Typedtree.Required)])) t in
    e_raise_fail

  let get_var : Typedtree.pattern -> Path.t =
    fun p -> match p.Typedtree.pat_desc with
    | Typedtree.Tpat_var (ident, _) -> Path.Pident ident
    | _ -> raise (Eunsupported_pattern p)

  (* substitute an expression for an identifier in an expression. subst p1 p2 exp = [p1/p2]exp. *)
  let rec subst : Typedtree.expression -> Path.t -> Typedtree.expression -> Typedtree.expression =
    fun p1 p2 exp -> match exp.Typedtree.exp_desc with
    | Typedtree.Texp_ident (path, loc, val_desc) ->
      if Path.same p2 path then {exp with Typedtree.exp_desc = p1.Typedtree.exp_desc}
      else exp
    | Typedtree.Texp_constant _ -> exp
    | Typedtree.Texp_let (rflag, pairs, e) ->
      let undef, def = List.split_while pairs ~f:(fun (p,_) ->
        let bound_list = List.map (Typedtree.pat_bound_idents p) (fun (ident,_) -> Path.Pident ident ) in
           List.exists bound_list (Fn.compose not (Path.same p2))
        || List.is_empty bound_list
        ) in
      let undef' = List.map undef (fun (p,e) -> p, subst p1 p2 e) in
      let def', e' =
        match def with
        | [] -> [], subst p1 p2 e
        | (pi,ei)::xs -> (pi, subst p1 p2 ei)::xs, e
      in {exp with Typedtree.exp_desc = Typedtree.Texp_let (rflag, undef' @ def', e')}

    | Typedtree.Texp_function (label, pairs, part) ->
      let pairs' = List.map pairs (fun (p,e) ->
        let bound_list = List.map (Typedtree.pat_bound_idents p) (fun (ident,_) -> Path.Pident ident) in
        if List.exists bound_list (Path.same p2) then (p,e)
        else (p, subst p1 p2 e)
      ) in
      {exp with Typedtree.exp_desc = Typedtree.Texp_function (label, pairs', part)}

    | Typedtree.Texp_apply (e, es) ->
      let es' = List.map es (fun (l,e,o) ->
        match e with
        | Some e' -> (l, Some (subst p1 p2 e'), o)
        | _ -> (l,e,o)
      ) in
      {exp with Typedtree.exp_desc = Typedtree.Texp_apply (subst p1 p2 e, es')}

    | Typedtree.Texp_match (e, pairs, part) ->
      let e' = subst p1 p2 e in
      let pairs' = List.map pairs (fun (p,e) ->
        let bound_list = List.map (Typedtree.pat_bound_idents p) (fun (ident,_) -> Path.Pident ident) in
        if List.exists bound_list (Path.same p2) then (p, e)
        else (p, subst p1 p2 e)
    ) in
      {exp with Typedtree.exp_desc = Typedtree.Texp_match (e', pairs', part)}

    | Typedtree.Texp_try (e, pairs) ->
      let e' = subst p1 p2 e in
      let pairs' = List.map pairs (fun (p,e) ->
        let bound_list = List.map (Typedtree.pat_bound_idents p) (fun (ident,_) -> Path.Pident ident) in
        if List.exists bound_list (Path.same p2) then (p,e)
        else (p, subst p1 p2 e)
      ) in
      {exp with Typedtree.exp_desc = Typedtree.Texp_try (e', pairs')}

    | Typedtree.Texp_tuple es ->
      let es' = List.map es (subst p1 p2) in
      {exp with Typedtree.exp_desc = Typedtree.Texp_tuple es'}

    | Typedtree.Texp_construct (loc, desc, es, b) ->
      let es' = List.map es (subst p1 p2) in
      {exp with Typedtree.exp_desc = Typedtree.Texp_construct (loc, desc, es', b)}

    | Typedtree.Texp_ifthenelse (e1, e2, e3) ->
    {exp with Typedtree.exp_desc = Typedtree.Texp_ifthenelse (subst p1 p2 e1, subst p1 p2 e2, Option.map e3 (subst p1 p2))}

    | Typedtree.Texp_sequence (e1, e2) ->
    {exp with Typedtree.exp_desc = Typedtree.Texp_sequence (subst p1 p2 e1, subst p1 p2 e2)}

    | _ -> raise (Fail "unsupported substitutions")

  let get_type_params : Types.type_expr -> Types.type_expr list =
    fun t -> match t.Types.desc with
    | Types.Tconstr (_,ts,_) -> ts
    | _ -> raise (Fail "cannot get params for non constructor type")

    (* returns all the constructor_description's of a constructor type (aka variant type. However, note that
      polymorphic variants are represented as Tpat_variant and Texp_variant in the Typedtree.
      These are not related.
      i.e. : variant_cstrs env ('a list) = [nil, cons(['a, 'a list])]) *)
  let rec variant_cstrs : Env.t -> Types.type_expr -> Types.constructor_description list =
    fun env t -> match t.Types.desc with
      | Types.Tconstr (path,ts,_) ->
      let descs = fst (Env.find_type_descrs path env) in
      List.map descs (fun desc ->
        let params = get_type_params desc.Types.cstr_res in
        let ty_args = List.map desc.Types.cstr_args (fun t -> Ctype.apply env params t ts) in
        let ty_res = Ctype.apply env params desc.Types.cstr_res ts in
        {desc with Types.cstr_args = ty_args ;
                   Types.cstr_res = ty_res})
      | Tlink t' -> variant_cstrs env t'
      | _ ->
      (let _ = Printf.printf "can't find constructors for non-variant type: " in
      let _ = Printtyp.raw_type_expr Format.std_formatter t in
      let _ = Printtyp.type_expr Format.std_formatter t in
      raise (Fail "variant_cstrs"))

  let remove_wild : Typedtree.pattern -> Typedtree.pattern =
    fun p -> match p.Typedtree.pat_desc with
    | Typedtree.Tpat_any ->
      let v = new_var_id_1 "wild" in
      {p with Typedtree.pat_desc = Typedtree.Tpat_var (v, Location.mknoloc "")}
    | _ -> p

  let remove_constraint : Typedtree.pattern -> Typedtree.pattern =
    fun p -> match p.Typedtree.pat_extra with
    | [(Typedtree.Tpat_constraint _, _)] -> {p with Typedtree.pat_extra = []}
    | _ -> p

  let remove_alias :Typedtree.pattern -> Typedtree.pattern =
    fun p -> match p.Typedtree.pat_desc with
    | Typedtree.Tpat_alias ({Typedtree.pat_desc = Typedtree.Tpat_any}, id, loc) -> {p with Typedtree.pat_desc = Typedtree.Tpat_var (id,loc)}
    | _ -> p

  let rec strip_type : Types.type_expr -> Types.type_expr =
    fun t -> match t.Types.desc with
    | Types.Tlink t' -> strip_type t'
    | _ -> t

  let expand_or : (Typedtree.pattern * Typedtree.expression) list -> (Typedtree.pattern * Typedtree.expression) list =
    fun pes ->
      let rec dup (p,e) = match p.Typedtree.pat_desc with
        | Typedtree.Tpat_or (p1,p2,None) -> dup (p1,e) @ dup(p2, e)
        | _ -> [(p,e)]
      in
      List.concat @@ List.map pes (fun (p,e) -> dup (p,e))
  
  let name : Typedtree.expression -> string =
    fun e -> match e.Typedtree.exp_desc with
    | Texp_ident _ -> "Identifier"
    | Texp_constant _ -> "Constant"
    | Texp_let (Asttypes.Recursive,_,_) -> "Rec binding"
    | Texp_let (Asttypes.Nonrecursive,_,_) -> "Non-rec binding"
    | Texp_function _ -> "Function"
    | Texp_apply _ -> "Application"
    | Texp_match _ -> "Match"
    | Texp_try _ -> "Try"
    | Texp_tuple _ -> "Tuple"
    | Texp_construct (_,desc,_,_) -> "Constructor: " ^ desc.cstr_name
    | Texp_variant _ -> "Polymorphic variants" 
    | Texp_record _ -> "Records"
    | Texp_field _ -> "Retrieve field"
    | Texp_setfield _ -> "Update field"
    | Texp_array _ -> "Array"
    | Texp_ifthenelse _ -> "IfThenElse"
    | Texp_sequence _ -> "Sequence"
    | Texp_while _ -> "While"
    | Texp_for _ -> "For"
    | Texp_when _ -> "When"
    | Texp_send _ -> "Send"
    | Texp_new _ -> "New"
    | Texp_instvar _ -> "Get instance variable"
    | Texp_setinstvar _ -> "Set instance variable"
    | Texp_override _ -> "Override"
    | Texp_letmodule _ -> "Let module"
    | Texp_assert _ -> "Assert"
    | Texp_assertfalse _ -> "Assert false"
    | Texp_lazy _ -> "Lazy"
    | Texp_object _ -> "Object"
    | Texp_pack _ -> "Pack"

  (* The main function that implements the pattern matching compiler. All credit goes to Simon Peyton Jones. For more details on the algorithm,
  Refer to Chapter 5 of "The Implementation of Functional Programming Languages", available here:
  source : https=//www.microsoft.com/en-us/research/wp-content/uploads/1987/01/slpj-book-1987-small.pdf  *)
  let rec m : Env.t -> Typedtree.expression list -> (Typedtree.pattern list * Typedtree.expression) list -> Typedtree.expression -> Typedtree.expression =
    fun env us pat_expr_list e_fail ->
      match us with
      | [] -> (match pat_expr_list with
              | [] -> e_fail
              | (_,e)::_ -> e)
      | u::us' ->
        if List.is_empty pat_expr_list then e_fail else
        let pes = List.map pat_expr_list (fun (ps,e) -> List.map ps (fun p -> p |> remove_constraint |> remove_alias |> remove_wild), e) in
        let firsts = List.map pes (Fn.compose List.hd_exn fst) in
        let first = List.hd_exn firsts in
        (* variable rule *)
        if List.for_all firsts (fun p -> match p.Typedtree.pat_desc with | Typedtree.Tpat_var _ -> true | _ -> false) then
          let pes' = List.map pes (fun (p::ps,e) -> (ps, subst u (get_var p) e))
            in m env us' pes' e_fail

        (* unit rule *)
        else if List.for_all firsts (fun p -> match p.Typedtree.pat_desc with | Typedtree.Tpat_construct (_, {Types.cstr_name = "()"},_,_) -> true | _ -> false) then
          let pes' = List.map pes (fun (p::ps,e) -> (ps, e)) in
            m env us' pes' e_fail

        (* constant rule *)
        (* else if (match first.Typedtree.pat_desc with Typedtree.Tpat_constant _ -> true | _ -> false) &&
         List.for_all firsts (fun p -> p = first) then
          let pes' = List.map pes (fun (p::ps,e) -> (ps, e)) in
            m env us' pes' e_fail *)

        (* bool rule *)
        else if List.for_all firsts (fun p -> match p.Typedtree.pat_desc with
          | Typedtree.Tpat_construct (_, {Types.cstr_name = "true"},_,_) -> true
          | Typedtree.Tpat_construct (_, {Types.cstr_name = "false"},_,_) -> true
          | _ -> false) then
        let e_true =
          try (List.find_exn pes ~f:(fun (p::ps,e) -> match p.Typedtree.pat_desc with | Typedtree.Tpat_construct (_, {Types.cstr_name = "true"},_,_) -> true | _ -> false)) |> snd
          with Not_found -> e_fail in
        let e_false =
          try (List.find_exn pes ~f:(fun (p::ps,e) -> match p.Typedtree.pat_desc with | Typedtree.Tpat_construct (_, {Types.cstr_name = "false"},_,_) -> true | _ -> false)) |> snd
          with Not_found -> e_fail in
        let e_if = mk_exp env (Typedtree.Texp_ifthenelse (u, e_true, Some e_false)) e_true.Typedtree.exp_type in
        e_if

        (* tuple rule *)
        else if List.for_all firsts (fun p -> match p.Typedtree.pat_desc with | Typedtree.Tpat_tuple _ -> true | _ -> false) then
          match strip_type (u.Typedtree.exp_type) with
          | {Types.desc = Types.Ttuple ts } as tuple_t ->
            let vars = List.map ts (fun t -> new_var_id_1 "t", t) in
            let ps = List.map vars (fun (u,t) -> mk_pat env (Typedtree.Tpat_var (u, Location.mknoloc "")) t) in
            let us'' = List.map vars (fun (u,t) -> mk_exp_var env (Path.Pident u) t) in
            let p'= mk_pat env (Typedtree.Tpat_tuple ps) tuple_t in
            let pes' = List.map pes (fun (pl,e) ->
              match pl with
              | [] -> [], e
              | p::ps -> begin
                match p.Typedtree.pat_desc with
                | Typedtree.Tpat_tuple ps' -> ps' @ ps, e
                end) in
            let e' = m env (us'' @ us') pes' e_fail in
            let e_desc = Typedtree.Texp_match (u, [(p',e')], Typedtree.Total) in
            mk_exp env e_desc e_fail.Typedtree.exp_type

          | _ -> raise (Eunsupported_type (u.Typedtree.exp_loc, u.Typedtree.exp_type,"expected tuple type"))


        (* constructor rule *)
        else if List.for_all firsts (fun p -> match p.Typedtree.pat_desc with | Typedtree.Tpat_construct _ -> true | _ -> false) then
          let variant_t = u.Typedtree.exp_type in
          let cstr_map = List.fold_left pes ~init:CMap.empty ~f:(fun acc (pl,e) ->
            match pl with
            | [] -> acc
            | p::ps -> (match p.Typedtree.pat_desc with
                      | Typedtree.Tpat_construct (_,cstr,ps',_) -> CMap.change acc (cstr.Types.cstr_name) (function None -> Some [(ps' @ ps, e)] | Some l -> Some (l @ [(ps' @ ps, e)]))
                      | _ -> raise (Eunsupported_pattern p))
            ) in
          let pairs = List.map (variant_cstrs env variant_t) (fun c ->
            let vars = List.map (c.Types.cstr_args) (fun t -> new_var_id_1 "v", t) in
            let ps = List.map vars (fun (u,t) -> mk_pat env (Typedtree.Tpat_var (u, Location.mknoloc "")) t) in
            let us'' = List.map vars (fun (u,t) -> mk_exp_var env (Path.Pident u) t) in
            let pat_desc'= Typedtree.Tpat_construct (Location.mknoloc (Longident.Lident ""), c, ps, false) in
            let left = mk_pat env pat_desc' variant_t in
            let right  = m env (us'' @ us') (match CMap.find cstr_map (c.Types.cstr_name) with | None -> [] | Some l -> l) e_fail in
            left, right
            ) in
          let e_desc = Typedtree.Texp_match (u, pairs, Typedtree.Total) in
          mk_exp env e_desc e_fail.Typedtree.exp_type

        (* tuple mixture rule *)
        else if List.for_all firsts (fun p -> match p.Typedtree.pat_desc with | Typedtree.Tpat_tuple _ -> true | Typedtree.Tpat_var _ -> true | _ -> false) then
          let partition = List.group pes ~break:(fun (ps1,_) (ps2,_) ->
            let p1, p2 = List.hd_exn ps1, List.hd_exn ps2 in
            match p1.Typedtree.pat_desc, p2.Typedtree.pat_desc with
            | Typedtree.Tpat_tuple _, Typedtree.Tpat_var _ -> true
            | Typedtree.Tpat_var _, Typedtree.Tpat_tuple _ -> true
            | _ -> false
            ) in
          List.fold_right partition ~f:(fun qs acc -> m env us qs acc) ~init:e_fail

        (* constructor mixture rule *)
        else if List.for_all firsts (fun p -> match p.Typedtree.pat_desc with | Typedtree.Tpat_construct _ -> true | Typedtree.Tpat_var _ -> true | _ -> false) then
          let partition = List.group pes ~break:(fun (ps1,_) (ps2,_) ->
            let p1, p2 = List.hd_exn ps1, List.hd_exn ps2 in
            match p1.Typedtree.pat_desc, p2.Typedtree.pat_desc with
            | Typedtree.Tpat_construct _, Typedtree.Tpat_var _ -> true
            | Typedtree.Tpat_var _, Typedtree.Tpat_construct _ -> true
            | _ -> false
            ) in
          List.fold_right partition ~f:(fun qs acc -> m env us qs acc) ~init:e_fail

        else
        let _ = List.map firsts (Printtyped.pattern Format.std_formatter) in
        let _ = Printf.printf "; \n"; in
        raise (Fail "no compilation rule found for the given nested pattern")

  (* function used by the simplifier to transform expressions with nested patterns to the unnest form. *)
  let rec unnest : Typedtree.expression -> Typedtree.expression =
    fun exp ->
      let env = exp.Typedtree.exp_env in
      match exp.Typedtree.exp_desc with
      | Typedtree.Texp_match (e, [], part) -> exp
      | Typedtree.Texp_match (e, (x::xs as pairs), part) ->
        let pairs' = expand_or pairs in 
        (match e.Typedtree.exp_desc with
        | Typedtree.Texp_ident (path, _, _) -> m env [e] (List.map pairs' (fun (p',e') -> ([p'], unnest e'))) (e_raise_match_fail env exp.Typedtree.exp_type)
        | _ ->
          let e' = unnest e in
          let u = new_var_id_1 "u" in
          let u_pat = mk_pat env (Typedtree.Tpat_var (u, Location.mknoloc "")) e'.Typedtree.exp_type in
          let u_exp = mk_exp_var env (Path.Pident u) e'.Typedtree.exp_type in
          let e_match' = m env [u_exp] (List.map pairs' (fun (p,e) -> ([p],unnest e))) (e_raise_match_fail env exp.Typedtree.exp_type) in
          let outer = mk_exp env (Typedtree.Texp_let (Asttypes.Nonrecursive, [(u_pat,e')], e_match')) (exp.Typedtree.exp_type) in
          outer)

      | Typedtree.Texp_ident (path,_,_) -> exp
      | Typedtree.Texp_constant c -> exp
      | Typedtree.Texp_let (rflag, pairs, e) ->
        let e' = unnest e in
        let e_match =
          match rflag with
          | Asttypes.Recursive ->
            {exp with Typedtree.exp_desc = Typedtree.Texp_let (rflag, List.map pairs (fun (p,e) -> (p, unnest e)), e')}
          | _ -> List.fold_right pairs ~f:(fun (pi,ei) acc ->
            match pi.Typedtree.pat_desc with
            | Typedtree.Tpat_var (_,_)
            | Typedtree.Tpat_any ->
              mk_exp env (Typedtree.Texp_let (rflag, [(pi, unnest ei)], acc)) exp.Typedtree.exp_type
            | _ ->
              let e'' = mk_exp env (Typedtree.Texp_match (ei ,[(pi,acc)], Typedtree.Partial)) exp.Typedtree.exp_type in
              unnest e''
          ) ~init:e'
        in e_match

      | Typedtree.Texp_function (label, pairs, part) ->
        let p, e = List.hd_exn pairs in
        let u = new_var_id_1 "u" in
        let u_pat = mk_pat env (Typedtree.Tpat_var (u, Location.mknoloc "")) p.Typedtree.pat_type in
        let u_exp = mk_exp_var env (Path.Pident u) p.Typedtree.pat_type in
        let pairs' = expand_or pairs in
        let e' = m env [u_exp] (List.map pairs' (fun (p,e) -> ([p], unnest e))) (e_raise_match_fail env e.Typedtree.exp_type) in
        mk_exp env (Typedtree.Texp_function (label, [(u_pat, e')], part)) (exp.Typedtree.exp_type)

      | Typedtree.Texp_apply (e,es) ->
        let es' = List.map es (fun ((l,eo,o) as a) -> match eo with | Some eo' -> l, Some (unnest eo'), o | _ -> a ) in
        {exp with Typedtree.exp_desc = Typedtree.Texp_apply (unnest e, es')}

      | Typedtree.Texp_try (e, pairs)->
        let pairs' = expand_or pairs in
        let e_match = {exp with Typedtree.exp_desc = Typedtree.Texp_match (e,pairs',Typedtree.Total)} in
        let e_match' = unnest e_match in
          (match e_match'.Typedtree.exp_desc with
          | Typedtree.Texp_match (e',pairs',part') -> mk_exp env (Typedtree.Texp_try (e', pairs')) (exp.Typedtree.exp_type)
          | _ -> raise (Fail "failed to compile try expression"))

      | Typedtree.Texp_tuple es -> {exp with Typedtree.exp_desc = Typedtree.Texp_tuple (List.map es unnest)}
      | Typedtree.Texp_construct (loc, desc, es, b) ->
      {exp with Typedtree.exp_desc = Typedtree.Texp_construct (loc, desc, List.map es unnest, b)}

      | Typedtree.Texp_sequence (e1,e2) ->
        {exp with Typedtree.exp_desc = Typedtree.Texp_sequence (unnest e1, unnest e2)}

      | Typedtree.Texp_ifthenelse (e1,e2,e3) ->
        {exp with Typedtree.exp_desc = Typedtree.Texp_ifthenelse (unnest e1, unnest e2, Option.map e3 unnest)}

      | _ -> 
      let _ = Printf.printf "Unsupported expression: %s\n" (name exp) in 
      raise (Eunsupported_expr exp)
