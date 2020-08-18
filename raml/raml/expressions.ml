(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   expressions.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   RAML expressions.
 *)


open Rtypes
open Core


exception Emalformed of string

type expression_kind =
  | Efree
  | Enormal


type var_id = string [@@deriving sexp]

type call_name = string option

type 'a var_rbind = var_id * 'a rtype [@@deriving sexp]
type var_bind = unit var_rbind [@@deriving sexp]

type builtin_op =
  | Un_not
  | Un_iminus
  | Un_fminus
  | Bin_iadd
  | Bin_isub
  | Bin_imult
  | Bin_imod
  | Bin_idiv
  | Bin_fadd
  | Bin_fsub
  | Bin_fmult
  | Bin_fdiv
  | Bin_and
  | Bin_or
  | Bin_eq
  | Bin_iless_eq
  | Bin_igreater_eq
  | Bin_iless
  | Bin_igreater
  | Bin_fless_eq
  | Bin_fgreater_eq
  | Bin_fless
  | Bin_fgreater

let is_binop op =
  match op with
    | Un_not
    | Un_iminus
    | Un_fminus -> false
    | _ -> true

let  string_of_builtin_op op =
  match op with
    | Un_not -> "not"
    | Un_iminus -> "-"
    | Un_fminus -> "-."
    | Bin_iadd -> "+"
    | Bin_isub -> "-"
    | Bin_imult -> "*"
    | Bin_imod -> "%"
    | Bin_idiv -> "/"
    | Bin_fadd -> "+."
    | Bin_fsub -> "-."
    | Bin_fmult -> "*."
    | Bin_fdiv -> "/."
    | Bin_and -> "&&"
    | Bin_or -> "||"
    | Bin_eq -> "="
    | Bin_iless_eq -> "<="
    | Bin_igreater_eq -> ">="
    | Bin_iless -> "<"
    | Bin_igreater -> ">"
    | Bin_fless_eq -> "<=."
    | Bin_fgreater_eq -> ">=."
    | Bin_fless -> "<."
    | Bin_fgreater -> ">."


type builtin_fun =
  (* natural numbers *)
  | Nat_succ
  | Nat_to_int
  | Nat_of_int
  | Nat_of_intc of int
  | Nat_add
  | Nat_mult
  | Nat_minus
  | Nat_minusc of int
  | Nat_div_mod

  (* arrays *)
  | Arr_make
  | Arr_set
  | Arr_get
  | Arr_length

  (* references *)
  | Ref_swap

  (* resource managemt *)
  | Res_consume of int

let string_of_builtin_fun f =

  let nat_module str =
    Rconfig.ocaml_nat_module ^ "." ^ str
  in


  let arr_module str =
    Rconfig.ocaml_array_module ^ "." ^ str
  in

  let raml_module str =
    Rconfig.ocaml_raml_module ^ "." ^ str
  in
  

  match f with
    | Nat_succ -> nat_module "Succ"
    | Nat_to_int -> nat_module "to_int"
    | Nat_of_int -> nat_module "of_int"
    | Nat_of_intc n -> nat_module ("of_intc " ^ string_of_int(n))
    | Nat_add -> nat_module "add"
    | Nat_mult -> nat_module "mult"
    | Nat_minus -> nat_module "minus"
    | Nat_minusc n -> nat_module ("minus_" ^ string_of_int(n))
    | Nat_div_mod -> nat_module "div_mod"
    | Arr_make -> arr_module "make"
    | Arr_set -> arr_module "set"
    | Arr_get -> arr_module "get"
    | Arr_length -> arr_module "length"
    | Ref_swap -> raml_module "swap"
    | Res_consume _ -> raml_module "consume"                    


type constant =
  | Cint of int
  | Cfloat of float
  | Cbool of bool
  | Czero  (* of Nat *)
  | Cunit

let string_of_constant c =
  match c with
    | Cint n -> string_of_int n
    | Cfloat q -> Float.to_string q
    | Cbool b -> string_of_bool b
    | Czero -> Rconfig.ocaml_nat_module ^ ".Zero"
    | Cunit -> "()"





type ('a, 'b) expression =
    { exp_desc : ('a, 'b) expression_desc
    ; exp_type : 'b rtype
    ; exp_kind : expression_kind  
    ; exp_info : 'a
    }


and ('a, 'b) expression_desc =
  (* constants / built-in functions *) 
  (* there are no constants of type nat anymore *)  
  | Ebase_const of constant
  | Ebase_fun of builtin_fun
  | Ebase_op of builtin_op

  (* variables *)    
  | Evar of var_id

  (* functions *)
  | Eapp of call_name * ('a, 'b) expression * (('a, 'b) expression) list
  | Elambda of 'b var_rbind * ('a, 'b) expression

  (* let bindings / control flow / sharing *)
  | Elet of 'b var_rbind option * ('a, 'b) expression * ('a, 'b) expression
  | Eletrec of 'b var_rbind list * ('a, 'b) expression list * ('a, 'b) expression
  | Econd of ('a, 'b) expression * ('a, 'b) expression * ('a, 'b) expression
  | Eshare of ('a, 'b) expression * 'b var_rbind * 'b var_rbind * ('a, 'b) expression

  (* user defined data types *)
  | Econst of constr_id * ('a, 'b) expression
  | Ematch of ('a, 'b) expression * (constr_id * 'b var_rbind list * ('a, 'b) expression) list

  (* matching for natural numbers *)    
  | Enat_match of ('a, 'b) expression * ('a, 'b) expression * 'b var_rbind * ('a, 'b) expression

  (* references *)
  | Eref of ('a, 'b) expression
  | Eref_deref of ('a, 'b) expression
  | Eref_assign of ('a, 'b) expression * ('a, 'b) expression

  (* tuples *)
  | Etuple of ('a, 'b) expression list
  | Etuple_match of ('a, 'b) expression * 'b var_rbind list * ('a, 'b) expression

  (* raml specific *)
  | Eundefined
  | Etick of float


let get_type : ('a, 'b) expression -> 'b rtype =
  fun e -> e.exp_type

let set_type: ('a, 'b) expression -> 'b rtype -> ('a, 'b) expression =
  fun e t -> {e with exp_type = t}

let get_kind : ('a, 'b) expression -> expression_kind =
  fun e -> e.exp_kind

let set_kind: ('a, 'b) expression -> expression_kind -> ('a, 'b) expression =
  fun e k -> {e with exp_kind = k}

let get_info : ('a, 'b) expression -> 'a =
  fun e -> e.exp_info

let set_info: ('a, 'b) expression -> 'a -> ('a, 'b) expression =
  fun e b -> {e with exp_info = b}


type typed = Location.t

type sln = Sln

type sln_expression = (sln, unit) expression

type typed_expression = (typed, unit) expression


let is_lambda exp =
  match exp.exp_desc with
    | Elambda _ -> true
    | _ -> false

let substitute 
    : ('a, 'b) expression -> var_id ->
      ('b rtype -> expression_kind -> 'a -> ('a, 'b) expression) ->
      ('a, 'b) expression =
  fun exp var update ->

    let rec subst exp =

      match exp.exp_desc with
	| Evar x -> 
	  if x = var then 
	    update (get_type exp) (get_kind exp) (get_info exp)
	  else
	    exp
              
        | desc -> {exp with exp_desc =
	    match desc with
	      | Ebase_const _ -> desc
              | Ebase_fun _ -> desc
              | Ebase_op _ -> desc

              | Evar _ -> raise (Emalformed "This is dead code.")

 	      | Eapp (name,e,es) -> Eapp (name, subst e, List.map es subst)
	      | Elambda (x,e) -> Elambda (x,sub_bound [x] e)

	      | Elet (x_opt,e1,e2) ->
                let xs = Option.to_list x_opt in
		Elet (x_opt,subst e1, sub_bound xs e2)

	      | Eletrec (xs,es,e) ->
                let e' = sub_bound xs e in
		let es' = List.map es (sub_bound xs) in
                Eletrec (xs,es', e')

	      | Econd (e,e1,e2) -> Econd (subst e, subst e1, subst e2)
	      | Eshare (e1,x1,x2,e2) -> Eshare (subst e1,x1,x2, sub_bound [x1;x2] e2)

	      | Econst (c,e) -> Econst (c, subst e)
	      | Ematch (e,matches) ->
		let matches' = 
		  List.map matches (fun (c,xs,e) -> (c, xs, sub_bound xs e))
		in
		Ematch (subst e, matches')

	      | Enat_match (e,e1,x,e2) -> Enat_match (subst e, subst e1,x, sub_bound [x] e2)

	      | Eref e -> Eref (subst e)
	      | Eref_deref e -> Eref_deref (subst e)
	      | Eref_assign (e1,e2) -> Eref_assign (subst e1,subst e2)

	      | Etuple es -> Etuple (List.map es subst)
	      | Etuple_match (e1,xs,e2) -> Etuple_match (subst e1, xs, sub_bound xs e2)

	      | Eundefined -> desc
	      | Etick n -> desc
		  }

    and	sub_bound xs e =
      match List.find xs (fun (x,t) -> x=var) with
	| Some _ -> e
	| None -> subst e
    in
    subst exp


(* substituting variables *)

let subst_var 
    : ('a, 'b) expression -> var_id -> var_id -> ('a, 'b) expression =
  (* exp[x1 <- x2] *)
  fun exp x1 x2 ->
    let update t kind a  = { exp_desc = Evar x2
			   ; exp_type = t
			   ; exp_kind = kind
			   ; exp_info = a
                           } in
    substitute exp x1 update


(* apply a function to each subexpression *)

let apply_to_subexps
    : 'a 'b.
      (('a, 'c) expression -> ('b, 'c) expression) ->
      ('a, 'c) expression_desc -> ('b, 'c) expression_desc =
  fun f desc ->
    match desc with
      | Ebase_const c -> Ebase_const c
      | Ebase_fun base_fun -> Ebase_fun base_fun
      | Ebase_op op -> Ebase_op op

      | Evar v -> Evar v

      | Eapp (cname,e,es) -> Eapp (cname, f e, List.map es f)
      | Elambda (x,e) -> Elambda (x, f e)

      | Elet (x_opt,e1,e2) -> Elet (x_opt, f e1, f e2)
      | Eletrec (xs, es, e) -> Eletrec (xs, List.map es f, f e)
      | Econd (e,e1,e2) -> Econd (f e, f e1, f e2)
      | Eshare (e1,x1,x2,e2) -> Eshare (f e1, x1, x2, f e2)

      | Econst (c,e) -> Econst (c, f e)
      | Ematch (e,matches) -> 
	let matches' = List.map matches (fun (c,xs,e) -> (c, xs, f e)) in
	Ematch (f e, matches')

      | Enat_match (e,e1,x,e2) -> Enat_match (f e, f e1, x, f e2)

      | Eref e -> Eref (f e)
      | Eref_deref e -> Eref_deref (f e)
      | Eref_assign (e1,e2) -> Eref_assign (f e1,f e2)

      | Etuple es -> Etuple (List.map es f)
      | Etuple_match (e1,xs,e2) -> Etuple_match (f e1, xs, f e2)

      | Eundefined -> Eundefined
      | Etick n -> Etick n


(* map for expressions *)

let rec e_map
    : 'a 'b.
      ('c rtype -> expression_kind -> 'a -> 'c rtype * expression_kind * 'b) ->
      ('a, 'c) expression -> ('b, 'c) expression =
  fun f exp ->
    let e_desc = apply_to_subexps (e_map f) exp.exp_desc in
    let (e_type,e_kind,e_info) = f exp.exp_type exp.exp_kind exp.exp_info in

    { exp_desc = e_desc
    ; exp_type = e_type
    ; exp_kind = e_kind
    ; exp_info = e_info
    }

let e_map_type
    : 'b 'c. ('b rtype -> 'c rtype) -> ('a, 'b) expression -> ('a, 'c) expression =
  fun f ->
    let map_var_bind (v, t) = (v, f t) in
    let rec map_rec exp =
      { exp_type = f exp.exp_type
      ; exp_kind = exp.exp_kind
      ; exp_info = exp.exp_info
      ; exp_desc = match exp.exp_desc with
      | Ebase_const c -> Ebase_const c
      | Ebase_fun f -> Ebase_fun f
      | Ebase_op o -> Ebase_op o
      | Evar v -> Evar v
      | Eapp (cname, e, es) -> Eapp (cname, map_rec e, List.map es map_rec)
      | Elambda (x, e) -> Elambda (map_var_bind x, map_rec e)
      | Elet (x_opt, e1, e2) -> Elet (Option.map x_opt map_var_bind,
                                      map_rec e1, map_rec e2)
      | Eletrec (xs, es, e) -> Eletrec (List.map xs map_var_bind,
                                        List.map es map_rec, map_rec e)
      | Econd (e, e1, e2) -> Econd (map_rec e, map_rec e1, map_rec e2)
      | Eshare (e1, x1, x2, e2) ->
        Eshare (map_rec e1, map_var_bind x1, map_var_bind x2, map_rec e2)

      | Econst (c, e) -> Econst (c, map_rec e)
      | Ematch (e, matches) ->
	let matches' =
          List.map matches (fun (c, xs, e) ->
                                (c, List.map xs map_var_bind, map_rec e)) in
        Ematch (map_rec e, matches')

      | Enat_match (e, e1, x, e2) ->
        Enat_match (map_rec e, map_rec e1, map_var_bind x, map_rec e2)

      | Eref e -> Eref (map_rec e)
      | Eref_deref e -> Eref_deref (map_rec e)
      | Eref_assign (e1, e2) -> Eref_assign (map_rec e1, map_rec e2)

      | Etuple es -> Etuple (List.map es map_rec)
      | Etuple_match (e1, xs, e2) ->
        Etuple_match (map_rec e1, List.map xs map_var_bind, map_rec e2)
      | Eundefined -> Eundefined
      | Etick n -> Etick n
      }
    in map_rec

(* apply a transformer to each subexpression, starting at the leaves *)

let rec e_transform
    : (('a, 'b) expression -> ('a, 'b) expression) ->
      ('a, 'b) expression -> ('a, 'b) expression =
  fun f exp ->
    let e_desc = apply_to_subexps (e_transform f) exp.exp_desc in
    f {exp with exp_desc = e_desc}


(* apply a transformer to each subexpression, starting at the root *)
let rec e_transform_outside_in
    : (('a, 'b) expression -> ('a, 'b) expression) ->
      ('a, 'b) expression -> ('a, 'b) expression =
  fun f exp ->
    let exp' = f exp in
    let e_desc = apply_to_subexps (e_transform_outside_in f) exp'.exp_desc in
    {exp' with exp_desc = e_desc}

(* free variables of an expression *)


module T = struct
  type t = var_bind [@@deriving sexp]
  
  let compare (x,t) (y,u) =
    let c = compare x y in c
  (* sanity check conflicts with analysis.ml in one place *) 
(*    if c <> 0 then
      c
    else if t = u then
      c
    else
      raise (Emalformed "A free variable has different types in the same expression.") *)
end

module Vb_set = Set.Make(T)


let free_vars
    : ('a, 'b) expression -> Vb_set.t =

  let empty_set = Vb_set.empty in
  let union_list = Vb_set.union_list in
  let remove_list s xs = List.fold xs ~init:s ~f:Set.remove in

  let rec fvars exp =
    match exp.exp_desc with
      | Ebase_const _ -> empty_set
      | Ebase_fun _ -> empty_set
      | Ebase_op _ -> empty_set

      | Evar v -> Vb_set.singleton (v,exp.exp_type)

      | Eapp (cname,e,es) -> 
	let fvs = Vb_set.union_list (List.map es fvars) in
	Set.union (fvars e) fvs

      | Elambda (x,e) -> Set.remove (fvars e) x

      | Elet (x_opt,e1,e2) -> 
	let fv1 = fvars e1 in
        let fv2 = match x_opt with
	  	    | None -> fvars e2
		    | Some x -> Set.remove (fvars e2) x
	in
	Set.union fv1 fv2

      | Eletrec (xs, es, e) -> 
	let fv_es = union_list (List.map es fvars) in
	let fvs = Set.union (fvars e) fv_es in
        remove_list fvs xs

      | Econd (e,e1,e2) -> union_list [fvars e; fvars e1; fvars e2]

      | Eshare (e1,x1,x2,e2) -> 
	let fv2 = remove_list (fvars e2) [x1;x2] in
        Set.union (fvars e1) fv2

      | Econst (c,e) -> fvars e

      | Ematch (e,matches) -> 
	let fvs = fvars e in
	let mvars = List.map matches 
	  (fun (c,xs,e) -> remove_list (fvars e) xs )
	in
        union_list (fvs::mvars)

      | Enat_match (e,e1,x,e2) ->
	let fvs = Set.union (fvars e) (fvars e1) in
	let fv = Set.remove (fvars e2) x in
        Set.union fvs fv
	
      | Eref e -> fvars e
      | Eref_deref e -> fvars e
      | Eref_assign (e1,e2) -> Set.union (fvars e1) (fvars e2)

      | Etuple es -> union_list (List.map es fvars)

      | Etuple_match (e1,xs,e2) ->
	let fv2 = remove_list (fvars e2) xs in
	Set.union (fvars e1) fv2

      | Eundefined -> empty_set
      | Etick n -> empty_set
  in

  fvars


