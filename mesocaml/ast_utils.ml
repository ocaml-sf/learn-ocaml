open Parsetree
open Asttypes

(* TODO hash node *)

let hash_string_lst x xs=
  Digest.string @@
    String.concat "" (Digest.string x::xs)

(* let hash_lst accr f self x xs =
  let res =
    List.fold_left
      (fun acc x -> f self x; !accr :: acc)
      [] xs in
  hash_string_lst accr x (List.sort compare res) *)

let hash_lst f x xs =
  let res =
    List.fold_left
      (fun acc x -> f x :: acc)
      [] xs in
  hash_string_lst x (List.sort compare res)

let hash_lst_anon f xs = hash_lst f "" xs

let hash_option f =
  function
  | None -> Digest.string "None"
  | Some x -> f x

let hash_rec_flag = function
  | Recursive -> Digest.string "Recursive"
  | Nonrecursive -> Digest.string "Nonrecursive"

let hash_arg_label _ = Digest.string "arg_label" (* Important ? *)

(* TODO attributes *)
let rec hash_value_binding x =
  hash_string_lst "value_binding"
    [ hash_pattern x.pvb_pat
    ; hash_expression x.pvb_expr
    ]
and hash_pattern_desc = function
  | Ppat_any -> Digest.string "any"
  | Ppat_var _ -> Digest.string "var"
  | Ppat_constant _ -> Digest.string "constant"
  | Ppat_interval _ -> Digest.string "interval"
  | Ppat_alias (p,_) ->
     hash_string_lst "alias"
       [ hash_pattern p ]
  | Ppat_array xs ->
     hash_string_lst "array" @@
       (List.map  hash_pattern xs)
  | Ppat_or (u,v) ->
     hash_string_lst "or"
       [ hash_pattern u
       ; hash_pattern v]
  | _ -> failwith "hash_pattern_desc"
and hash_pattern x = hash_pattern_desc x.ppat_desc
and hash_expression x = hash_expression_desc x.pexp_desc
and hash_expression_desc = function
  | Pexp_ident    _ -> Digest.string "ident"
  | Pexp_constant _ -> Digest.string "const"
  | Pexp_let (r,valbindl,e) ->
     hash_string_lst "let"
       [ hash_rec_flag r
       ; hash_lst_anon hash_value_binding valbindl
       ; hash_expression e
       ]
  | Pexp_function xs ->
     hash_string_lst
       "function"
       [hash_cases xs]
  | Pexp_fun (arg,oe,p,e) ->
     hash_string_lst "fun"
       [ hash_arg_label arg
       ; hash_option (hash_expression) oe
       ; hash_pattern p
       ; hash_expression e
       ]
  | Pexp_apply (e,xs) ->
     hash_string_lst "apply" @@
       hash_expression e ::
         (List.map  (fun (_,x) -> hash_expression x) xs)
  | Pexp_match (e,cl) ->
     hash_string_lst "match"
       [ hash_expression e
       ; hash_cases cl
       ]
  | Pexp_try (e,cl) ->
     hash_string_lst "try"
       [ hash_expression e
       ; hash_cases cl
       ]
  | Pexp_tuple xs ->
     hash_lst hash_expression "tuple" xs
  | Pexp_ifthenelse (i,f,e) ->
     hash_string_lst "ifthenelse"
       [ hash_expression i
       ; hash_expression f
       ; hash_option (hash_expression) e
       ]
  | _ -> failwith "hash_expression_desc"
and hash_cases xs = hash_lst hash_case "cases" xs
and hash_case pc =
  hash_string_lst "case"
    [ hash_pattern pc.pc_lhs
    ; hash_option hash_expression pc.pc_guard
    ; hash_expression pc.pc_rhs
    ]
(* and hash_structure xs = hash_lst hash_structure_item "structure" xs *)
and hash_structure_item x =
  match x.pstr_desc with
  | Pstr_value (p,r) ->
     hash_string_lst "value" @@
       [ hash_rec_flag p
       ; hash_lst hash_value_binding "value_binding" r
       ]
  | _ -> failwith "hash_structure_item"

let hash_of_bindings (r,v) =
  hash_structure_item {pstr_desc=(Pstr_value (r,v)); pstr_loc=Location.none}
