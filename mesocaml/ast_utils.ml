open Parsetree
open Asttypes

(* Pour le poids:
   - Chaque constante à poids 1
   - hash_string_lst (et donc hash_lst) ajoute 1 à la somme des poids des sous-arbres
 *)

(* Limite de poids d'ajout des sous-arbres *)
let alpha = ref 0 (* OCAML... *)

let h1 x = 1,[],x

let hash_string_lst x xs =
  let p,lst, xs =
    List.fold_right
      (fun (u,l,x) (v,l',xs) -> u+v,(if u > !alpha then (u,x)::l@l' else l'),x::xs)
      xs (0,[],[]) in
  1+p,lst,Digest.string @@
    String.concat "" (Digest.string x::xs)

let hash_lst f x xs =
  let res =
    List.fold_left
      (fun acc x -> f x :: acc)
      [] xs in
  hash_string_lst x (List.sort compare res)

let hash_lst_anon f xs = hash_lst f "" xs

let hash_option f =
  function
  | None -> h1 @@ Digest.string "None"
  | Some x -> f x

let hash_rec_flag x =
  h1 @@
    match x with
    | Recursive -> Digest.string "Recursive"
    | Nonrecursive -> Digest.string "Nonrecursive"

let hash_arg_label _ = Digest.string "arg_label" (* Important ? *)

(* TODO attributes *)
let rec hash_value_binding x =
  hash_string_lst "value_binding"
    [ hash_pattern x.pvb_pat
    ; hash_expression x.pvb_expr
    ]
and hash_pattern x =
  try hash_pattern_desc x.ppat_desc
  with
  | Exit -> Pprintast.pattern Format.std_formatter x;
            failwith "hash_pattern"
and hash_pattern_desc = function
  | Ppat_any -> h1 @@ Digest.string "any"
  | Ppat_var _ -> h1 @@ Digest.string "var"
  | Ppat_constant _ -> h1 @@ Digest.string "constant"
  | Ppat_interval _ -> h1 @@ Digest.string "interval"
  | Ppat_tuple xs ->
     hash_lst hash_pattern "tuple" xs
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
  | Ppat_construct (_,x) ->
     hash_string_lst "construct"
       [ hash_option hash_pattern x ]
  | Ppat_constraint _ -> h1 "" (* On ne s'intéresse pas aux annotations de types *)
  | _ -> raise Exit
and hash_expression x =
  try hash_expression_desc x.pexp_desc
  with
  | Exit -> Pprintast.expression Format.std_formatter x;
            failwith "hash_expression"
and hash_expression_desc = function
  | Pexp_ident    _ -> h1 @@ Digest.string "ident"
  | Pexp_constant _ -> h1 @@ Digest.string "const"
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
       [ h1 @@ hash_arg_label arg
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
  | Pexp_construct (_,e) ->
     hash_string_lst "construct"
       [ hash_option (hash_expression) e ]
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
and hash_structure xs = hash_lst hash_structure_item "structure" xs
and hash_structure_item x =
  match x.pstr_desc with
  | Pstr_value (p,r) ->
     hash_string_lst "value" @@
       [ hash_rec_flag p
       ; hash_lst hash_value_binding "value_binding" r
       ]
  | _ -> failwith "hash_structure_item"

let hash_of_bindings a (r,v) =
  alpha := a;
  let poids,ss_arbres,h = hash_structure_item {pstr_desc=(Pstr_value (r,v)); pstr_loc=Location.none}
  in
  (poids,h), List.sort (fun x y -> - (compare x y)) ss_arbres

let hash_of_structure a s =
  alpha := a;
  let poids,ss_arbres,h = hash_structure s
  in
  (poids,h), List.sort (fun x y -> - (compare x y)) ss_arbres
