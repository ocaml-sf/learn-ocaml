open Lambda
open Typedtree

let hash_string_lst x xs=
  Digest.string @@
    String.concat "" @@ Digest.string x :: xs

let expr_list_of_bindings =
  List.map (fun x -> x.vb_expr)

let lambdas_of_bindings env recflag valbindl =
  let valbindls,_ = Typecore.type_binding env recflag valbindl None in
  let get_lambda x = Simplif.simplify_lambda "" @@ Translcore.transl_exp x in
  List.map get_lambda @@ expr_list_of_bindings valbindls

let rec hash_lambda =
  let hash_map_snd xs = List.map (fun (_,x) -> hash_lambda x) xs in
  function
  | Lvar _ -> Digest.string "var" (* TODO if free, keep ? *)
  | Lconst _ -> Digest.string "const" (* keep ? *)
  | Lapply f ->
     hash_string_lst "apply"
       (  hash_lambda f.ap_func
       :: List.map hash_lambda f.ap_args
       )
  | Lfunction f ->
     hash_string_lst "function"
       [hash_lambda f.body]
  | Llet (_,_,_,l1,l2) ->
     hash_string_lst "let"
       [ hash_lambda l1
       ; hash_lambda l2]
  | Lletrec (lst,l) ->
     hash_string_lst "letrec" @@
       hash_lambda l :: hash_map_snd lst
  | Lprim (_,lst,_) ->
     hash_string_lst "prim" @@ List.map hash_lambda lst
  | Lswitch (l,swi) ->
     let hashedswi =
       hash_string_lst "" @@
         hash_map_snd swi.sw_consts @
           hash_map_snd swi.sw_blocks
     in
     hash_string_lst "letrec" @@
       [hash_lambda l
       ; hashedswi]
  | Lifthenelse (l1,l2,l3) ->
     hash_string_lst "ifthenelse"
       [ hash_lambda l1
       ; hash_lambda l2
       ; hash_lambda l3]
  | _ -> Digest.string "todo"
(*  | Lfunction of lfunction
  | Llet of let_kind * value_kind * Ident.t * lambda * lambda
  | Lletrec of (Ident.t * lambda) list * lambda
  | Lprim of primitive * lambda list * Location.t
  | Lswitch of lambda * lambda_switch * Location.t
(* switch on strings, clauses are sorted by string order,
   strings are pairwise distinct *)
  | Lstringswitch of
      lambda * (string * lambda) list * lambda option * Location.t
  | Lstaticraise of int * lambda list
  | Lstaticcatch of lambda * (int * (Ident.t * value_kind) list) * lambda
  | Ltrywith of lambda * Ident.t * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda
  | Lassign of Ident.t * lambda
  | Lsend of meth_kind * lambda * lambda * lambda list * Location.t
  | Levent of lambda * lambda_event
  | Lifused of Ident.t * lambda *)

let hash_of_bindings (recflag,valbindl) =
  Compmisc.init_path true;
  let init_env = Compmisc.initial_env () in
  try
    hash_string_lst "" @@
      List.map 
        hash_lambda @@
        lambdas_of_bindings init_env recflag valbindl
  with
  | _ -> Digest.string "fail"
  
