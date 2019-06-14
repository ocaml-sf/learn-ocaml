open Lambda

let h1 x = 1,[],x

let hash_string_lst x xs =
  let p,lst, xs =
    List.fold_right
      (fun (u,l,x) (v,l',xs) -> u+v,(u,x)::l@l',x::xs)
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

let hash_case g f (i,x) =
  let a,b,c = f x in
  a+1,b,Digest.string (g i ^ c)

let hash_option f =
  function
  | None -> h1 @@ Digest.string "None"
  | Some x -> f x

let rec hash_lambda = function
  | Lvar _ -> h1 "Lvar"
  | Lconst _ -> h1 "Lconst"
  | Lapply x ->
     hash_string_lst "Lapply"
       [ hash_lambda x.ap_func
       ; hash_lst_anon hash_lambda x.ap_args
       ]
  | Lfunction x ->
     hash_string_lst "Lfunction"
       [ hash_lambda x.body ] (* TODO *)
  | Llet (_,_,_,l,r) -> (* TODO *)
     hash_string_lst "Llet"
       [ hash_lambda l
       ; hash_lambda r]
  | Lletrec (lst,l) ->
     hash_string_lst "Lletrec"
       [ hash_lst_anon (fun (_,x) -> hash_lambda x) lst
       ; hash_lambda l]
  | Lprim (_,lst,_) ->
     hash_string_lst "Lprim"
       [ hash_lst_anon hash_lambda lst
       ]
  | Lstaticraise (_,lst) ->
     hash_string_lst "Lstaticraise"
       [ hash_lst_anon hash_lambda lst
       ]
  | Lifthenelse (i,f,e) ->
     hash_string_lst "Lifthenelse"
       [ hash_lambda i
       ; hash_lambda f
       ; hash_lambda e
       ]
  | Lsequence (l,r) ->
     hash_string_lst "Lsequence"
       [ hash_lambda l
       ; hash_lambda r
       ]
  | Lwhile (l,r) ->
     hash_string_lst "Lwhile"
       [ hash_lambda l
       ; hash_lambda r
       ]
  | Lifused (_,l) ->
     hash_string_lst "Lifused"
       [ hash_lambda l
       ]
  | Lswitch (l,s) ->
     hash_string_lst "Lswitch"
       [ hash_lambda l
       ; hash_lst (hash_case string_of_int hash_lambda) "sw_consts" s.sw_consts
       ; hash_lst (hash_case string_of_int hash_lambda) "sw_blocks" s.sw_blocks
       ; hash_option hash_lambda s.sw_failaction
       ]
  | Lstringswitch (l,lst,opt,_) ->
     hash_string_lst "Lstringswitch"
       [ hash_lambda l
       ; hash_lst (hash_case (fun x -> x) hash_lambda) "sw_consts" lst
       ; hash_option hash_lambda opt
       ]
  | Lassign (_,l) ->
     hash_string_lst "Lassign"
       [ hash_lambda l
       ]
  | Levent (l,_) -> (* TODO *)
     hash_string_lst "Levent"
       [ hash_lambda l
       ]
  | Lstaticcatch (l,_,r) -> (* TODO *)
     hash_string_lst "Lstaticcatch"
       [ hash_lambda l
       ; hash_lambda r
       ]
  | Ltrywith (l,_,r) ->
     hash_string_lst "Ltrywith"
       [ hash_lambda l
       ; hash_lambda r
       ]
  (*
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda
  | Lsend of meth_kind * lambda * lambda * lambda list * Location.t *)
  | _ -> failwith "hash_lambda"

let sort_filter alpha x xs =
  let x = float_of_int x in
  List.sort compare (List.filter (fun (u,_) -> float_of_int u > alpha *. x) xs)

let hash_lambda alpha l =
  let alpha = (float_of_int alpha) /. 100. in
  let poids,ss_arbres,h = hash_lambda l
  in
  (poids,h), sort_filter alpha poids ss_arbres
