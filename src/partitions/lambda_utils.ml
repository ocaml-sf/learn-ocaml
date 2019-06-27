open Lambda
open Asttypes

let h1 x = 1,[],Digest.string x

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
  | None -> h1 "None"
  | Some x -> f x

let hash_direction x = h1 @@
  match x with
  | Upto -> "Upto"
  | Downto -> "Downto"

let hash_meth_kind x = h1 @@
   match x with
   | Self -> "Self"
   | Public -> "Public"
   | Cached -> "Cached"

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
       [ hash_lambda x.body ]
  | Llet (_,_,_,l,r) ->
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
  | Levent (l,_) ->
     hash_string_lst "Levent"
       [ hash_lambda l
       ]
  | Lstaticcatch (l,_,r) ->
     hash_string_lst "Lstaticcatch"
       [ hash_lambda l
       ; hash_lambda r
       ]
  | Ltrywith (l,_,r) ->
     hash_string_lst "Ltrywith"
       [ hash_lambda l
       ; hash_lambda r
       ]
  | Lfor (_,a,b,d,c) ->
     hash_string_lst "Lfor"
       [ hash_lambda a
       ; hash_lambda b
       ; hash_direction d
       ; hash_lambda c
       ]
  | Lsend (m,a,b,xs,_) ->
     hash_string_lst "Lsend"
       [ hash_meth_kind m
       ; hash_lambda a
       ; hash_lambda b
       ; hash_lst_anon hash_lambda xs
       ]

let sort_filter alpha x xs =
  let x = float_of_int x in
  List.sort compare (List.filter (fun (u,_) -> float_of_int u > alpha *. x) xs)

let hash_lambda alpha l =
  let alpha = (float_of_int alpha) /. 100. in
  let poids,ss_arbres,h = hash_lambda l
  in
  (poids,h), sort_filter alpha poids ss_arbres

(* Replace every occurence of ident by its body *)
let replace ident body =
  let rec aux expr =
    let insnd lst = List.map (fun (e,x) -> e, aux x) lst in
    let inopt = function
    | None -> None
    | Some x -> Some (aux x) in
    match expr with
    | Lvar x ->
       if x = ident
       then body
       else expr
    | Lconst _ -> expr
    | Llet (k,e,ident,l,r) ->
       Llet (k, e, ident, aux l, aux r)
  | Lapply x ->
     let ap_func = aux x.ap_func in
     let ap_args = List.map aux x.ap_args in
     Lapply {x with ap_func; ap_args }
  | Lfunction x ->
     let body = aux x.body in
     Lfunction {x with body}
  | Lletrec (lst,l) ->
     Lletrec (insnd lst, aux l)
  | Lprim (a,lst,b) ->
     Lprim (a,List.map aux lst, b)
  | Lstaticraise (a,lst) ->
     Lstaticraise (a,List.map aux lst)
  | Lifthenelse (i,f,e) ->
     Lifthenelse (aux i, aux f, aux e)
  | Lsequence (l,r) ->
     Lsequence (aux l, aux r)
  | Lwhile (l,r) ->
     Lwhile (aux l, aux r)
  | Lifused (i,l) ->
     Lifused (i, aux l)
  | Lswitch (l,s) ->
     let sw_consts = insnd s.sw_consts in
     let sw_blocks = insnd s.sw_blocks in
     Lswitch (aux l, {s with sw_consts; sw_blocks})
  | Lstringswitch (l,lst,opt,e) ->
     Lstringswitch (aux l, insnd lst, inopt opt, e)
  | Lassign (i,l) ->
     Lassign (i, aux l)
  | Levent (l,e) ->
     Levent (aux l, e)
  | Lstaticcatch (l,lst,r) ->
     Lstaticcatch (aux l, lst, aux r)
  | Ltrywith (l,i,r) ->
     Ltrywith (aux l, i, aux r)
  | Lfor (e,a,b,d,c) ->
     Lfor (e, aux a, aux b, d, aux c)
  | Lsend (a,b,c,d,e) ->
     Lsend (a, aux b, aux c, List.map aux d, e)
  in aux

(* Is the definition inlineable ? *)
let inlineable x f =
  match x with
  | Alias -> true
  | Strict ->
     begin
       match f with
       | Lvar _ | Lconst _ -> true
       | _ -> false
     end
  | _  -> false

(* Inline all possible "let definition" (that is, "let definition" without a side effet) *)
let rec inline_all x =
  let insnd lst = List.map (fun (e,l) -> e,inline_all l) lst in
  let inopt = function
    | None -> None
    | Some x -> Some (inline_all x) in
  match x with
  | Lvar _ | Lconst _ -> x
  | Llet (k,e,ident,l,r) ->
     if inlineable k l
     then
       inline_all (replace ident l r)
     else
       Llet (k, e, ident, inline_all l, inline_all r)
  | Lapply x ->
     let ap_func = inline_all x.ap_func in
     let ap_args = List.map inline_all x.ap_args in
     Lapply {x with ap_func; ap_args }
  | Lfunction x ->
     let body = inline_all x.body in
     Lfunction {x with body}
  | Lletrec (lst,l) ->
     Lletrec (insnd lst, inline_all l)
  | Lprim (a,lst,b) ->
     Lprim (a,List.map inline_all lst, b)
  | Lstaticraise (a,lst) ->
     Lstaticraise (a,List.map inline_all lst)
  | Lifthenelse (i,f,e) ->
     Lifthenelse (inline_all i, inline_all f, inline_all e)
  | Lsequence (l,r) ->
     Lsequence (inline_all l, inline_all r)
  | Lwhile (l,r) ->
     Lwhile (inline_all l, inline_all r)
  | Lifused (i,l) ->
     Lifused (i, inline_all l)
  | Lswitch (l,s) ->
     let sw_consts = insnd s.sw_consts in
     let sw_blocks = insnd s.sw_blocks in
     Lswitch (inline_all l, {s with sw_consts; sw_blocks})
  | Lstringswitch (l,lst,opt,e) ->
     Lstringswitch (inline_all l, insnd lst, inopt opt, e)
  | Lassign (i,l) ->
     Lassign (i, inline_all l)
  | Levent (l,e) ->
     Levent (inline_all l, e)
  | Lstaticcatch (l,lst,r) ->
     Lstaticcatch (inline_all l, lst, inline_all r)
  | Ltrywith (l,i,r) ->
     Ltrywith (inline_all l, i, inline_all r)
  | Lfor (e,a,b,d,c) ->
     Lfor (e, inline_all a, inline_all b, d, inline_all c)
  | Lsend (a,b,c,d,e) ->
     Lsend (a, inline_all b, inline_all c, List.map inline_all d, e)
