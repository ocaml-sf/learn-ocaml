(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * File:
 *   toolbox.ml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (2014)
 *
 * Description:
 *   Missing library functions.
 *)
open Core

(* generate a list that repeatedly contains the first argument *)

type ('a,'b) either =
  | Left of 'a
  | Right of 'b


let repeat
    : 'a -> int -> 'a list =
  fun a ->
    let rec rp acc n = 
      if n<=0 then
	acc
      else
	rp (a::acc) (n-1) 
    in
    rp []


let iterate
    : (int*int) -> 'a -> (int -> 'a -> 'a) -> 'a =
  fun (low,high) start f ->
    let rec iter i acc =
      if i = high then
	acc
      else
	iter (i+1) (f i acc)
    in
    iter low start
  


let sum = List.fold ~init:0 ~f:(+)


let p_map
    :  ('a -> 'b) -> ('a*'a) -> ('b*'b) =
  fun f (a1,a2) ->
    (f a1, f a2)

let map_add_list m_init ks vs =
  List.fold2_exn ks vs ~init:m_init ~f:(fun m k v -> Map.set m k v)


let unzip3 list =
  let rec loop list l1 l2 l3 =
    match list with
    | [] -> (List.rev l1, List.rev l2, List.rev l3)
    | (x, y, z) :: tl -> loop tl (x :: l1) (y :: l2) (z :: l3)
  in
  loop list [] [] []

  
let zip3_exn l1 l2 l3 = List.map3_exn l1 l2 l3 (fun a b c -> (a,b,c))

(* Suppress the labels *)
let fold ls init f = List.fold ls ~init ~f
let fold_right ls init f = List.fold_right ls ~f ~init
let fold2_exn a b init f = List.fold2_exn a b ~init ~f

(* checks if pref is a prefix of l *)
(* returns (Some suffix) if so and none otherwise. *)
let rec prefix l pref =
  match pref with
    | [] -> Some l
    | p::ps -> (
      match l with
	| [] -> None
	| x::xs -> 
	  if p = x then
	    prefix xs ps
	  else
	    None
    )


(* All ordered pairs of the list elements*)
let rec pairs l =
  match l with
    | [] -> []
    | (x::xs) -> 
      let np = List.map xs (fun y -> (x,y)) in
      np @ (pairs xs)




let binomial n k =

  let rec binom n k =
    match n,k with
      | n, 0 -> 1
      | 0, k -> 0
      | n, k -> (binom (n-1) (k-1)) * n / k
  in

  if k > n then
    0
  else if k > n - k then
    binom n (n - k)
  else
    binom n k


(*stirling numbers of the first kind*)

let rec pow n p =
  match p with
    | 1 -> n
    | _ when p <= 0 -> 1
    | _ -> n * (pow n (p-1))

module T = struct
  type t = int * int [@@deriving sexp,compare]
end

module Stir_map = Map.Make(T)


let stirling_mem = ref Stir_map.empty

let stirling n k =
  
  let mem = stirling_mem in

  let rec u_stirling n k =
    match Map.find !mem (n,k) with
      | Some res -> res
      | None -> begin
	let res = match n,k with
	  | 0, 0 -> 1
	  | 0, k -> 0
	  | n, 0 -> 0
	  | n, k -> 
	    let n' = n-1 in
	    n' * (u_stirling n' k) + (u_stirling n' (k-1))
	in
	mem := Map.set !mem (n,k) res
	; res
      end
  in

  if k>n then
    0
  else
    let sign = 
      if ((n-k) mod 2) = 0 then 1 else -1
    in
    sign * (u_stirling n k)


let rec fact n =
  if n <= 0 then 1 else n * (fact (n-1))


let enum n =
  match n with
    | 1 -> "1st"
    | 2 -> "2nd"
    | 3 -> "3rd"
    | n -> (string_of_int n)^"th"



type 'a ref_chain =
  | Rleaf of 'a
  | Rnode of 'a ref_chain ref

let rec rc_cycle address ref_chain =
  match ref_chain with
  | Rleaf _ -> false
  | Rnode ref_chain' ->
     if phys_equal address ref_chain' then
       true
     else
       rc_cycle address !ref_chain'


let rec rc_update f ref_chain =
  match !ref_chain with
  | Rleaf a ->
     let update = f a in
     if rc_cycle ref_chain update then
       ()
     else
       ref_chain := update
  | Rnode ref_chain' ->
     rc_update f ref_chain'

let rec rc_get ref_chain =
  match !ref_chain with
    | Rleaf a -> a
    | Rnode ref_chain' ->
      rc_get ref_chain'

		
let is_int s =
  try ignore (int_of_string s); true
  with _ -> false


let mult_coef : int -> int -> int -> int =
  (* Recursive computation of the coefficients A(i,j,k) in
     sum_i,j (A(i,j,k)*binom(n,i)*binom(m,j)) = binom(n*m,k)

     Following Riordan and Stein, in "Arrangements on Chessboards"
     (Journal of Combinatorial Theory, Series A, 12 72-80, 1972)
  *)

(*
  let rec multc i j k =
    match i,j,k with
    | 0,0,0 -> 1
    | 0,_,_ -> 0
    | _,0,_ -> 0
    | _,_,0 -> 0
    | _ -> 
      let a =
        i*j*
        ( (multc i j (k-1))
          + (multc (i-1) j (k-1))
          + (multc i (j-1) (k-1))
          + (multc (i-1) (j-1) (k-1))
        )
        - (k-1) * (multc i j (k-1))
      in
      a / k
  in
 *)


  let multc_fast i j k =
    let rec sum l =
      match l with
      | [] -> 0
      | (r,s)::facts ->
        (sum facts) + (pow (-1) (i+j+r+s)) * (binomial i r) * (binomial j s) * (binomial (r*s) k)
    in
    let f x (y,a) =
      (y,(x,y)::a)
    in
    let g y a =
      snd (iterate (0,i+1) (y,a) f)
    in
    sum (iterate (0,j+1) [] g)
  in

  (* Using multc_fast for now. Use test_multc to test. *)
  multc_fast

let rec tuples ns =
  match ns with
  | [] -> [[]]
  | n::ns ->
    let tups = tuples ns in
    let f i acc =
      let new_tups = List.map tups (fun t -> i::t) in
      List.append new_tups acc
    in
    iterate (0,n+1) [] f
     


let test_multc n =
  List.filter ~f:(fun ((i,j,k),a) -> a <> 0 && i+j > k)
    begin
      List.map ~f:(fun (i,j,k) -> ((i,j,k),mult_coef i j k(* , multc_fast i j k *)))
        begin
          let f j (i,k,a) =
            (i,k,(j,k,i)::a)
          in
          let g i (k,a) =
            let (_,_,a) = iterate (0,n+1) (i,k,a) f in
            (k,a)
          in
          let h k a =
            snd (iterate (0,n+1) (k,a) g)
          in
          iterate (0,n+1) [] h
        end
    end


let rec snoc l =
  match l with
  | [] -> raise (Invalid_argument "Empty list")
  | [x] -> ([],x)
  | x::xs ->
    let (ys,y) = snoc xs in
    (x::ys,y)
  
  

let apply f x = f x
