open Learnocaml_data
open Learnocaml_data.Partition

open Utils

let string_of_tree printer =
  let rec aux i xs =
    let first = String.make i ' ' in
    match xs with
    | Node (p,u,v) ->
       first ^ "Node " ^ string_of_float p  ^ ":\n"
       ^ aux (i+1) u ^ "\n"
       ^ aux (i+1) v
    | Leaf a -> first ^ "Leaf: " ^ printer a
  in aux 0

(* Suppose that x and y are sorted *)
let rec diff x y =
  match x,y with
  | [],_ -> false,y
  | _,[] -> false,x
  | xx::xs,yy::ys ->
     if xx < yy
     then let b,ndiff = diff xs y in
          b,xx::ndiff
     else
       if xx > yy
       then let b,ndiff = diff x ys in
          b,yy::ndiff
       else let _,ndiff = diff xs ys in
            true,ndiff

let sum_of_fst = List.fold_left (fun acc (a,_) -> acc + a) 0

(* Compute the distance between two clusters,
   if there are Nodes, takes choose using f (max gives complete-linkage clustering)
*)
let dist f =
  let rec aux x y =
    match x,y with
    | Leaf (x,_), Leaf (y,_) ->
       let b,diff = diff x y in
       if b
       then Some (float_of_int  @@ sum_of_fst diff)
       else None
    | Node (_,u,v), Node (_,u',v') ->
       f
         (f (aux u u') (aux u v'))
         (f (aux v u') (aux v v'))
    | Node (_,u,v), l | l, Node (_,u,v) ->
       f (aux u l) (aux v l)
  in aux

(* NB: None is the biggest number *)

type 'a compare_option = True of 'a | False

let compare_option x y =
  match x with
  | None -> False
  | Some x' ->
     match y with
     | None -> True x'
     | Some y' ->
        if x' < y'
        then True x' else False

let max_option x y =
  match compare_option x y with
  | True _ -> y
  | False ->  x

(* O(n^2) algorithm to get the two closeset elements *)
let get_min_dist xs =
  let min = ref None in
  List.iter
    (fun x ->
      List.iter (fun y ->
          if x != y
          then
            let d = dist max_option x y in
            match compare_option d (fmapOption fst !min) with
            | True d -> min := Some (d,(x,y))
            | False -> ();
        )
     xs
    )
    xs;
  !min

(* Merge two elements of a cluster *)
let merge p u v xs =
  let xs = List.filter (fun x -> x != u && x != v) xs in
  (Node (p,u,v))::xs

let add_in_eq x xs =
  let rec go = function
    | [] -> [(xs,[x])]
    | ((us,ys) as e)::zs ->
       if us = xs
       then (us,x::ys)::zs
       else e::go zs
  in go

let rec remove_hash_in_tree = function
  | Leaf (_,x) -> Leaf x
  | Node (p,u,v) -> Node (p,remove_hash_in_tree u, remove_hash_in_tree v)

(* Compute a hierarchical cluster from data *)
let cluster (m : (Token.t, (int * string) list) Hashtbl.t) =
  let rec aux res = function
    | [] -> failwith "cluster, empty list"
    | [a] -> a::res
    | x::xs as lst ->
       match get_min_dist lst with
       | None -> aux (x::res) xs
       | Some (p, (u,v)) -> aux res (merge p u v lst)
  in
  let start =
    List.map (fun x -> Leaf x) @@
      Hashtbl.fold (fun x xs acc -> add_in_eq x (List.sort compare xs) acc) m []
  in
  List.sort
    (fun x y -> - compare (weight_of_tree List.length x) (weight_of_tree List.length y)) @@
    List.map remove_hash_in_tree @@
      aux [] start

let rec flatten = function
  | Leaf x -> [x]
  | Node (_,u,v) -> List.rev_append (flatten u) (flatten v)

(* Compute a hierarchical cluster from data *)
(* Flatten the obtained trees *)
let cluster_flatten m = List.map flatten (cluster m)
