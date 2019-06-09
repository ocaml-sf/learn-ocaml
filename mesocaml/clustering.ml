open Learnocaml_data

(* To represent a hierachical cluster *)
type 'a tree =
  | Node of ('a tree * 'a tree)
  | Leaf of 'a

let string_of_tree printer =
  let rec aux = function
    | Node (u,v) ->
       "Node (" ^ aux u  ^ ") (" ^ aux v ^ ")"
    | Leaf a -> printer a
  in aux

(* Suppose that x and y are sorted *)
let rec intersect x y =
  match x,y with
  | [],_ -> []
  | _,[] -> []
  | xx::xs,yy::ys ->
     if xx < yy
     then intersect xs y
     else
       if xx > yy
       then intersect x ys
       else xx::intersect xs ys

let sum_of_fst = List.fold_left (fun acc (a,_) -> acc + a) 0

(* NB: None is the biggest number *)

let compare_option x y =
  match x with
  | None -> false
  | Some x' ->
     match y with
     | None -> true
     | Some y' -> x' < y'

let max_option x y =
  if compare_option x y
  then y
  else x

(* Compute the distance between two clusters,
   if there are Nodes, takes choose using f (max gives complete-linkage clustering)
*)
let dist f =
  let rec aux x y =
    match x,y with
    | Leaf (x,_), Leaf (y,_) ->
       begin
         match intersect x y with
         | [] -> None
         | xs -> Some (1. /. (float_of_int (sum_of_fst xs)))
       end
    | Node (u,v), Node (u',v') ->
       f
         (f (aux u u') (aux u v'))
         (f (aux v u') (aux v v'))
    | Node (u,v), l | l, Node (u,v) ->
       f (aux u l) (aux v l)
  in aux

(* O(n^2) algorithm to get the two closeset elements *)
let get_min_dist xs =
  let min = ref (None, None) in
  List.iter
    (fun x ->
      List.iter (fun y ->
          if x != y
          then
            let d = dist max_option x y in
            if compare_option d (fst !min)
            then min := (d,Some (x,y))
            else ();
        )
     xs
    )
    xs;
  !min

(* Merge two elements of a cluster *)
let merge u v xs =
  let xs = List.filter (fun x -> x != u && x != v) xs in
  (Node (u,v))::xs

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
  | Node (u,v) -> Node (remove_hash_in_tree u, remove_hash_in_tree v)

(* Compute a hierarchical cluster from data *)
let cluster (m : (Token.t, (int * string) list) Hashtbl.t) =
  let rec aux res = function
    | [] -> failwith "cluster, empty list"
    | [a] -> a::res
    | x::xs as lst ->
       match get_min_dist lst with
       | (_,None) -> aux (x::res) xs
       | (_, Some (u,v)) -> aux res (merge u v lst)
  in
  let start =
    List.map (fun x -> Leaf x) @@
      Hashtbl.fold (fun x xs acc -> add_in_eq x (List.sort compare xs) acc) m []
  in List.map remove_hash_in_tree (aux [] start)

let rec flatten = function
  | Leaf x -> [x]
  | Node (u,v) -> List.rev_append (flatten u) (flatten v)

(* Compute a hierarchical cluster from data *)
(* Flatten the obtained trees *)
let cluster_flatten m = List.map flatten (cluster m)
