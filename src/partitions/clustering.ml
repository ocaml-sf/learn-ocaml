open Learnocaml_data
open Learnocaml_data.Partition

(* Return the symmetric difference of two _sorted_ lists *)
(* Return also if the intersection was empty *)
let rec symmetric_difference x y =
  match x,y with
  | [],z|z,[] -> false,z
  | xx::xs,yy::ys ->
     if xx < yy
     then let b,ndiff = symmetric_difference xs y in
          b,xx::ndiff
     else
       if xx > yy
       then let b,ndiff = symmetric_difference x ys in
          b,yy::ndiff
       else let _,ndiff = symmetric_difference xs ys in
            true,ndiff

let sum_of_fst = List.fold_left (fun acc (a,_) -> acc + a) 0

(* NB: None is the biggest number *)

(* Compute the distance between two clusters,
   if there are Nodes, choose using f (max gives complete-linkage clustering) *)
let dist f =
  let rec aux x y =
    match x,y with
    | Leaf (x,_), Leaf (y,_) ->
       let b,diff = symmetric_difference x y in
       if b
       then Some (float_of_int @@ sum_of_fst diff)
       else None
    | Node (_,u,v), l | l, Node (_,u,v) ->
       f (aux u l) (aux v l)
  in aux

let compare_option x y =
  match x with
  | None -> None
  | Some x' ->
     match y with
     | None -> Some x'
     | Some y' ->
        if x' < y'
        then Some x' else None

let max_option x y =
  match compare_option x y with
  | Some _ -> y
  | None ->  x

(* O(n^2) algorithm to get the two closeset elements *)
let get_min_dist xs =
  let fmapfst = function
    | None -> None
    | Some (x,_) -> Some x in
  let min = ref None in
  List.iter
    (fun x ->
      List.iter (fun y ->
          if x != y
          then
            let d = dist max_option x y in
            match compare_option d (fmapfst !min) with
            | Some d -> min := Some (d,(x,y))
            | None -> ();
        )
     xs
    )
    xs;
  !min

(* Merge two elements of a cluster *)
let merge p u v xs =
  let xs = List.filter (fun x -> x != u && x != v) xs in
  (Node (p,u,v))::xs

(* Add x in his cluster, identified by his hash list xs *)
let add_in_cluster x xs =
  let rec go = function
    | [] -> [(xs,[x])]
    | ((us,ys) as e)::zs ->
       if us = xs
       then (us,x::ys)::zs
       else e::go zs
  in go

let remove_fst_in_tree =
  fold_tree
    (fun (_,x) -> Leaf x)
    (fun p u v -> Node (p, u, v))

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
      Hashtbl.fold (fun x xs -> add_in_cluster x (List.sort compare xs)) m []
  in
  List.sort
    (fun x y -> - compare (weight_of_tree List.length x) (weight_of_tree List.length y)) @@
    List.map remove_fst_in_tree @@
      aux [] start
