open Learnocaml_data

type 'a tree =
  | Node of ('a tree * 'a tree)
  | Leaf of 'a

let string_of_tree printer =
  let rec aux = function
    | Node (u,v) ->
       "Node (" ^ aux u  ^ ") (" ^ aux v ^ ")"
    | Leaf a -> printer a
  in aux

let string_of_token_tree =
  string_of_tree Token.to_string

(* Suppose that x and y are sorted *)
let rec intersect x y =
  match x,y with
  | [],_ -> true,[]
  | _,[] -> true,[]
  | xx::xs,yy::ys ->
     if xx < yy
     then let _,res = intersect xs y in false, res
     else
       if xx > yy
       then let _,res = intersect x ys in false,res
       else let b,res = intersect xs ys in b,xx::res

let sum_of_fst = List.fold_left (fun acc (a,_) -> acc + a) 0

let big_value = 100.

let rec dist hm x y =
  match x,y with
  | Leaf x, Leaf y ->
     begin
       let x = Hashtbl.find hm x in
       let y = Hashtbl.find hm y in
       match intersect x y with
       | true,_ -> 0.
       | _,[] -> big_value
       | _,xs  -> 1. /. (float_of_int (sum_of_fst xs))
     end
  | Node (u,v), Node (u',v') ->
     max
       (max (dist hm u u') (dist hm u v'))
       (max (dist hm v u') (dist hm v v'))
  | Node (u,v), l | l, Node (u,v) ->
     max (dist hm u l) (dist hm v l)

let get_min_dist hm xs =
  let min = ref (big_value +. 1., None) in
  List.iter
    (fun x ->
      List.iter (fun y ->
          if x != y
          then
            let d = dist hm x y in
            if d < fst !min
            then min := (d,Some (x,y))
            else ();
        )
     xs
    )
    xs;
  match snd !min with
  | None -> failwith "get_min_dist, empty list"
  | Some x -> x

let merge u v xs =
  let xs = List.filter (fun x -> x != u && x != v) xs in
  (Node (u,v))::xs

let cluster (m : (Token.t, (int * string) list) Hashtbl.t) =
  let rec aux = function
    | [] -> failwith "cluster, empty list"
    | [a] -> a
    | xs ->
       let (u,v) = get_min_dist m xs in
       aux (merge u v xs)
  in
  let start = List.map (fun x -> Leaf x) @@ Hashtbl.fold (fun x _ acc -> x::acc) m [] in
  aux start
