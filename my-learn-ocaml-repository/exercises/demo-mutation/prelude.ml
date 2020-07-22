exception NotImplemented

(* Type of a binary tree *)
type tree =
  | Empty
  | Node of tree * int * tree

(* An example binary tree *)
let t = Node (
    Node (Empty, 1, Empty),
    2,
    Node (Empty, 3, Empty)
  )
