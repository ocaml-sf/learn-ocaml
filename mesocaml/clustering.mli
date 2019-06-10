open Learnocaml_data

type 'a tree =
  | Node of (float * 'a tree * 'a tree)
  | Leaf of 'a

val string_of_tree : ('a -> string) -> 'a tree -> string

val cluster : (Token.t, (int * string) list) Hashtbl.t -> (Token.t list) tree list

val cluster_flatten : (Token.t, (int * string) list) Hashtbl.t -> (Token.t list) list list
