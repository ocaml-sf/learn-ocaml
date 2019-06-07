open Learnocaml_data

type 'a tree =
  | Node of ('a tree * 'a tree)
  | Leaf of 'a

val string_of_token_tree : Token.t tree -> string
          
val cluster : (Token.t, (int * string) list) Hashtbl.t -> Token.t tree
