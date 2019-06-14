open Learnocaml_data
open Learnocaml_data.Partition

val fold_tree : (float -> 'b -> 'b -> 'b) -> ('a -> 'b) -> 'a tree -> 'b

val weight_of_list_tree : ('a -> int) -> 'a tree -> int

val string_of_tree : ('a -> string) -> 'a tree -> string

val cluster : (Token.t, (int * string) list) Hashtbl.t -> (Token.t list) tree list

val cluster_flatten : (Token.t, (int * string) list) Hashtbl.t -> (Token.t list) list list
