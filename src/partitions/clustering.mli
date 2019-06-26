open Learnocaml_data
open Learnocaml_data.Partition

val cluster : (Token.t, (int * string) list) Hashtbl.t -> (Token.t list) tree list

val cluster_flatten : (Token.t, (int * string) list) Hashtbl.t -> (Token.t list) list list
