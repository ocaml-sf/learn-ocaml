open Learnocaml_data

val partition : string (* Exercise name *)
                -> string (* function name *)
                -> int (* percent of subtrees to keep *)
                -> Partition.t Lwt.t
