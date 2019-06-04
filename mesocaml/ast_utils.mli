open Parsetree

val hash_of_struct : structure -> string
val hash_of_value : (Asttypes.rec_flag*value_binding list) -> string
