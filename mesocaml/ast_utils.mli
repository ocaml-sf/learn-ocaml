open Parsetree

val hash_of_struct : structure -> string
val hash_of_bindings : (Asttypes.rec_flag*value_binding list) -> string
