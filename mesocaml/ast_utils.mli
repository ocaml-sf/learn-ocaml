open Parsetree

val hash_of_bindings :
  int (* Poids min pour garder les hashs des sous-arbres *)
  -> (Asttypes.rec_flag*value_binding list) (* Les bindings d'un let *)
  -> string*string list (* Les hashs des sous-arbres + le hash de l'arbre *)

val hash_of_structure :
  int
  -> structure
  -> string * string list
