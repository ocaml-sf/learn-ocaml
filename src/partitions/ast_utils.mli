open Parsetree

val hash_of_bindings :
  int (* pourcentage du poids max conditionnant les hashs des sous-abres conservés *)
  -> (Asttypes.rec_flag*value_binding list) (* Les bindings d'un let *)
  -> (int*string) * (int*string) list (* Les hashs des sous-arbres + le hash de l'arbre *)

val hash_of_structure :
  int
  -> structure
  -> (int*string) * (int*string) list
