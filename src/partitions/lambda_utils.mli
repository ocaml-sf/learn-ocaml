val hash_lambda :
    int (* pourcentage du poids max conditionnant les hashs des sous-abres conservés *)
  -> Lambda.lambda (* Le lambda *)
  -> (int*string) * (int*string) list (* Les hashs des sous-arbres + le hash de l'arbre *)

val inline_all : Lambda.lambda -> Lambda.lambda
