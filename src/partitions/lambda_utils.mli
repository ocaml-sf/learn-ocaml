val hash_lambda :
    int (* weight threshold, exprimed in percent compared to the weight of the main AST *)
  -> Lambda.lambda (* The lambda *)
  -> (int*string) * (int*string) list (* The main hash + hash of sub-AST over the threshold *)

(* Inline all possible expression *)
val inline_all : Lambda.lambda -> Lambda.lambda
