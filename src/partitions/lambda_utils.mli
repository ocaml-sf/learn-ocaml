(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

val hash_lambda :
    int (* weight threshold, exprimed in percent compared to the weight of the main AST *)
  -> Lambda.lambda (* The lambda *)
  -> (int*string) * (int*string) list (* The main hash + hash of sub-AST over the threshold *)

(* Inline all possible expression *)
val inline_all : Lambda.lambda -> Lambda.lambda
