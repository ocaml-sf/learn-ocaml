(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type to_worker =
  { exercise : Learnocaml_exercise.t ;
    solution : string }
type from_worker =
  | Callback of string
  | Answer of Learnocaml_report.t * string * string * string

val to_worker_enc : to_worker Json_encoding.encoding
val from_worker_enc : from_worker Json_encoding.encoding
