(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

exception Timeout

(** Launch a worker, send it the exercise, grade it, return the result
    and kill the worker. Fail with {!Timeout} after [timeout] seconds. *)
val get_grade :
  ?worker_js_file: string ->
  ?callback:(string -> unit) ->
  ?timeout: float ->
  Learnocaml_exercise.t ->
  (string -> (Learnocaml_report.t * string * string * string) Lwt.t) Lwt.t
