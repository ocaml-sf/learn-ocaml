(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type error =
  | Internal_error of string * Toploop_ext.error
  | User_code_error of Toploop_ext.error
  | Invalid_grader

exception Grading_error of error

(** Take an exercise, a solution, and return the report, stdout,
    stderr and outcomes of the toplevel, or raise ont of the
    exceptions above. The divert mechanism is a platform dependent way
    of rerouting the standard channel descriptors, as implemented by
    {!Toploop_unix} and {!Toploop_jsoo}. {load_code} is expected to load
    compiled code, either in {cmo} or {js} depending on the backend. *)
val get_grade:
  ?callback:(string -> unit) ->
  ?timeout:int ->
  ?dirname:string ->
  divert:(string -> out_channel -> (string -> unit) -> (unit -> unit)) ->
  load_code:(Learnocaml_exercise.compiled_lib -> bool Toploop_ext.toplevel_result) ->
  Learnocaml_exercise.t -> string -> (Learnocaml_report.t, error) result * string * string * string

(** Returns user-friendly messages when called on [Internal_error] or
    [User_code_error] *)
val string_of_err: error -> string
