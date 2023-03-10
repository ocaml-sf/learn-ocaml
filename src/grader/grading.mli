(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Toploop_ext

exception Internal_error of string * error
exception User_code_error of error
exception Invalid_grader

(** Take an exercise, a solution, and return the report, stdout,
    stderr and outcomes of the toplevel, or raise ont of the
    exceptions above. The divert mechanism is a platform dependent way
    of rerouting the standard channel descriptors, as implemented by
    {!Toploop_unix} and {!Toploop_jsoo}. *)
val get_grade:
  ?callback:(string -> unit) ->
  ?timeout:int ->
  ?dirname:string ->
  divert:(string -> out_channel -> (string -> unit) -> (unit -> unit)) ->
  Learnocaml_exercise.t -> string -> (Learnocaml_report.t, exn) result * string * string * string

(** Returns user-friendly messages when called on [Internal_error] or
    [User_code_error] *)
val string_of_exn: exn -> string option
