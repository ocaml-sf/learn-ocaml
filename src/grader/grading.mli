(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 *
 * Learn-OCaml is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Learn-OCaml is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

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
