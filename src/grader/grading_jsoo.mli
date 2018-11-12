(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016 OCamlPro.
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

exception Timeout

(** Launch a worker, send it the exercise, grade it, return the result
    and kill the worker. Fail with {!Timeout} after [timeout] seconds. *)
val get_grade :
  ?worker_js_file: string ->
  ?callback:(string -> unit) ->
  ?timeout: float ->
  Learnocaml_exercise.t ->
  (string -> (Learnocaml_report.t * string * string * string) Lwt.t) Lwt.t
