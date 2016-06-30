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

(** Take an exercise, a solution, and return the report, stdout,
    stderr and outcomes of the toplevel, or raise ont of the
    exceptions defined in module {!Grading}. *)
val get_grade:
  ?callback:(string -> unit) ->
  Learnocaml_exercise.t -> string ->
  ((Learnocaml_report.report, exn) result * string * string * string) Lwt.t
