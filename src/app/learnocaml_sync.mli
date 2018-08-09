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

module Map: Map.S with type key = string

type save_file =
  { nickname: string ;
    all_exercise_states :
      Learnocaml_exercise_state.exercise_state Map.t ;
    all_toplevel_histories :
      Learnocaml_toplevel_history.snapshot Map.t ;
    all_exercise_toplevel_histories :
      Learnocaml_toplevel_history.snapshot Map.t }

val save_file_enc : save_file Json_encoding.encoding

(** Merges two save files, trusting the [mtime] fields to take the most recent
    versions of every item. All other things equal, the fields from the second
    argument are preferred. *)
val sync : save_file -> save_file -> save_file

(** Checks all [mtime] fields to get them back to now in case they are in the
    future. Needed for save files that come from clients with possibly bad
    clocks. *)
val fix_mtimes : save_file -> save_file

module Token: sig
  type t
  val to_path: t -> string
  val to_string: t -> string
  val parse: string -> t
  val check: string -> bool
  val random: unit -> t
  val random_teacher: unit -> t
  val is_teacher: t -> bool

  (** The relative path containing teacher tokens *)
  val teacher_tokens_path: string
end
