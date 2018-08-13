(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2018 OCamlPro.
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

module Exercise: sig

  type id = string

  type tag = string

  type t = {
    id: id;
    path: string list;
    meta: Learnocaml_index.exercise;
    tags: tag list;
    (* XXX Learnocaml_process_exercise_repository.exercise_meta, but not
       exported at the moment *)
(*
    attempted_count: int;
    complete_count: int;
    average_grade: int;
*)
  }

  module Map = Map.S with type key = id

  (** [load exercise_index_path] loads the exercise index into memory *)
  val load: string -> unit

  (** Lists the registered exercises *)
  val select: ?filter: unit -> ?sort: unit -> Learnocaml_index.exercise list

end

module Student: sig

  type tag = string

  type token = Token.t

  type t = {
    token: token;
    nickname: string;
    exercises: Learnocaml_exercise_state.exercise_state ExoMap.t;
    tags: tag list;
  }

  module Set = Set.S with type elt = token

end

module Assignment: sig

  type t = {
    exercises: Exercise.id list;
    start_date: float option;
    end_date: float option;
    is_open: bool;
    students: Student.Set.t
  }

  module Set = Set.S with type elt = t

end

module Teacher: sig

  type token = Token.t

  type t = {
    token: token;
    nickname: string;
    students: Student.set;
    assignments: Assignment.t
  }

end

(** Initial load of all the data. *)
val init: exercise_index:string -> unit
