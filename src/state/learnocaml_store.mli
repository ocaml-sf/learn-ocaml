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

open Learnocaml_data

(** {2 Configuration options} *)

(** All static data accesses will be made relative to this directory *)
val static_dir: string ref

(** All mutable data accesss will be made relative to this directory *)
val sync_dir: string ref

(** {2 Utility server-side conversion functions} *)

(** Used both for file i/o and request handling *)
module Json_codec: Learnocaml_api.JSON_CODEC

(** {2 Static data} *)

module Lesson: sig

  type id = string

  type t = Learnocaml_lesson.lesson

  val enc: t Json_encoding.encoding

  val get: id -> t Lwt.t

  module Index: sig

    type t = (string * string) list

    val enc: t Json_encoding.encoding

    val get: unit -> t Lwt.t

  end

end

module Tutorial: sig

  type id = string

  type t = Learnocaml_tutorial.tutorial

  val enc: t Json_encoding.encoding

  val get: id -> t Lwt.t

  module Index: sig

    type t = (string * Learnocaml_index.series) list

    val enc: t Json_encoding.encoding

    val get: unit -> t Lwt.t

  end

end

(** {2 Dynamic data} *)

module Token: sig

  include module type of Token with type t = Token.t

  (** Initialise and register a new student token *)
  val create_student: unit -> t Lwt.t

  (** Initialise and register a new student token *)
  val create_teacher: unit -> t Lwt.t

  (** Registers the given token. By default, only registration of student tokens
      is allowed. [Failure] is raised if the token exists already. *)
  val register: ?allow_teacher:bool -> t -> unit Lwt.t

  (** Check if the token has been registered *)
  val exists: t -> bool Lwt.t

  val delete: t -> unit Lwt.t

  (** True for registered teacher tokens only *)
  val check_teacher: t -> bool Lwt.t

  module Index: sig

    type nonrec t = t list

    val enc: t Json_encoding.encoding

    val get: unit -> t Lwt.t

  end

end

module Save: sig

  include module type of Save with type t = Save.t

  val get: Token.t -> t option Lwt.t

  (** Writes the given save to disk. Note: writing to an unregistered teacher
      token will be rejected with [Failure], writing to an unregistered student
      token registers it. *)
  val set: Token.t -> t -> unit Lwt.t

end

module Exercise: sig

  type id = string

  type tag = string

  type status = Open | Closed

  type assignment = {
    start: float;
    stop: float;
  }

  type t = {
    id: id;
    path: string list;
    meta: Learnocaml_index.exercise;
    tags: tag list;
    status: status;
    assigned: assignment Token.Map.t;
  }

  val enc: t Json_encoding.encoding

  (** The base index, {i without} the mutable part *)
  module Index: sig

    type t = Learnocaml_index.group_contents

    val enc: t Json_encoding.encoding

    val reload: unit -> unit

  end


  (* (\** [load exercise_index_path] loads the exercise index into memory *\)
   * val load: string -> unit
   * 
   * (\** Lists the registered exercises *\)
   * val select: ?filter: unit -> ?sort: unit -> Learnocaml_index.exercise list *)

end

module Student: sig

  include module type of Student with type t = Student.t

end


(* module Teacher: sig
 * 
 *   type token = Token.t
 * 
 *   type t = {
 *     token: token;
 *     nickname: string;
 *     students: Student.set;
 *     assignments: Assignment.t
 *   }
 * 
 * end *)
