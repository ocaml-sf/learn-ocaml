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

open Learnocaml_data

type request_error = [
  | `Unreachable of string
  | `Not_found of string
  | `Http_error of int * string
  | `Exception of exn
  | `Invalid_response of exn
]

val string_of_error: request_error -> string

val request: 'a Learnocaml_api.request -> ('a, request_error) result Lwt.t

exception Cannot_fetch of string
val request_exn: 'a Learnocaml_api.request -> 'a Lwt.t

val[@deprecated] fetch_exercise:
  Token.t -> Exercise.id -> (Exercise.Meta.t * Exercise.t * float option) Lwt.t

val[@deprecated] fetch_lesson_index: unit -> Lesson.Index.t Lwt.t
val[@deprecated] fetch_lesson : string -> Lesson.t Lwt.t

val[@deprecated] fetch_tutorial_index : unit -> Tutorial.Index.t Lwt.t
val[@deprecated] fetch_tutorial : string -> Tutorial.t Lwt.t
