(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

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
  Token.t option -> Exercise.id -> bool -> (Exercise.Meta.t * Exercise.t * float option) Lwt.t

val[@deprecated] fetch_lesson_index: unit -> Lesson.Index.t Lwt.t
val[@deprecated] fetch_lesson : string -> Lesson.t Lwt.t

val[@deprecated] fetch_tutorial_index : unit -> Tutorial.Index.t Lwt.t
val[@deprecated] fetch_tutorial : string -> Tutorial.t Lwt.t
