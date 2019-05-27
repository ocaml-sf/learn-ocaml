(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** This module defines the complete LearnOCaml API, with helpers for generating
    well-typed implementations of both the client and server side.

    - the [request] GADT gives all requests with their input and output types
    - the [response_codec] function gives the underlying encoding for responses
      to all requests
    - the [to_http_request] function maps the [request] type to HTTP requests
    - the [server_handler] function conversely maps inbound HTTP requests to the
      [request] type, and passes the results to a callback.
    - the [make_request] function takes a [request], and retrieves the
      well-typed response from the server
*)

open Learnocaml_data

val version: string

type _ request =
  | Static:
      string list -> string request
  | Version:
      unit -> string request
  | Create_token:
      string * student token option * string option -> student token request
  | Create_teacher_token:
      teacher token -> teacher token request
  | Fetch_save:
      'a token -> Save.t request
  | Update_save:
      'a token * Save.t -> Save.t request
  | Git:
      'a token * string list -> string request

  | Students_list:
      teacher token -> Student.t list request
  | Set_students_list:
      teacher token * (Student.t * Student.t) list -> unit request
    (** Does not affect the students absent from the list. the pairs are the
        before/after states, used for merging *)
  | Students_csv:
      teacher token * Exercise.id list * Token.t list -> string request

  | Exercise_index:
      'a token -> (Exercise.Index.t * (Exercise.id * float) list) request
  | Exercise:
      'a token * string -> (Exercise.Meta.t * Exercise.t * float option) request

  | Lesson_index:
      unit -> (string * string) list request
  | Lesson:
      string -> Lesson.t request

  | Tutorial_index:
      unit -> Tutorial.Index.t request
  | Tutorial:
      string -> Tutorial.t request

  | Exercise_status_index:
      teacher token -> Exercise.Status.t list request
  | Exercise_status:
      teacher token * Exercise.id -> Exercise.Status.t request
  | Set_exercise_status:
      teacher token * (Exercise.Status.t * Exercise.Status.t) list ->
      unit request
    (** The two Status.t correspond to the states before and after changes, used
        for three-way merge *)

  | Invalid_request:
      string -> string request
    (** Only for server-side handling: bound to requests not matching any case
        above *)

type http_request = {
  meth: [ `GET | `POST of string];
  path: string list;
  args: (string * string) list;
}

module type JSON_CODEC = sig
  val decode: 'a Json_encoding.encoding -> string -> 'a
  val encode: ?minify:bool -> 'a Json_encoding.encoding -> 'a -> string
end

module type REQUEST_HANDLER = sig
  type 'resp ret

  val map_ret: ('a -> 'b) -> 'a ret -> 'b ret

  val callback: int -> 'resp request -> 'resp ret
end

module Server: functor (Json: JSON_CODEC) (Rh: REQUEST_HANDLER) -> sig

  (** Helper to define a server: handles recognition of the incoming request, and
      encoding of the response. *)
  val handler: int -> http_request -> string Rh.ret

end

module Client: functor (Json: JSON_CODEC) -> sig

  (** Helper to make a client request: handles encoding of the request and
      decoding of the response. *)
  val make_request:
    (http_request -> (string, 'a) result Lwt.t) ->
    'resp request -> ('resp, 'a) result Lwt.t

end
