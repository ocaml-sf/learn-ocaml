(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
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

module type COMPAT = sig
  (** List-based versions endowed with a lexicographic order. *)
  type t

  val to_string : t -> string

  (** Supported formats: [Compat.v "str"] where "str" is nonempty and
      either "n", "-n" (a signed integer), or "n.str".
      However, [Compat.v "0.14.rc1"] or so is not supported for now. *)
  val v : string -> t

  (** Note that trailing zeros are ignored, i.e. (v "1") and (v "1.0")
      are equal versions. But (v "1") is higher than (v "1.-1"), itself
      higher than (v "1.-2"), and so on. *)
  val le : t -> t -> bool

  val eq : t -> t -> bool

  val lt : t -> t -> bool

  type pred =
    | Since of t | Upto of t | And of pred * pred

  val compat : pred -> t -> bool
end

module Compat: COMPAT

(** Note about backward-compatibility:

The architecture of learn-ocaml merges the (client, server) components
in the same codebase, so it's easier to update both of them in one go.

But this tight coupling meant that a learn-ocaml-client version would
only be compatible with a single server version, hence a frequent but
annoying error "API version mismatch: client v.x and server v.y".

So since learn-ocaml 0.13.0, a given client_version will try to be
compatible with as much server_version's as possible such that
[client_version >= server_version && server_version >= "0.12"].

To this aim, each [request] constructor comes with a version
constraint of type [Compat.pred], see [supported_versions].

Regarding the inevitable extensions of the API:

- make sure we only add constructors to this [request] type,
- and that their semantics does not change
  (or at least in a backward-compatible way;
   see PR https://github.com/ocaml-sf/learn-ocaml/pull/397
   for a counter-example);
- but if a given entrypoint would need to be removed,
  add a [Compat.Upto] constraint (*<*) instead.
 *)
type _ request =
  | Static:
      string list -> string request
  | Version:
      unit -> (string * int) request
  | Nonce:
      unit -> string request
  | Create_token:
      string * student token option * string option -> student token request
  | Create_teacher_token:
      teacher token -> teacher token request
  | Fetch_save:
      'a token -> Save.t request
  | Archive_zip:
      'a token -> string request
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
      'a token option -> (Exercise.Index.t * (Exercise.id * float) list) request
  | Exercise:
      'a token option * string -> (Exercise.Meta.t * Exercise.t * float option) request

  | Lesson_index:
      unit -> (string * string) list request
  | Lesson:
      string -> Lesson.t request

  | Tutorial_index:
      unit -> Tutorial.Index.t request
  | Tutorial:
      string -> Tutorial.t request

  | Playground_index:
      unit -> Playground.Index.t request
  | Playground:
      string -> Playground.t request

  | Exercise_status_index:
      teacher token -> Exercise.Status.t list request
  | Exercise_status:
      teacher token * Exercise.id -> Exercise.Status.t request
  | Set_exercise_status:
      teacher token * (Exercise.Status.t * Exercise.Status.t) list ->
      unit request
    (** The two Status.t correspond to the states before and after changes, used
        for three-way merge *)

  | Partition:
      teacher token * Exercise.id * string * int -> Partition.t request

  | Invalid_request:
      string -> string request
    (** Only for server-side handling: bound to requests not matching any case
        above *)

val supported_versions: 'a request -> Compat.pred

(** [is supported client server req] = Ok () if
    [server <= client && "client supports req" && "server supports req"] *)
val is_supported:
  ?current:Compat.t -> server:Compat.t ->
  'resp request -> (unit, string) result

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

  val callback: Conduit.endp ->
                Learnocaml_data.Server.config -> 'resp request -> 'resp ret
end

module Server: functor (_: JSON_CODEC) (Rh: REQUEST_HANDLER) -> sig

  (** Helper to define a server: handles recognition of the incoming request, and
      encoding of the response. *)
  val handler: Conduit.endp ->
               Learnocaml_data.Server.config -> http_request -> string Rh.ret

end

module Client: functor (_: JSON_CODEC) -> sig

  (** Helper to make a client request: handles encoding of the request and
      decoding of the response. *)
  val make_request:
    (http_request -> (string, 'a) result Lwt.t) ->
    'resp request -> ('resp, 'a) result Lwt.t

end
