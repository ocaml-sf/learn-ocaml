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

(** The exercise parts required by the exercise page and the grader.
    Not the metadata from the repository. *)
type t

(** An exercise field accessor *)
type 'a field

(** Get was called on a missing undefaulted field *)
exception Missing_field of string

(** Access a field in an exercise, may raise [Missing_field] *)
val get: 'a field -> t -> 'a

(** Check the existence of a field in an exercise *)
val has: 'a field -> t -> bool

(** Access a field in an exercise *)
val set: 'a field -> 'a -> t -> t

(** Learnocaml_exercise id accessor *)
val id: string field

(** Learnocaml_exercise title / name accessor *)
val title: string field

(** Maximum score for the exercise *)
val max_score: int field

(** Returns the (private, already decyphered) [prepare.ml] *)
val prepare: string field

(** Returns the (private, already decyphered) [solution.ml] *)
val solution: string field

(** Returns the (private, already decyphered) [test.ml] *)
val test: string field

(** Returns the (public) [prelude.ml] *)
val prelude: string field

(** Returns the (public) [template.ml] *)
val template: string field

(** Returns the (public) [descr.html] *)
val descr: string field

(** Reader and decipherer *)
val read:
  read_field:(string -> string option) ->
  ?id:string -> ?decipher:bool -> unit ->
  t

(** Writer and cipherer, ['a] can be [unit] *)
val write:
  write_field:(string -> string -> 'a -> 'a) ->
  t -> ?cipher:bool -> 'a ->
  'a

(** Reader and decipherer with {!Lwt} *)
val read_lwt:
  read_field:(string -> string option Lwt.t) ->
  ?id:string -> ?decipher:bool -> unit ->
  t Lwt.t

(** Writer and cipherer with {!Lwt}, ['a] can be [unit] *)
val write_lwt:
  write_field:(string -> string -> 'a -> 'a Lwt.t) ->
  t -> ?cipher:bool -> 'a ->
  'a Lwt.t

(** JSON serializer, with {!id} field included *)
val enc : t Json_encoding.encoding
