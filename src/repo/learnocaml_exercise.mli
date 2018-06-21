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

(** Internal representation of the exercises files, including the metadata from
    the repository. *)

type t

type id = string

(* JSON encoding of the exercise representation. Includes cipher and decipher at
   at encoding and decoding. *)
val encoding: t Json_encoding.encoding

(** Intermediate representation of files, resulting of reading the exercise directory *)
module File : sig

  (** The exercise parts required by the exercise page and the grader.
      Not the metadata from the repository. *)
  type files

  (** An exercise file accessor *)
  type 'a file

  (** Get was called on a missing undefaulted field *)
  exception Missing_file of string

  (** Access a field in an exercise, may raise [Missing_field] *)
  val get: 'a file -> files -> 'a

  (** Check the existence of a field in an exercise *)
  val has: 'a file -> files -> bool

  (** Access a field in an exercise *)
  val set: 'a file -> 'a -> files -> files

  (** Learnocaml_exercise id accessor *)
  val id: id file

  (** Learnocaml_exercise title / name accessor *)
  val title: string file

  (** Maximum score for the exercise *)
  val max_score: int file

  (** Returns the (private, already decyphered) [prepare.ml] *)
  val prepare: string file

  (** Returns the (private, already decyphered) [solution.ml] *)
  val solution: string file

  (** Returns the (private, already decyphered) [test.ml] *)
  val test: string file

  (** Returns the (public) [prelude.ml] *)
  val prelude: string file

  (** Returns the (public) [template.ml] *)
  val template: string file

  (** Returns the (public) [descr.html] *)
  val descr: (string * string) list file

end

(** Access a field from the exercise, using the [t] representation, without **
    deciphering it. May raise [Missing_file] if the field is optional and set to
    [None]. *)
val access: 'a File.file -> t -> 'a

(** Access a string field from the exercise, using the [t] representation, and
    deciphers if necessary. May raise [Missing_file] if the field is optional and
    set to [None]. *)
val decipher: string File.file -> t -> string

(** Updates the value of a field of the exercise in its [t] representation. *)
val update: 'a File.file -> 'a -> t -> t

(** Updates the value of a field of the exercise in its [t] representation, and
    ciphers it. *)
val cipher: string File.file -> string -> t -> t

val meta_from_index: Learnocaml_index.exercise -> Learnocaml_meta.meta

(** Generates the exercise representation for the exercises index. *)
val to_index: t -> Learnocaml_index.exercise

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

(** JSON serializer, with {!id} file included *)
val enc : t Json_encoding.encoding
