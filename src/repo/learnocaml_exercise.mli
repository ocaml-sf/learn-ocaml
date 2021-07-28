(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Internal representation of the exercises files, including the metadata from
    the repository. *)

type t

type id = string

(* JSON encoding of the exercise representation. Includes cipher and decipher at
   at encoding and decoding. *)
val encoding : t Json_encoding.encoding

(** Intermediate representation of files, resulting of reading the exercise directory *)
module File : sig
  (** The exercise parts required by the exercise page and the grader.
      Not the metadata from the repository. *)
  type files

  (** An exercise file accessor *)
  type 'a file

  (** Get was called on a missing undefaulted field *)
  exception Missing_file of string

  val get : 'a file -> files -> 'a
  (** Access a field in an exercise, may raise [Missing_file] *)

  val get_opt : 'a option file -> files -> 'a option
  (** Access an optional field in an exercise *)

  val has : 'a file -> files -> bool
  (** Check the existence of a field in an exercise *)

  val set : 'a file -> 'a -> files -> files
  (** Access a field in an exercise *)

  val key : 'a file -> string
  (** Returns the key (i.e. then name) of a file *)

  val id : id file
  (** Learnocaml_exercise id accessor *)

  (* (\** Learnocaml_exercise title / name accessor *\)
   * val title: string file *)

  val max_score : int file
  (** Maximum score for the exercise *)

  val prepare : string file
  (** Returns the (private, already deciphered) [prepare.ml] *)

  val solution : string file
  (** Returns the (private, already deciphered) [solution.ml] *)

  val test : string file
  (** Returns the (private, already deciphered) [test.ml] *)

  val prelude : string file
  (** Returns the (public) [prelude.ml] *)

  val template : string file
  (** Returns the (public) [template.ml] *)

  val descr : (string * string) list file
  (** Returns the (public) [descr.html] *)

  val depend : string option file
  (** Returns the (public) depend file *)

  val dependencies : string option -> string file list
  (** [dependencies txt] create the (private, already deciphered) dependencies 
      declared in [txt] *)
end

val access : 'a File.file -> t -> 'a
(** Access a field from the exercise, using the [t] representation, without **
    deciphering it. May raise [Missing_file] if the field is optional and set to
    [None]. *)

val decipher : string File.file -> t -> string
(** Access a string field from the exercise, using the [t] representation, and
    deciphers if necessary. May raise [Missing_file] if the field is optional and
    set to [None]. *)

val update : 'a File.file -> 'a -> t -> t
(** Updates the value of a field of the exercise in its [t] representation. *)

val cipher : string File.file -> string -> t -> t
(** Updates the value of a field of the exercise in its [t] representation, and
    ciphers it. *)

val read :
     read_field:(string -> string option)
  -> ?id:string
  -> ?decipher:bool
  -> unit
  -> t
(** Reader and decipherer *)

val write :
  write_field:(string -> string -> 'a -> 'a) -> t -> ?cipher:bool -> 'a -> 'a
(** Writer and cipherer, ['a] can be [unit] *)

val read_lwt :
     read_field:(string -> string option Lwt.t)
  -> ?id:string
  -> ?decipher:bool
  -> unit
  -> t Lwt.t
(** Reader and decipherer with {!Lwt} *)

val write_lwt :
     write_field:(string -> string -> 'a -> 'a Lwt.t)
  -> t
  -> ?cipher:bool
  -> 'a
  -> 'a Lwt.t
(** Writer and cipherer with {!Lwt}, ['a] can be [unit] *)

val enc : t Json_encoding.encoding
(** JSON serializer, with {!id} file included *)
