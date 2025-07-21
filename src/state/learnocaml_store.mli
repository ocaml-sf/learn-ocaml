(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_data

(** {2 Configuration options} *)

(** All static data accesses will be made relative to this directory *)
val static_dir: string ref

(** All mutable data access will be made relative to this directory *)
val sync_dir: string ref
val data_dir: string ref

(** {2 Utility server-side conversion functions} *)

(** Used both for file i/o and request handling *)

val get_from_file : 'a Json_encoding.encoding -> string -> 'a Lwt.t
val write_to_file : 'a Json_encoding.encoding -> 'a -> string -> unit Lwt.t

(* [sanitise_path prefix subdir] simplifies "." and ".." references in [subdir],
   and returns the concatenation, but guaranteeing the result remains below
   [prefix] (not accounting for symlinks of course, this is purely syntaxical)
*)
val sanitise_path: string -> string list -> string

(** {2 Static data} *)

module Lesson: sig

  module Index: sig
    include module type of struct include Lesson.Index end
    val get: unit -> t Lwt.t
  end

  include module type of struct include Lesson end with module Index := Lesson.Index

  val get: id -> t Lwt.t

end

module Playground: sig

  module Index: sig
    include module type of struct include Playground.Index end
    val get: unit -> t Lwt.t
  end

  include module type of struct include Playground end with module Index := Playground.Index

  val get: id -> t Lwt.t

end

module Server : sig
  val get : unit -> Server.config Lwt.t
end

module Tutorial: sig

  module Index: sig
    include module type of struct include Tutorial.Index end
    val get: unit -> t Lwt.t
  end

  include module type of struct include Tutorial end with module Index := Tutorial.Index

  val get: id -> t Lwt.t

end

module Exercise: sig

  module Meta: sig
    include module type of struct include Exercise.Meta end
    val get: Exercise.id -> t Lwt.t
  end

  module Index: sig
    include module type of struct include Exercise.Index end
    val get_from_index: t -> t Lwt.t
    val get: unit -> t Lwt.t
    val reload: unit -> unit Lwt.t
  end

  module Status: sig
    include module type of struct include Exercise.Status end

    val is_open:
      Exercise.id -> Token.t ->
      [`Open | `Closed | `Deadline of float] Lwt.t
    val get: Exercise.id -> t Lwt.t
    val set: t -> unit Lwt.t
    val all: unit -> t list Lwt.t
  end

  include module type of struct include Exercise end
  with module Meta := Exercise.Meta
   and module Status := Exercise.Status
   and module Index := Exercise.Index

  val get: id -> t Lwt.t

end



(** {2 Dynamic data} *)
module Session: sig

  include module type of struct include Session end

  type entry = {
      session : Session.t;
      token : Token.t;
      last_connection : float;
    }
  val enc : entry Json_encoding.encoding

  (** Retrieves the token associated with the given session. *)
  val get_user_token : t -> Token.t option Lwt.t

  (** Associates a token to a session. *)
  val set_session : t -> Token.t -> unit Lwt.t

   (** Generates a fresh session identifier *)
  val gen_session : unit -> Session.t
end


module Token: sig

  include module type of struct include Token end

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

  include module type of struct include Save end

  val get: Token.t -> t option Lwt.t

  (** Writes the given save to disk. Note: writing to an unregistered teacher
      token will be rejected with [Failure], writing to an unregistered student
      token registers it. *)
  val set: Token.t -> t -> unit Lwt.t

end

module Student: sig

  module Index: sig
    include module type of struct include Student.Index end
    val get: unit -> t Lwt.t

    (** Does not affect the registered students absent from the list. Only the
        tags can be updated this way, the rest needs to be set through
        [Save.set] *)
    val set: Student.t list -> unit Lwt.t
  end

  include module type of struct include Student end with module Index := Student.Index

  val get: student token -> t option Lwt.t

  (** Only updates the tags at the moment, the rest is stored with [Save.set].
      Use [Index.set] to set multiple students at once. *)
  val set: t -> unit Lwt.t

end

module LtiIndex: sig

  (** Adds a new LTI entry to the store. *)
  val add : string -> Token.t -> unit Lwt.t

  (** Retrieves the token associated with the given user ID. *)
  val get_user_token : string -> Token.t option Lwt.t

  (** Checks if the user ID exists in the LTI index. *)
  val exists : string -> bool Lwt.t

end

module TokenIndex : sig

  type methods = {
    idmoodle : string option;
    email    : string option;
  }

  (** Associate a token with an external authentication method and value (e.g. "idmoodle", "email"). *)
  val add_association : token:Token.t -> method_:string -> value:string -> unit Lwt.t

  (** Retrieve all associated external methods for a given token. *)
  val get_methods : Token.t -> methods option Lwt.t

  (** Check if a token is associated with a specific method (e.g. "idmoodle", "email"). *)
  val has : Token.t -> string -> bool Lwt.t

  (** Check if a token exists in the TokenIndex (i.e. has any association). *)
  val exists : Token.t -> bool Lwt.t

end

module NonceIndex: sig
  val create_index : unit -> string Lwt.t

  val get_first_oauth : unit -> (string * string list) Lwt.t
  val get_current_secret : unit -> string Lwt.t

  (** Delete all secrets + nonce associated excepted the current secret with its nonces *)
  val purge : unit -> unit Lwt.t

  val add_nonce : string -> unit Lwt.t
  val check_nonce : string -> bool Lwt.t

end