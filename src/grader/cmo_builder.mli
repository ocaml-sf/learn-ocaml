(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2021-2022  OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Return [true] if the string matches [^[0-9A-Za-z_-]+$] *)
val check_shortid_ascii: string -> bool

(** Return [true] if the string matches [^given_[0-9A-Za-z_]+$] *)
val check_given: string -> bool

(** Escape "-" and "_" & Prepend "given_".
    Emit a warning if [check_shortid_ascii] doesn't hold on the input string. *)
val given_of_shortid: string -> string

(** Remove "given_" prefix & Unescape "-" and "_".
    Raise a [Failure] if [check_given] doesn't hold on the input string. *)
val shortid_of_given: string -> string

(** Take an exercise and a json filename "./www/exercises/part/exo.json",
    compile [prelude ^ " ;;\n" ^ prepare]
    to "./www/exercises/part/given_exo.{cmi,cmo,cmt}",
    and write prelude (even if that last step is somewhat redundant)
    to "./www/exercises/part/given_exo.ml" *)
val compile_given : Learnocaml_exercise.t -> string -> unit Lwt.t
