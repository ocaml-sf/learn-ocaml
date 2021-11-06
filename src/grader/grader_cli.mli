(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** {2 Configuration options} *)

(** Should should generate exo.{cmi,cmo,cmt} for [prelude ^ " ;;\n" ^ prepare].

    This experimental option can be set from command-line
    (explicit mode: learn-ocaml build [--enable-cmo-build|--disable-cmo-build])
    and thereby regenerate [*.cm*] files unconditionally.
    Otherwise (implicit mode), it is on by default for changed exercises only.

    This feature is useful for learn-ocaml.el but optional (it can be disabled)
    yet it may become mandatory (not an option anymore) if/when the js_of_ocaml
    frontend also fetches [*.cmo] binaries for [{solution,test}.ml].

    Note: the initial value of this [bool ref] (in [*.ml]) is a dummy value. *)
val build_cmo: bool ref

(** Grade the exercise (to be set to [false] if the exercise did not change).

    Use case: if we run [learn-ocaml build --enable-cmo-build] (explicit mode)
    on an unchanged exercise, we will have [!build_cmo && not !build_grade], so
    the [*.cmo] will be built, but the grading will be skipped. *)
val build_grade: bool ref

(** Should stdout / stderr of the grader be echoed *)
val display_std_outputs: bool ref

(** Should outputs of the grader be saved and where *)
val dump_outputs: string option ref

(** Should the reports be saved and where *)
val dump_reports: string option ref

(** Should the message from 'test.ml' be displayed on stdout ? *)
val display_callback: bool ref

(** Should compiler outcome be printed ? *)
val display_outcomes: bool ref

(** Should the tool grade a student file instead of 'solution.ml' ? *)
val grade_student: string option ref

(** Should each test be run with a specific timeout (in secs) ? *)
val individual_timeout: int option ref

(** Display reports to stderr *)
val display_reports: bool ref

(** Should the tool generate and dump a dependency graph of the exercises and where *)
val dump_dot: string option ref

(** {2 Functions} *)

(** Runs the grading process *)
val grade:
  ?print_result:bool -> ?dirname:string -> Learnocaml_data.Exercise.Meta.t -> Learnocaml_exercise.t -> string option ->
  (unit, int) result Lwt.t

val grade_from_dir:
  ?print_result:bool -> string -> string option ->
  (unit, int) result Lwt.t
