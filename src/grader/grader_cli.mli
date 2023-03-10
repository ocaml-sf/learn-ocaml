(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** {2 Configuration options} *)

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
