(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** {2 Configuration options} *)

val display_std_outputs : bool ref
(** Should stdout / stderr of the grader be echoed *)

val dump_outputs : string option ref
(** Should outputs of the grader be saved and where *)

val dump_reports : string option ref
(** Should the reports be saved and where *)

val display_callback : bool ref
(** Should the message from 'test.ml' be displayed on stdout ? *)

val display_outcomes : bool ref
(** Should compiler outcome be printed ? *)

val grade_student : string option ref
(** Should the tool grade a student file instead of 'solution.ml' ? *)

val individual_timeout : int option ref
(** Should each test be run with a specific timeout (in secs) ? *)

val display_reports : bool ref
(** Display reports to stderr *)

val dump_dot : string option ref
(** Should the tool generate and dump a dependency graph of the exercises and where *)

(** {2 Functions} *)

val grade :
     ?print_result:bool
  -> ?dirname:string
  -> Learnocaml_data.Exercise.Meta.t
  -> Learnocaml_exercise.t
  -> string option
  -> (unit, int) result Lwt.t
(** Runs the grading process *)

val grade_from_dir :
  ?print_result:bool -> string -> string option -> (unit, int) result Lwt.t
