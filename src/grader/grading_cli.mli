(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

val get_grade :
     ?callback:(string -> unit)
  -> ?timeout:int
  -> ?dirname:string
  -> Learnocaml_exercise.t
  -> string
  -> ((Learnocaml_report.t, exn) result * string * string * string) Lwt.t
(** Take an exercise, a solution, and return the report, stdout,
    stderr and outcomes of the toplevel, or raise ont of the
    exceptions defined in module {!Grading}. *)
