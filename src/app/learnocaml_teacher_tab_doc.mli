(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2023 OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** This module contains documentation strings (in markdown) for the different
    panes in the teacher tab *)

type markdown_doc = string * string (** title, markdown text *)

val exercises_pane_md: markdown_doc
val students_pane_md: markdown_doc
val assignments_pane_md: markdown_doc
