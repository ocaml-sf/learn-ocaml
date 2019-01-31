(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** the following are relative paths to the www root, using [/] as path
    separator *)
val exercise_index_path : string

val exercises_dir : string

val exercise_path : string -> string

val lesson_index_path : string

val lessons_dir : string

val lesson_path : string -> string

val tutorial_index_path : string

val tutorials_dir : string

val tutorial_path : string -> string

val focus_path : string

val requirements_path : string
