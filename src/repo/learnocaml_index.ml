(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

let exercise_index_path = "exercises.json"

let exercises_dir = "exercises"

let exercise_path id = exercises_dir ^ "/" ^ id ^ ".json"

let playground_dir = "playgrounds"

let playground_path id = playground_dir ^ "/" ^ id ^ ".json"

let playground_index_path = "playgrounds.json"

let lesson_index_path = "lessons.json"

let lessons_dir = "lessons"

let lesson_path id = lessons_dir ^ "/" ^ id ^ ".json"

let tutorial_index_path = "tutorials.json"

let tutorials_dir = "tutorials"

let tutorial_path id = tutorials_dir ^ "/" ^ id ^ ".json"

let focus_path = "focus.json"

let requirements_path = "requirements.json"

let server_config_path = "server_config.json"
