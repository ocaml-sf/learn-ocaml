(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016 OCamlPro.
 *
 * Learn-OCaml is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Learn-OCaml is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

let exercise_index_path = "exercises.json"

let exercises_dir = "exercises"

let exercise_path id = exercises_dir ^ "/" ^ id ^ ".json"

let lesson_index_path = "lessons.json"

let lessons_dir = "lessons"

let lesson_path id = lessons_dir ^ "/" ^ "lesson_" ^ id ^ ".json"

let tutorial_index_path = "tutorials.json"

let tutorials_dir = "tutorials"

let tutorial_path id = tutorials_dir ^ "/" ^ id ^ ".json"

let focus_path = "focus.json"

let requirements_path = "requirements.json"
