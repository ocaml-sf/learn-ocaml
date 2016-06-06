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

open Lwt.Infix

exception Cannot_fetch of string

let cannot_fetch msg = Lwt.fail (Cannot_fetch msg)

let fetch filename =
  let path = Regexp.(split (regexp "/")) filename in
  let rel_uri = String.concat "_" path in
  Lwt_request.get_file rel_uri >>= function
  | None -> cannot_fetch ("Cannot download " ^ filename ^ ".")
  | Some text -> Lwt.return text

let fetch_json filename =
  fetch filename >>= fun text ->
  try Lwt.return (Js._JSON##parse (Js.string text)) with
  | _ -> cannot_fetch ("Invalid JSON for " ^ filename)

let fetch_and_decode_json enc filename =
  fetch_json filename >>= fun json ->
  try Lwt.return (Browser_json.Json_encoding.destruct enc json) with
  | Json_encoding.Cannot_destruct _ -> cannot_fetch ("Invalid structure for " ^ filename)

let fetch_exercise_index () =
  fetch_and_decode_json
    Server_index.exercise_index_enc
    Server_paths.exercise_index_path

let fetch_exercise id =
  fetch_and_decode_json
    Exercise.enc
    (Server_paths.exercise_path id)

let fetch_lesson_index () =
  fetch_and_decode_json
    Server_index.lesson_index_enc
    Server_paths.lesson_index_path

let fetch_lesson id =
  fetch_and_decode_json
    Lesson.lesson_enc
    (Server_paths.lesson_path id)
