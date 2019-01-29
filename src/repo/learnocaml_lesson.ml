(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
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

type lesson =
  { lesson_title : string ;
    lesson_steps : step list }
and step =
  { step_title : string ;
    step_phrases : phrase list }
and phrase =
  | Text of string
  | Code of string

open Json_encoding

let lesson_enc =
  Learnocaml_index.check_version_2 @@
  conv
    (fun { lesson_title ; lesson_steps } -> (lesson_title, lesson_steps))
    (fun (lesson_title, lesson_steps) -> { lesson_title ; lesson_steps }) @@
  obj2
    (req "title" string)
    (req "steps"
       (list @@
        conv
          (fun { step_title ; step_phrases } -> (step_title, step_phrases))
          (fun (step_title, step_phrases) -> { step_title ; step_phrases }) @@
        (obj2
           (req "title" string)
           (req "contents"
              (list @@ union
                 [ case
                     (obj1 (req "html" string))
                     (function Text text -> Some text | Code _ -> None)
                     (fun text -> Text text) ;
                   case
                     (obj1 (req "code" string))
                     (function Code code -> Some code | Text _ -> None)
                     (fun code -> Code code) ])))))
