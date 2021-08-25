(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type lesson = {lesson_title : string; lesson_steps : step list}

and step = {step_title : string; step_phrases : phrase list}

and phrase = Text of string | Code of string

open Json_encoding

let lesson_enc =
  Learnocaml_index.check_version_2
  @@ conv
       (fun {lesson_title; lesson_steps} -> (lesson_title, lesson_steps))
       (fun (lesson_title, lesson_steps) -> {lesson_title; lesson_steps})
  @@ obj2 (req "title" string)
       (req "steps"
          ( list
          @@ conv
               (fun {step_title; step_phrases} -> (step_title, step_phrases))
               (fun (step_title, step_phrases) -> {step_title; step_phrases})
          @@ obj2 (req "title" string)
               (req "contents"
                  ( list
                  @@ union
                       [ case
                           (obj1 (req "html" string))
                           (function Text text -> Some text | Code _ -> None)
                           (fun text -> Text text)
                       ; case
                           (obj1 (req "code" string))
                           (function Code code -> Some code | Text _ -> None)
                           (fun code -> Code code) ] )) ))
