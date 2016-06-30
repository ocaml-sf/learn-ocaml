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

type tutorial =
  { tutorial_title : text ;
    tutorial_steps : step list }
and step =
  { step_title : text ;
    step_contents : phrase list }
and phrase =
  | Paragraph of text
  | Enum of text list
and text = Learnocaml_index.text

open Json_encoding

let phrase_enc =
  union
    [ case
        Learnocaml_index.text_enc
        (function Paragraph phrase -> Some phrase | _ -> None)
        (fun phrase -> Paragraph phrase) ;
      case
        (obj1 (req "paragraph" Learnocaml_index.text_enc))
        (function Paragraph phrase -> Some phrase | _ -> None)
        (fun phrase -> Paragraph phrase) ;
      case
        (obj1 (req "enum" (list Learnocaml_index.text_enc)))
        (function Enum items -> Some items | _ -> None)
        (fun items -> Enum items) ]

let tutorial_enc =
  Learnocaml_index.check_version_1 @@
  conv
    (fun { tutorial_title ; tutorial_steps } ->
       (tutorial_title, tutorial_steps))
    (fun (tutorial_title, tutorial_steps) ->
       { tutorial_title ; tutorial_steps }) @@
  obj2
    (req "title" Learnocaml_index.text_enc)
    (req "steps"
       (list @@
        conv
          (fun { step_title ; step_contents } ->
             (step_title, step_contents))
          (fun (step_title, step_contents) ->
             { step_title ; step_contents }) @@
        (obj2
           (req "title" Learnocaml_index.text_enc)
           (req "contents" (list phrase_enc)))))
