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
  { tutorial_title : Learnocaml_index.text ;
    tutorial_steps : step list }
and step =
  { step_title : Learnocaml_index.text ;
    step_contents : phrase list }
and phrase =
  | Paragraph of Learnocaml_index.text
  | Enum of phrase list list
  | Code_block of Learnocaml_index.code

open Json_encoding

let phrase_enc =
  mu "phrase" @@ fun phrase_enc ->
  union
    [ case
        (obj1 (req "paragraph" Learnocaml_index.text_enc))
        (function Paragraph phrase -> Some phrase | _ -> None)
        (fun phrase -> Paragraph phrase) ;
      case
        (obj1 (req "enum" (list (list phrase_enc))))
        (function Enum items -> Some items | _ -> None)
        (fun items -> Enum items) ;
      case
        (obj2 (req "code" string) (dft "runnable" bool false))
        (function Code_block { Learnocaml_index.code ; runnable } ->
           Some (code, runnable) | _ -> None)
        (fun (code, runnable) ->
           Code_block { Learnocaml_index.code ; runnable }) ;
      case
        Learnocaml_index.text_enc
        (function Paragraph phrase -> Some phrase | _ -> None)
        (fun phrase -> Paragraph phrase) ]

let tutorial_enc =
  Learnocaml_index.check_version_2 @@
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
