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
  { tutorial_title : string ;
    tutorial_steps : step list }
and step =
  { step_title : string ;
    step_contents : phrase list }
and phrase =
  | Paragraph of text list
  | Enum of text list list
and text =
  | Text of string
  | Code of { code : string ; runnable : bool }
  | Emph of text list
  | Image of { alt : string ; mime : string ; contents : bytes }
  | Math of string

open Json_encoding

let text_enc =
  mu "text" @@ fun content_enc ->
  list @@ union
    [ case string
        (function Text text -> Some text | _ -> None)
        (fun text -> Text text) ;
      case
        (obj1 (req "text" string))
        (function Text text -> Some text | _ -> None)
        (fun text -> Text text) ;
      case
        (obj1 (req "emph" content_enc))
        (function Emph content -> Some content | _ -> None)
        (fun content -> Emph content) ;
      case
        (obj2 (req "code" string) (dft "runnable" bool true))
        (function Code { code ; runnable } -> Some (code, runnable) | _ -> None)
        (fun (code, runnable) -> Code { code ; runnable }) ;
      case
        (obj1 (req "math" string))
        (function Math math-> Some math | _ -> None)
        (fun math -> Math math) ;
      case
        (obj3 (req "image" bytes) (req "alt" string) (req "mime" string))
        (function Image { alt ; mime ; contents = image } -> Some (image, alt, mime) | _ -> None)
        (fun (image, alt, mime) -> Image { alt ; mime ; contents = image }) ]

let phrase_enc =
  union
    [ case
        text_enc
        (function Paragraph phrase -> Some phrase | _ -> None)
        (fun phrase -> Paragraph phrase) ;
      case
        (obj1 (req "enum" (list text_enc)))
        (function Enum items -> Some items | _ -> None)
        (fun items -> Enum items) ]

let tutorial_enc =
  Server_index.check_version_1 @@
  conv
    (fun { tutorial_title ; tutorial_steps } -> (tutorial_title, tutorial_steps))
    (fun (tutorial_title, tutorial_steps) -> { tutorial_title ; tutorial_steps }) @@
  obj2
    (req "title" string)
    (req "steps"
       (list @@
        conv
          (fun { step_title ; step_contents } -> (step_title, step_contents))
          (fun (step_title, step_contents) -> { step_title ; step_contents }) @@
        (obj2
           (req "title" string)
           (req "contents" (list phrase_enc)))))
