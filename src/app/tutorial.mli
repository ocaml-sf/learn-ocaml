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

val tutorial_enc : tutorial Json_encoding.encoding
