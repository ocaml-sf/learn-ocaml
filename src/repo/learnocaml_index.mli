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

type exercise_kind =
  | Project
  | Problem
  | Learnocaml_exercise

type exercise =
  { exercise_kind : exercise_kind ;
    exercise_title : string ;
    exercise_short_description : string option ;
    exercise_stars : float (* in [0.,4.] *) }

and group =
  { group_title : string ;
    group_contents : group_contents }

and group_contents =
  | Learnocaml_exercises of exercise Map.Make (String).t
  | Groups of group Map.Make (String).t

val exercise_index_enc : group_contents Json_encoding.encoding

val lesson_index_enc : (string * string) list Json_encoding.encoding

type word =
  | Text of string
  | Code of code
  | Emph of text
  | Image of { alt : string ; mime : string ; contents : bytes }
  | Math of string
and text =
  word list
and code =
  { code : string ; runnable : bool }

val text_enc : text Json_encoding.encoding

type tutorial =
  { tutorial_name : string ;
    tutorial_title : text }

and series =
  { series_title : string ;
    series_tutorials : tutorial list }

val tutorial_index_enc : series Map.Make (String).t Json_encoding.encoding

val check_version_1 : 'a Json_encoding.encoding -> 'a Json_encoding.encoding

val exercise_index_path : string

val exercise_path : string -> string

val lesson_index_path : string

val lesson_path : string -> string

val tutorial_index_path : string

val tutorial_path : string -> string

val map_enc :'a Json_encoding.encoding -> 'a Map.Make(String).t Json_encoding.encoding
    
val exercise_enc :exercise Json_encoding.encoding
