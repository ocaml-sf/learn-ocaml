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

module StringMap = Map.Make (String)

type exercise_kind =
  | Project
  | Problem
  | Learnocaml_exercise

type exercise =
  { exercise_kind : exercise_kind ;
    exercise_title : string ;
    exercise_short_description : string option ;
    exercise_stars : float (* \in [0.,4.] *) }

and group =
  { group_title : string ;
    group_contents : group_contents }

and group_contents =
  | Learnocaml_exercises of exercise Map.Make (String).t
  | Groups of group Map.Make (String).t

open Json_encoding

let check_version_1 enc =
  conv
    (fun exercise -> ("1", exercise))
    (fun (version, exercise) ->
       if version <> "1" then begin
         let msg = Format.asprintf "unknown version %s" version in
         raise (Cannot_destruct ([], Failure msg))
       end ;
       exercise)
    (merge_objs (obj1 (req "learnocaml_version" string)) enc)

let map_enc enc =
  conv
    StringMap.bindings
    (List.fold_left (fun s (k,v) -> StringMap.add k v s) StringMap.empty)
    (assoc enc)

let exercise_kind_enc =
  string_enum
    [ "problem", Problem ;
      "project", Project ;
      "exercise", Learnocaml_exercise ]

let exercise_enc =
  conv
    (fun { exercise_kind = kind ;
           exercise_title = title ;
           exercise_short_description = short ;
           exercise_stars = stars } ->
      (kind, title, short, stars))
    (fun (kind, title, short, stars) ->
       { exercise_kind = kind ;
         exercise_title = title ;
         exercise_short_description = short ;
         exercise_stars = stars })
    (obj4
       (req "kind" exercise_kind_enc)
       (req "title" string)
       (opt "shortDescription" string)
       (req "stars" float))

let server_exercise_meta_enc =
  check_version_1 exercise_enc

let group_enc =
  mu "group" @@ fun group_enc ->
  conv
    (fun { group_title ; group_contents } -> (group_title, group_contents))
    (fun (group_title, group_contents) -> { group_title ; group_contents }) @@
  union
    [ case
        (obj2
           (req "title" string)
           (req "exercises" (map_enc exercise_enc)))
        (function
          | (title, Learnocaml_exercises map) -> Some (title, map)
          | _ -> None)
        (fun (title, map) -> (title, Learnocaml_exercises map)) ;
      case
        (obj2
           (req "title" string)
           (req "groups" (map_enc group_enc)))
        (function
          | (title, Groups map) -> Some (title, map)
          | _ -> None)
        (fun (title, map) -> (title, Groups map)) ]

let exercise_index_enc =
  check_version_1 @@
  union
    [ case
        (obj1 (req "exercises" (map_enc exercise_enc)))
        (function
          | Learnocaml_exercises map -> Some map
          | _ -> None)
        (fun map -> Learnocaml_exercises map) ;
      case
        (obj1 (req "groups" (map_enc group_enc)))
        (function
          | Groups map -> Some map
          | _ -> None)
        (fun map -> Groups map) ]

let lesson_index_enc =
  check_version_1 @@
  obj1 (req "lessons" (list @@ tup2 string string))

type word =
  | Text of string
  | Code of { code : string ; runnable : bool }
  | Emph of text
  | Image of { alt : string ; mime : string ; contents : bytes }
  | Math of string
and text =
  word list

let text_enc =
  mu "text" @@ fun content_enc ->
  let word_enc =
    union
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
          (obj2 (req "code" string) (dft "runnable" bool false))
          (function Code { code ; runnable } -> Some (code, runnable) | _ -> None)
          (fun (code, runnable) -> Code { code ; runnable }) ;
        case
          (obj1 (req "math" string))
          (function Math math-> Some math | _ -> None)
          (fun math -> Math math) ;
        case
          (obj3 (req "image" bytes) (req "alt" string) (req "mime" string))
          (function
            | Image { alt ; mime ; contents = image } -> Some (image, alt, mime)
            | _ -> None)
          (fun (image, alt, mime) ->
             Image { alt ; mime ; contents = image }) ] in
  union
    [ case
        word_enc
        (function [ ctns ] -> Some ctns | _ -> None) (fun ctns -> [ ctns ]) ;
      case
        (list @@ word_enc)
        (fun ctns -> Some ctns) (fun ctns -> ctns) ]

type tutorial =
  { tutorial_name : string ;
    tutorial_title : text }

and series =
  { series_title : string ;
    series_tutorials : tutorial list }

let tutorial_index_enc =
  let open Json_encoding in
  let tutorial_enc =
    conv
      (fun { tutorial_name ; tutorial_title } ->
         (tutorial_name, tutorial_title))
      (fun (tutorial_name, tutorial_title) ->
         { tutorial_name ; tutorial_title })
      (tup2 string text_enc) in
  let series_enc =
    conv
      (fun { series_title ; series_tutorials } ->
         (series_title, series_tutorials))
      (fun (series_title, series_tutorials) ->
         { series_title ; series_tutorials }) @@
    obj2
      (req "title" string)
      (req "tutorials" (list tutorial_enc)) in
  check_version_1 @@
  obj1 (req "series" (map_enc series_enc))

let exercise_index_path = "exercises.json"

let exercise_path id = "exercise_" ^ id ^ ".json"

let lesson_index_path = "lessons.json"

let lesson_path id = "lesson_" ^ id ^ ".json"
