(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2018 OCamlPro.
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

open Learnocaml_index

open Lwt.Infix

type meta = {
  meta_kind : exercise_kind ;
  meta_stars : float ;
  meta_title : string option ;
  meta_short_description : string option ;
  meta_identifier : string option ;
  meta_author : (string * string) list ;
  meta_focus : string list ;
  meta_requirements : string list ;
  meta_forward : string list ;
  meta_backward : string list ;
  meta_max_score : int option ;
}

let exercise_kind_enc =
  let open Json_encoding in
  string_enum
    [ "problem", Problem ;
      "project", Project ;
      "exercise", Learnocaml_exercise ]

let exercise_meta_enc_v1 =
  let open Json_encoding in
  obj2
    (req "kind" exercise_kind_enc)
    (req "stars" float)

let exercise_meta_enc_v2 =
  let open Json_encoding in
  obj9
     (opt "title" string)
     (opt "short_description" string)
     (opt "identifier" string)
     (opt "author" (list (tup2 string string)))
     (opt "focus" (list string))
     (opt "requirements" (list string))
     (opt "forward" (list string))
     (opt "backward" (list string))
     (opt "max_score" int)

let exercise_meta_enc =
  let open Json_encoding in
  check_version_2
    (merge_objs
       (merge_objs
          exercise_meta_enc_v1
          exercise_meta_enc_v2)
       unit) (* FIXME: temporary parameter, that allows unknown fields *)

let opt_to_list_enc = function
    None -> []
  | Some l -> l

let encoding =
  let open Json_encoding in
  conv
    (fun
      { meta_kind ; meta_stars ; meta_title ; meta_short_description;
        meta_identifier ; meta_author ; meta_focus ; meta_requirements ;
        meta_forward ; meta_backward ; meta_max_score ;
      } ->
      (((meta_kind, meta_stars),
        (meta_title, meta_short_description, meta_identifier,
         Some meta_author, Some meta_focus, Some meta_requirements,
         Some meta_forward, Some meta_backward, meta_max_score)),
       ()))
    (fun (((meta_kind, meta_stars),
           (meta_title, meta_short_description,
            meta_identifier,
            author, focus, requirements,
            forward, backward, meta_max_score)),
          _) ->
      { meta_kind ; meta_stars ;
        meta_title ;
        meta_short_description;
        meta_identifier ;
        meta_author = opt_to_list_enc author ;
        meta_focus = opt_to_list_enc focus ;
        meta_requirements = opt_to_list_enc requirements ;
        meta_forward = opt_to_list_enc forward ;
        meta_backward = opt_to_list_enc backward ;
        meta_max_score ;
      })
    exercise_meta_enc
