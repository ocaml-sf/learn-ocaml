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

module Exercise = struct

  type id = string

  type tag = string

  type t = {
    id: id;
    path: string list;
    meta: Learnocaml_index.exercise;
    tags: tag list;
  }

  module Map = Map.Make(struct
      type key = id
      let compare = compare
    end)

  let all: (id, t) Hashtbl.t = Hashtbl.create 223

  let load exercise_index_file =
    let ic = open_in exercise_index_file in
    let json = Ezjsonm.from_channel ic in
    close_in ic;
    let index =
      Json_encoding.destruct Learnocaml_index.exercise_index_enc json
    in
    let rec register path = function
      | Groups groups ->
          List.iter (fun (group_name, { _group_title; group_contents }) ->
              register (path @ [group_name]) group_contents)
            groups
      | Learnocaml_exercises exos ->
          List.iter (fun (name, exercise) ->
              Hashtbl.add all id {
                id;
                path;
                meta = exercise;
                tags = [];
              })
            exos
    in
    register [] index

  let select ?(filter=None) ?(sort=None) () =
    Hashtbl.fold (fun _id ex acc -> ex::acc) all []

end

module Student: sig

  type tag = string

  type token = Token.t

  type t = {
    token: token;
    nickname: string;
    exercises: Learnocaml_exercise_state.exercise_state Exercise.SMap.t;
    tags: tag list;
  }

  module Set = Set.Make(struct
      type key = token
      let compare = compare
    end)

  let all: (token, t) Hashtbl.t = Hashtbl.create 223
end

module Assignment: sig

  type t = {
    exercises: Exercise.id list;
    start_date: float option;
    end_date: float option;
    is_open: bool;
    students: Student.Set.t;
  }

  module Set = Set.S with type elt = t

  let all = ref Set.empty
end

module Teacher: sig

  type token = Token.t

  type t = {
    token: token;
    nickname: string;
    students: Student.set;
  }

  let all: (token, t) Hashtbl.t = Hashtbl.create 71
end

let init ~exercise_index =
  Exercise.load exercise_index
