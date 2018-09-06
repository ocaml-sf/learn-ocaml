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

open Learnocaml_data

let common_skills l1 l2 =
  List.fold_left (fun acc skill ->
      if List.mem skill l2 then skill :: acc
      else acc) [] l2

let compare (e1_id, e1_meta) (e2_id, e2_meta) =
  let open Exercise in
  let e1_required =
    common_skills e1_meta.Meta.focus e2_meta.Meta.requirements in
  let e2_required =
    common_skills e2_meta.Meta.focus e1_meta.Meta.requirements in
  if e1_required = [] then -1
  else if e2_required = [] then 1
  else if
    List.mem e1_id e2_meta.Meta.backward ||
    List.mem e2_id e1_meta.Meta.forward then -1
  else if
    List.mem e2_id e1_meta.Meta.backward ||
    List.mem e1_id e1_meta.Meta.backward then 1
  else 0

type kind = Skill of string | Backward of string | Forward of string

type node =
  Node of Exercise.id * (node * kind) list


let compute_skill_tree ?(depth = 2) skill exs (focus, requirements) =
  let ex_seen = ref SSet.empty in
  let skill_seen = ref SSet.empty in
  let rec compute_exercises depth skill =
    if not @@ SSet.mem skill !skill_seen then
      begin
        skill_seen := SSet.add skill !skill_seen;
        let exs_focus = SMap.find skill focus in
        List.fold_left
          (fun acc e ->
             match compute_skills depth e with
               Some n -> (n, Skill skill) :: acc
             | None -> acc)
          [] exs_focus
      end
    else []
  and compute_skills depth ex =
    if not @@ SSet.mem ex !ex_seen && depth > 0 then
      begin
        ex_seen := SSet.add ex !ex_seen;
        try let meta = List.assoc ex exs in
          let req = meta.Exercise.Meta.requirements in
          let deps =
            List.fold_left
              (fun acc s -> compute_exercises (depth-1) s :: acc) [] req
          |> List.flatten
          in
          Some (Node (ex, deps))
        with Not_found -> Some (Node (ex, []))
      end
    else None
  in
  compute_exercises depth skill
