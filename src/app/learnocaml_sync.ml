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

type save_file =
  { all_exercise_states :
      Learnocaml_exercise_state.exercise_state Map.Make (String).t  ;
    all_toplevel_histories :
      Learnocaml_toplevel_history.snapshot Map.Make (String).t ;
    all_exercise_toplevel_histories :
      Learnocaml_toplevel_history.snapshot Map.Make (String).t }

let map_enc enc =
  let open Json_encoding in
  conv
    StringMap.bindings
    (List.fold_left
       (fun acc (n, v) -> StringMap.add n v acc)
       StringMap.empty)
    (assoc enc)

let save_file_format =
  let open Json_encoding in
  conv
    (fun { all_exercise_states ;
           all_toplevel_histories ;
           all_exercise_toplevel_histories } ->
      (all_exercise_states,
       all_toplevel_histories,
       all_exercise_toplevel_histories))
    (fun (all_exercise_states,
          all_toplevel_histories,
          all_exercise_toplevel_histories) ->
      { all_exercise_states ;
        all_toplevel_histories ;
        all_exercise_toplevel_histories }) @@
  (obj3
     (dft "exercises" (map_enc Learnocaml_exercise_state.exercise_state_enc) StringMap.empty)
     (dft "toplevel-histories" (map_enc Learnocaml_toplevel_history.snapshot_enc) StringMap.empty)
     (dft "exercise-toplevel-histories" (map_enc Learnocaml_toplevel_history.snapshot_enc) StringMap.empty))

let sync
    { all_exercise_states = all_exercise_states_a ;
      all_toplevel_histories = all_toplevel_histories_a ;
      all_exercise_toplevel_histories = all_exercise_toplevel_histories_a }
    { all_exercise_states = all_exercise_states_b ;
      all_toplevel_histories = all_toplevel_histories_b ;
      all_exercise_toplevel_histories = all_exercise_toplevel_histories_b } =
  let sync_snapshot snapshot_a snapshot_b =
    let open Learnocaml_toplevel_history in
    if snapshot_a.mtime > snapshot_b.mtime then
      snapshot_a
    else
      snapshot_b in
  let sync_exercise_state state_a state_b =
    let open Learnocaml_exercise_state in
    if state_a.mtime > state_b.mtime then
      state_a
    else
      state_b in
  let sync_map sync_item index_a index_b =
    let open Learnocaml_exercise_state in
    StringMap.merge
      (fun _id a b -> match a, b with
         | None, None -> assert false
         | None, Some i | Some i, None -> Some i
         | Some a, Some b -> Some (sync_item a b))
      index_a index_b in
  { all_exercise_states =
    sync_map sync_exercise_state
      all_exercise_states_a
      all_exercise_states_b ;
    all_toplevel_histories =
    sync_map sync_snapshot
      all_toplevel_histories_a
      all_toplevel_histories_b ;
    all_exercise_toplevel_histories =
    sync_map sync_snapshot
      all_exercise_toplevel_histories_a
      all_exercise_toplevel_histories_b }
