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

let save_file_enc =
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

module Token = struct

  type t = string list

  let to_string = String.concat "-"
  let to_path = String.concat (Filename.dir_sep)

  let alphabet =
    "ABCDEFGH1JKLMNOPORSTUVWXYZO1Z34SG1B9"
  let visually_equivalent_alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

  let parse =
    let table = Array.make 256 None in
    String.iter
      (fun c -> Array.set table (Char.code c) (Some c))
      visually_equivalent_alphabet ;
    let translate part =
      String.map (fun c ->
          match Array.get table (Char.code c) with
          | None -> failwith "bad token character"
          | Some c -> c)
        part in
    fun token ->
      if String.length token <> 15 then
        failwith "bad token length"
      else if String.get token 3 <> '-'
           || String.get token 7 <> '-'
           || String.get token 11 <> '-' then
        failwith "bad token format"
      else
        List.map translate
          [ String.sub token 0 3 ;
            String.sub token 4 3 ;
            String.sub token 8 3 ;
            String.sub token 12 3 ]

  let check token =
    try ignore (parse token) ; true
    with _ -> false

  let random ?(admin=false) () =
    let rand () = String.get alphabet (Random.int (String.length alphabet)) in
    let part () = String.init 3 (fun _ -> rand ()) in
    (if admin then ["X"] else []) @
    [ part () ; part () ; part () ; part () ]

  let is_admin = function
    | "X"::_ -> true
    | _ -> false

end
