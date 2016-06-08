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
  Client_index.client_index *
  Learnocaml_toplevel_history.snapshot

let save_file_format =
  let open Json_encoding in
  (obj2
     (dft "exercises" Client_index.client_index_enc StringMap.empty)
     (dft "toplevel-history" Learnocaml_toplevel_history.snapshot_enc
        Learnocaml_toplevel_history.empty_snapshot))

let sync
    (index_a, snapshot_a)
    (index_b, snapshot_b) =
  let snapshot =
    let open Learnocaml_toplevel_history in
    if snapshot_a.mtime > snapshot_b.mtime then
      snapshot_a
    else
      snapshot_b in
  let index =
    let module StringMap = Map.Make (String) in
    let open Client_index in
    StringMap.merge
      (fun _id a b -> match a, b with
         | None, None -> assert false
         | None, Some state | Some state, None -> Some state
         | Some state_a, Some state_b ->
             if state_a.mtime > state_b.mtime then
               Some state_a
             else
               Some state_b)
      index_a index_b in
  (index, snapshot)
