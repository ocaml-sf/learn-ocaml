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

type 'a storage_key

val init : unit -> unit

val store : 'a storage_key -> 'a -> unit

val retrieve : 'a storage_key -> 'a

val delete : 'a storage_key -> unit

val listener : 'a storage_key -> ('a option -> unit) option ref

val cached_exercise : string -> Learnocaml_exercise.t storage_key

val exercise_state : string -> Learnocaml_exercise_state.exercise_state storage_key

val all_exercise_states : Learnocaml_exercise_state.exercise_state Map.Make (String).t storage_key

val exercise_toplevel_history : string -> Learnocaml_toplevel_history.snapshot storage_key

val all_exercise_toplevel_histories : Learnocaml_toplevel_history.snapshot Map.Make (String).t storage_key

val toplevel_history : string -> Learnocaml_toplevel_history.snapshot storage_key

val all_toplevel_histories : Learnocaml_toplevel_history.snapshot Map.Make (String).t storage_key

val sync_token : string storage_key
