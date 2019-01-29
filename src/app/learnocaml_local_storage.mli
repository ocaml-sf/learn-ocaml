(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
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

type 'a storage_key

val init : unit -> unit

val clear : unit -> unit

val store : 'a storage_key -> 'a -> unit

val retrieve : 'a storage_key -> 'a

val delete : 'a storage_key -> unit

val listener : 'a storage_key -> ('a option -> unit) option ref

val cached_exercise : string -> Learnocaml_exercise.t storage_key

val exercise_list : string list storage_key

val exercise_state : string -> Answer.t storage_key

val all_exercise_states : Answer.t SMap.t storage_key

val exercise_toplevel_history : string -> Learnocaml_toplevel_history.snapshot storage_key

val exercise_toplevel_history_list : string list storage_key

val all_exercise_toplevel_histories : Learnocaml_toplevel_history.snapshot SMap.t storage_key

val toplevel_history_list : string list storage_key

val toplevel_history : string -> Learnocaml_toplevel_history.snapshot storage_key

val all_toplevel_histories : Learnocaml_toplevel_history.snapshot SMap.t storage_key

val sync_token : Token.t storage_key

val nickname : string storage_key
