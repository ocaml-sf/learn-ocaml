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

val mkdir_p: ?perm:int -> string -> unit Lwt.t

(** [copy_tree src dst] copies the contents of directory [src] into directory
    [dst] *)
val copy_tree: string -> string -> unit Lwt.t


type 'a with_lock = { with_lock: 'b. 'a -> (unit -> 'b Lwt.t) -> 'b Lwt.t }

(** Creates a mutex hashtable when applied to [()], and returns a `with_mutex`
    function that can lock with a given mutex identifier *)
val gen_mutex_table: unit -> 'a with_lock
