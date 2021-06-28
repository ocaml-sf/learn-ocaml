(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

val is_directory: string -> bool Lwt.t
val mkdir_p: ?perm:int -> string -> unit Lwt.t

(** [copy_tree src dst] copies the file [src] into file [dst] *)
val copy_file: string -> string -> unit Lwt.t

(** [copy_tree src dst] copies the contents of directory [src] into directory
    [dst] *)
val copy_tree: string -> string -> unit Lwt.t

type 'a with_lock = { with_lock: 'b. 'a -> (unit -> 'b Lwt.t) -> 'b Lwt.t }

(** Creates a mutex hashtable when applied to [()], and returns a `with_mutex`
    function that can lock with a given mutex identifier *)
val gen_mutex_table: unit -> 'a with_lock
