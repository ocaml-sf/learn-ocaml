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

open Lwt.Infix

let rec mkdir_p ?(perm=0o755) dir =
  Lwt_unix.file_exists dir >>= function
  | true ->
      if Sys.is_directory dir then
        Lwt.return ()
      else
        Lwt.fail_with
          (Printf.sprintf "Can't create dir: file %s is in the way" dir)
  | false ->
      mkdir_p (Filename.dirname dir) >>= fun () ->
      Lwt_unix.mkdir dir perm

let copy_tree src dst =
  Lwt.catch (fun () ->
      mkdir_p dst >>= fun () ->
      let cmd =
        Array.concat
          [[|"cp"; "-PR"|];
           Array.map (Filename.concat src) (Sys.readdir src);
           [|dst|]]
      in
      Lwt_process.exec ("", cmd) >>= fun r ->
      if r <> Unix.WEXITED 0 then Lwt.fail_with "copy_tree"
      else Lwt.return_unit)
    (function
      | Sys_error _ | Unix.Unix_error _ -> Lwt.fail_with "copy_tree"
      | e -> raise e)

type 'a with_lock = { with_lock: 'b. 'a -> (unit -> 'b Lwt.t) -> 'b Lwt.t }

let gen_mutex_table: type t. unit -> t with_lock = fun () ->
  let table = Hashtbl.create 223 in
  let get_mutex key =
    try Hashtbl.find table key with Not_found ->
      let mutex = Lwt_mutex.create () in
      Hashtbl.add table key mutex;
      mutex
  in
  let with_lock key f =
    let mutex = get_mutex key in
    Lwt_mutex.with_lock mutex @@ fun () ->
    Lwt.finalize f @@ fun () ->
    if Lwt_mutex.is_empty mutex then
      (* we still hold the mutex, nobody else is waiting: drop it *)
      Hashtbl.remove table key;
    Lwt.return_unit
  in
  { with_lock }
