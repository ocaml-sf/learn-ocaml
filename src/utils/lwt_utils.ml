(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Lwt.Infix

let is_directory path =
  Lwt.catch
    (fun () ->
      Lwt_unix.lstat path >|= function
      | Lwt_unix.{st_kind=S_DIR; _} -> true
      | _ -> false)
    (fun _exn -> Lwt.return_false)

let rec mkdir_p ?(perm=0o755) dir =
  Lwt_unix.file_exists dir >>= function
  | true ->
      is_directory dir >>= fun is_directory ->
      if is_directory then
        Lwt.return ()
      else
        Lwt.fail_with
          (Printf.sprintf "Can't create dir: file %s is in the way" dir)
  | false ->
      mkdir_p (Filename.dirname dir) >>= fun () ->
      Lwt_unix.mkdir dir perm

let copy_file src dst =
  Lwt.catch (fun () ->
      let cmd = [|"cp";src;dst|]in
      Lwt_process.exec ("", cmd) >>= fun r ->
      if r <> Unix.WEXITED 0 then Lwt.fail_with "copy_file"
      else Lwt.return_unit)
    (function
     | Sys_error _ | Unix.Unix_error _ -> Lwt.fail_with "copy_file"
     | e -> raise e)

let copy_tree src dst =
  let files = Sys.readdir src in
  if Array.length files = 0 then Lwt.return_unit
  else
    Lwt.catch (fun () ->
        mkdir_p dst >>= fun () ->
        let cmd =
          Array.concat
            [[|"cp"; "-PR"|];
             Array.map (Filename.concat src) files;
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
