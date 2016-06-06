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

open Lwt

let map_opt f = function
  | None -> None
  | Some x -> Some (f x)
let iter_opt f = function
  | None -> ()
  | Some x -> f x

type redirection =
  { channel : out_channel ;
    target_fd : Unix.file_descr ;
    backup_fd : Unix.file_descr ;
    read_fd : Unix.file_descr ;
    append : string -> unit }

let redirections = ref []

let redirect_channel ?tee name channel append =
  flush channel ;
  let append data =
    let tee = map_opt (fun tee -> tee name) tee in
    iter_opt (fun tee -> tee data) tee ;
    append data in
  let target_fd = Unix.descr_of_out_channel channel in
  let backup_fd = Unix.dup target_fd in
  let stack = try List.assq target_fd !redirections with Not_found -> [] in
  let read_fd, write_fd = Unix.pipe () in
  Unix.dup2 write_fd target_fd ;
  Unix.set_nonblock read_fd ;
  let redirected_channel =
    { target_fd ; backup_fd ; read_fd ; append ; channel } in
  redirections :=
    List.filter (fun (fd, _) -> fd != target_fd) !redirections ;
  redirections :=
    (target_fd, redirected_channel :: stack) :: !redirections ;
  redirected_channel

let flush_redirected_channel { read_fd ; append ; channel } =
  let buf = Bytes.create 503 in
  let rec loop () =
    let len = Unix.read read_fd buf 0 (Bytes.length buf) in
    let data = Bytes.sub_string buf 0 len in
    append data ;
    loop () in
  flush channel ; try loop () with _ -> ()

let stop_channel_redirection ({ target_fd ; read_fd ; backup_fd } as redirection) =
  let fail () = invalid_arg "Toploop_unix.stop_channel_redirection" in
  match List.assq target_fd !redirections with
  | exception Not_found -> fail ()
  | [] -> fail ()
  | redirection' :: rest ->
      if redirection' != redirection then fail () ;
      flush_redirected_channel redirection ;
      Unix.dup2 backup_fd target_fd ;
      Unix.close read_fd ;
      redirections :=
        List.filter (fun (fd, _) -> fd != target_fd) !redirections ;
      if rest <> [] then
        redirections := (target_fd, rest) :: !redirections

let initialize () =
  Toploop.initialize_toplevel_env ()
