(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2022 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

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
  Unix.close write_fd ;
  Unix.set_nonblock read_fd ;
  let redirected_channel =
    { target_fd ; backup_fd ; read_fd ; append ; channel } in
  redirections :=
    List.filter (fun (fd, _) -> fd != target_fd) !redirections ;
  redirections :=
    (target_fd, redirected_channel :: stack) :: !redirections ;
  redirected_channel

let flush_redirected_channel { read_fd ; append ; channel ; _ } =
  let buf = Bytes.create 503 in
  let rec loop () =
    let len = Unix.read read_fd buf 0 (Bytes.length buf) in
    let data = Bytes.sub_string buf 0 len in
    append data ;
    loop () in
  flush channel ; try loop () with _ -> ()

let stop_channel_redirection ({ target_fd ; read_fd ; backup_fd ; _ } as redirection) =
  let fail () = invalid_arg "Toploop_unix.stop_channel_redirection" in
  match List.assq target_fd !redirections with
  | exception Not_found -> fail ()
  | [] -> fail ()
  | redirection' :: rest ->
      if redirection' != redirection then fail () ;
      flush_redirected_channel redirection ;
      Unix.dup2 backup_fd target_fd ;
      Unix.close backup_fd ;
      Unix.close read_fd ;
      redirections :=
        List.filter (fun (fd, _) -> fd != target_fd) !redirections ;
      if rest <> [] then
        redirections := (target_fd, rest) :: !redirections

let initialize () =
  Toploop.initialize_toplevel_env ()

let use_compiled_string code =
  let cma = Filename.temp_file "learnocaml-file" ".cma" in
  let r =
    try
      let oc = open_out_bin cma in
      output_string oc code;
      close_out oc;
      Topdirs.load_file Format.std_formatter cma
    with
    | Symtable.Error e ->
        Format.kasprintf (fun msg -> Sys.remove cma; failwith msg)
          "%a"
          Symtable.report_error e
    | exn ->
        Sys.remove cma;
        raise exn
  in
  Sys.remove cma;
  Toploop_ext.register_pending_printers ();
  flush_all ();
  if r then ()
  else failwith "Failed to load compiled code"
