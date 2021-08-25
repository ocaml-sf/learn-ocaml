(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Learnocaml_process_common
open Learnocaml_data
open Lwt.Infix

let playground_dir = ref "./playground"

let playground_index = ref None

let errored exn =
  let print_unknown ppf = function
    | Failure msg -> Format.fprintf ppf "Cannot process exercises: %s" msg
    | exn ->
        Format.fprintf ppf "Cannot process exercises: %s"
          (Printexc.to_string exn)
  in
  Json_encoding.print_error ~print_unknown Format.err_formatter exn;
  Format.eprintf "@.";
  Lwt.return false

let auto_index path =
  let entries = Sys.readdir path in
  Array.sort compare entries;
  Array.fold_left
    (fun acc id ->
      let f = path / id in
      let full_id =
        String.sub f
          (String.length !playground_dir + 1)
          (String.length f - String.length !playground_dir - 1)
      in
      if Sys.file_exists (f / "template.ml") then
        let elem = (full_id, Playground.Meta.default full_id) in
        match acc with None -> Some [elem] | Some xs -> Some (elem :: xs)
      else acc )
    None entries

let get_structure exercises_index =
  if Sys.file_exists exercises_index then
    from_file Playground.Index.enc exercises_index
  else if Sys.file_exists !playground_dir then (
    match auto_index !playground_dir with
    | None -> Lwt.fail_with "Missing index file and malformed repository"
    | Some i ->
        Format.eprintf "Missing index file, using all exercise directories.@.";
        Lwt.return i )
  else (
    Format.eprintf "No index file, no exercise directory.@.";
    Format.eprintf
      "This does not look like a LearnOCaml exercise repository.@.";
    Lwt.fail (Failure "cannot continue") )

let fill_structure =
  let open Playground in
  Lwt_list.map_s
  @@ fun (id, _) ->
  Lwt_io.(with_file ~mode:Input (!playground_dir / id / "template.ml") read)
  >>= fun template ->
  let preludeml = !playground_dir / id / "prelude.ml" in
  ( if Sys.file_exists preludeml then
    Lwt_io.(with_file ~mode:Input preludeml read)
  else Lwt.return "" )
  >|= fun prelude -> {id; template; prelude}

let write_structure dest_dir =
  let open Playground in
  Lwt_list.iter_s
  @@ fun x -> to_file enc (dest_dir / Learnocaml_index.playground_path x.id) x

let write_index dest_dir =
  to_file Playground.Index.enc
    (dest_dir / Learnocaml_index.playground_index_path)

let catched playground_index dest_dir =
  get_structure playground_index
  >>= fun structure ->
  fill_structure structure >>= write_structure dest_dir
  >>= fun () -> write_index dest_dir structure >|= fun () -> true

let main dest_dir =
  let playground_index =
    match !playground_index with
    | Some playground_index -> playground_index
    | None -> !playground_dir / "index.json"
  in
  let playground_dest_dir = dest_dir / Learnocaml_index.playground_dir in
  Lwt_utils.mkdir_p playground_dest_dir
  >>= fun () -> Lwt.catch (fun () -> catched playground_index dest_dir) errored
