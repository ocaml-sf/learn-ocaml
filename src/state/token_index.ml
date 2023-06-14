(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2020 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Lwt

open Learnocaml_data

let ( / ) dir f = if dir = "" then f else Filename.concat dir f
let indexes_subdir = "data"

let logfailwith str arg =
  Printf.printf "[ERROR] %s (%s)\n%!" str arg;
  failwith str

let generate_random_hex len =
  Cryptokit.Random.string Cryptokit.Random.secure_rng len
  |> Cryptokit.transform_string @@ Cryptokit.Hexa.encode ()

module J = Json_encoding

module Json_codec = struct
  let decode enc s =
    (match s with
     | "" -> `O []
     | s -> Ezjsonm.from_string s)
    |> J.destruct enc

  let encode ?minify enc x =
    match J.construct enc x with
    | `A _ | `O _ as json -> Ezjsonm.to_string ?minify json
    | `Null -> ""
    | `Bool v -> string_of_bool v
    | _ -> assert false
end

module Store = Irmin_git_unix.FS.KV (Irmin.Contents.String)

module type IndexRW = sig
  val config : Irmin.config
  val read : string -> (string -> 'a) -> 'a Lwt.t
  val write : string -> ('a -> string) -> 'a -> unit Lwt.t
end

module IndexFile: IndexRW = struct
  let read keys parse =
    let* repo = Store.Repo.v config in
    let* t = Store.main repo in
    Lwt_list.map_p
      (fun key -> 
        parse @@ Store.get t key)
      keys

  let write keys serialise data_list =
    let* repo = Store.Repo.v config in
    let* t = Store.main repo in
    Lwt_list.iter_p
      (fun (key,data) ->
        Store.set_exn t ~info:(Irmin_git_unix.info ~author:"author" "message") key
          (serialise data))
      List.combine keys data_list
end

module BaseTokenIndex (RW: IndexRW) = struct
  let path = (sync_dir / indexes_subdir / "token")
  let config = Irmin_git.config ~bare:true path
  
  let file = "token.irmin"
  
  let parse token = token
  
  let serialise_str string = string
  let serialise = String.concat "-"

  let create_index sync_dir =
    let found_indexes =
      let rec scan f d acc =
        let rec aux s acc =
          Lwt.catch (fun () ->
              Lwt_stream.get s >>= function
              | Some ("." | ".." | "data") -> aux s acc
              | Some x -> scan f (d / x) acc >>= aux s
              | None -> Lwt.return acc)
          @@ function
          | Unix.Unix_error (Unix.ENOTDIR, _, _) -> f d acc
          | Unix.Unix_error _ -> Lwt.return acc
          | e -> Lwt.fail e
        in
        aux (Lwt_unix.files_of_directory (sync_dir / d)) acc
      in
      scan (fun d acc ->
          let d =
            if Filename.basename d = "save.json" then Filename.dirname d
            else d
          in
          let stok = String.map (function '/' | '\\' -> '-' | c -> c) d in
          if Token.check stok then
            Lwt.return (stok :: acc)
          else
            Lwt.return acc
        ) "" [] in
    Lwt_io.printl "[INFO] Regenerating the token index..." >>= fun () ->
    found_indexes >>= RW.write found_indexes serialise_str found_indexes

  let get_file sync_dir name =
    let tree = Store.get_tree t path in
    let entries = Store.Tree.list tree [] in
    let keys = List.map (fun (key,_) -> key) entries in
    let create () =
          create_index sync_dir >>= fun () ->
          RW.read keys parse in
    if Sys.file_exists path  then
      Lwt.catch
        (fun () -> RW.read keys parse)
        (fun _exn ->
           (* Note: this error handler may be adapted later to be more conservative?
              it does not matter now as sync/data/token.json is not a critical file, and
              can be regenerated. *)
           create ())
    else
      create ()

  let get_tokens sync_dir =
    get_file sync_dir file

  let add_token sync_dir token =
    get_tokens sync_dir >>= fun tokens ->
    if not (List.exists (fun found_token -> found_token = token) tokens) then
      RW.write path serialise (token :: tokens)
    else
      Lwt.return_unit
end

module TokenIndex = BaseTokenIndex (IndexFile)

