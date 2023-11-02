(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2020 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Lwt
open Learnocaml_data
open Lwt.Syntax

let ( / ) dir f = if dir = "" then f else Filename.concat dir f
let sync_dir = "sync"
let indexes_subdir = "data"

let logfailwith str arg =
  Printf.printf "[ERROR] %s (%s)\n%!" str arg;
  failwith str

let generate_random_hex len =
  Cryptokit.Random.string Cryptokit.Random.secure_rng len
  |> Cryptokit.transform_string @@ Cryptokit.Hexa.encode ()

module type IndexKV  = functor (Store: Irmin.S) -> sig
  type token = Learnocaml_data.Token.t
  type t

  val parse : [> `O of (string * [> `String of 'a ]) list ] -> 'a
  val serialise : 'a -> [> `O of (string * [> `String of 'a ]) list ]
  val read :
    Store.path list -> (Store.contents -> 'a) -> string -> 'a list Lwt.t
  val write :
    Store.path list ->
    ('a -> Store.contents) -> 'a list -> string -> unit Lwt.t
  val create_index : string -> unit Lwt.t
  val exists : t
  val remove : t
end

module AUTH: IndexKV = struct

  module Store = Irmin_mem.KV.Make(Irmin.Contents.Json_value)
  module Info = Irmin_unix.Info(Store.Info)

  let read keys parse path=
    let config = Irmin_git.config ~bare:true path in
    let* repo = Store.Repo.v config in
    let* t = Store.main repo in
    Lwt_list.map_p
      (fun key ->
        let+ x = Store.get t key in parse x)
      keys

  let write keys serialise data_list path=
    let config = Irmin_git.config ~bare:true path in
    let* repo = Store.Repo.v config in
    let* t = Store.main repo in
    Lwt_list.iter_p
      (fun (key,data) ->
        Store.set_exn t ~info:(Info.v "message") key
          (*deal with the errors if using `set` instead of `set_exn`*)
          (serialise data))
    @@ List.combine keys data_list
end
