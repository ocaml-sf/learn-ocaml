(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

exception Timeout

open Grader_jsoo_messages
open Lwt.Infix
open Js_of_ocaml

let get_grade ?(worker_js_file = "/js/learnocaml-grader-worker.js")
    ?(callback = fun _ -> ()) ?(timeout = infinity) exercise =
  let t, u = Lwt.task () in
  let worker = Worker.create worker_js_file in
  Lwt.on_cancel t (fun () -> worker##terminate);
  let onmessage (ev : Json_repr_browser.Repr.value Worker.messageEvent Js.t) =
    let json = ev##.data in
    ( match Json_repr_browser.Json_encoding.destruct from_worker_enc json with
    | Callback text -> callback text
    | Answer (report, stdout, stderr, outcomes) ->
        worker##terminate;
        Lwt.wakeup u (report, stdout, stderr, outcomes) );
    Js._true
  in
  worker##.onmessage := Dom.handler onmessage;
  Lwt.return
  @@ fun solution ->
  let req = {exercise; solution} in
  let json = Json_repr_browser.Json_encoding.construct to_worker_enc req in
  worker ## (postMessage json);
  let timer =
    Lwt_js.sleep timeout >>= fun () -> worker##terminate; Lwt.fail Timeout
  in
  Lwt.pick [timer; t]
