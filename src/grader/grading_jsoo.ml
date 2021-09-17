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
open Js_of_ocaml_lwt

let get_grade
    ?(worker_js_file = "/js/learnocaml-grader-worker.js")
    ?(callback = (fun _ -> ()))
    ?(timeout = infinity)
    exercise =
  let t, u = Lwt.task () in
  let worker = Worker.create worker_js_file in
  Lwt.on_cancel t (fun () ->
      Js_utils.js_warn "Grading worker END";
      worker##terminate) ;
  let onmessage (ev : Json_repr_browser.Repr.value Worker.messageEvent Js.t) =
    let json = ev##.data in
    Js_utils.js_warn ("msg from grading worker:");
    Js_utils.js_warn json;
    begin match Json_repr_browser.Json_encoding.destruct from_worker_enc json with
      | Callback text -> callback text
      | Answer (report, stdout, stderr, outcomes) ->
          worker##terminate ;
          Lwt.wakeup u (report, stdout, stderr, outcomes)
    end ;
    Js._true
  in
  worker##.onmessage := Dom.handler onmessage ;
  let onerror (ev : Worker.errorEvent Js.t) =
    worker##terminate ;
    let msg =
      let open Learnocaml_report in
      section ~title:"INTERNAL ERROR" [
        failure ~message:"The grader crashed";
        message ~message:(Js.to_string ev##.message);
      ] in
    Lwt.wakeup u ([msg], "", "", "") ;
    Js._true
  in
  worker##.onerror := Dom.handler onerror ;
  Lwt.return @@
  fun solution ->
    let req = { exercise ; solution } in
    let json = Json_repr_browser.Json_encoding.construct to_worker_enc req in
    Js_utils.js_warn ("Sending to grading worker: ");
    Js_utils.js_warn json;
    worker##(postMessage json) ;
    let timer =
      Lwt_js.sleep timeout >>= fun () ->
      worker##terminate ;
      Lwt.fail Timeout in
    Lwt.pick [ timer ; t ]
