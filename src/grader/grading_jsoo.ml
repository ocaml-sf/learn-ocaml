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

exception Timeout

open Grader_jsoo_messages

let get_grade
    ?(callback = (fun _ -> ()))
    ?(timeout = infinity)
    exercise =
  let t, u = Lwt.task () in
  let worker = Worker.create "js/learnocaml-grader-worker.js" in
  Lwt.on_cancel t (fun () -> worker##terminate ()) ;
  let onmessage (ev : Json_repr_browser.Repr.value Worker.messageEvent Js.t) =
    let json = ev##data in
    begin match Json_repr_browser.Json_encoding.destruct from_worker_enc json with
      | Callback text -> callback text
      | Answer (report, stdout, stderr, outcomes) ->
          worker##terminate () ;
          Lwt.wakeup u (report, stdout, stderr, outcomes)
    end ;
    Js._true
  in
  worker##onmessage <- Dom.handler onmessage ;
  fun solution ->
    let req = { exercise ; solution } in
    let json = Json_repr_browser.Json_encoding.construct to_worker_enc req in
    worker##postMessage (json) ;
    let timer =
      Lwt.bind (Lwt_js.sleep timeout) @@ fun () ->
      worker##terminate () ;
      Lwt.fail Timeout in
    Lwt.pick [ timer ; t ]
