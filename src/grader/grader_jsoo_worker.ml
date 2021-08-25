(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml

let get_grade ?callback exo solution =
  let path = "/grading_cmis" in
  let root =
    OCamlRes.Res.merge Embedded_cmis.root Embedded_grading_cmis.root
  in
  Sys_js.mount ~path (fun ~prefix:_ ~path ->
      match OCamlRes.Res.find (OCamlRes.Path.of_string path) root with
      | cmi ->
          Js.Unsafe.set cmi (Js.string "t") 9;
          (* XXX hack *)
          Some cmi
      | exception Not_found -> None );
  Config.load_path := [path];
  Toploop_jsoo.initialize ();
  let divert name chan cb =
    let redirection = Toploop_jsoo.redirect_channel name chan cb in
    fun () -> Toploop_jsoo.stop_channel_redirection redirection
  in
  Grading.get_grade ?callback ~divert exo solution

open Grader_jsoo_messages

let () =
  ( match Js_utils.get_lang () with
  | Some l -> Ocplib_i18n.set_lang l
  | None -> () );
  Worker.set_onmessage
  @@ fun (json : Json_repr_browser.Repr.value) ->
  let {exercise; solution} =
    Json_repr_browser.Json_encoding.destruct to_worker_enc json
  in
  let callback msg =
    let msg = Callback msg in
    let json = Json_repr_browser.Json_encoding.construct from_worker_enc msg in
    Worker.post_message json
  in
  let ans =
    let result, stdout, stderr, outcomes =
      get_grade ~callback exercise solution
    in
    match result with
    | Ok report -> Answer (report, stdout, stderr, outcomes)
    | Error exn ->
        let msg =
          match exn with
          | Grading.User_code_error {Toploop_results.msg; _} ->
              [%i "Error in your solution:\n"] ^ msg
          | Grading.Internal_error (step, {Toploop_results.msg; _}) ->
              [%i "Error in the exercise "] ^ step ^ "\n" ^ msg
          | Grading.Invalid_grader ->
              [%i "Internal error:\nThe grader did not return a report."]
          | exn -> [%i "Unexpected error:\n"] ^ Printexc.to_string exn
        in
        let report = Learnocaml_report.[Message ([Code msg], Failure)] in
        Answer (report, stdout, stderr, outcomes)
  in
  let json = Json_repr_browser.Json_encoding.construct from_worker_enc ans in
  Worker.post_message json
