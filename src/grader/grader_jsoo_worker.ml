(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml

let get_grade ?callback exo solution =
  (* the new toplevel uses directory listings to discover .cmis, so the old
     approach of using [Sys_js.mount] for subpaths of individual files no longer
     works: we need to mount everything explicitely. *)
  (* FIXME: copied from learnocaml_toplevel_worker_main.ml *)
  let rec rec_mount path = function
    | OCamlRes.Res.Dir (name, children) ->
        List.iter (rec_mount (name::path)) children
    | OCamlRes.Res.File (name, content) ->
        let name = "/" ^ String.concat "/" (List.rev (name::path)) in
        Js.Unsafe.set content (Js.string "t") 9 ; (* XXX hack *)
        Sys_js.create_file ~name ~content
    | OCamlRes.Res.Error _ -> ()
  in
  rec_mount [] (OCamlRes.Res.Dir ("worker_cmis", Embedded_cmis.root));
  (try Toploop_jsoo.initialize ["/worker_cmis"; "/grading_cmis"] with
   | Typetexp.Error (loc, env, error) ->
       Js_utils.log "FAILED INIT %a at %a"
         (Typetexp.report_error env) error
         Location.print_loc loc
   | e ->
       Js_utils.log "FAILED INIT %s" (Printexc.to_string e));
  let divert name chan cb =
    let redirection = Toploop_jsoo.redirect_channel name chan cb in
    fun () -> Toploop_jsoo.stop_channel_redirection redirection in
  let load_code compiled_code =
    try
      Toploop_jsoo.use_compiled_string compiled_code.Learnocaml_exercise.js;
      flush_all ();
      Toploop_ext.Ok (true, [])
    with exn ->
      prerr_endline (Printexc.to_string exn);
      Toploop_ext.Ok (false, [])
  in
  Grading.get_grade ?callback ~divert ~load_code exo solution

open Grader_jsoo_messages

let () =
  (match Js_utils.get_lang() with Some l -> Ocplib_i18n.set_lang l | None -> ());
  Worker.set_onmessage @@ fun (json : Json_repr_browser.Repr.value) ->
  let { exercise ; solution } =
    Json_repr_browser.Json_encoding.destruct to_worker_enc json in
  let callback msg =
    let msg = Callback msg in
    let json = Json_repr_browser.Json_encoding.construct from_worker_enc msg in
    Worker.post_message json in
  let ans =
    match get_grade ~callback exercise solution with
    | Ok report, stdout, stderr, outcomes ->
        Answer (report, stdout, stderr, outcomes)
    | Error err, stdout, stderr, outcomes ->
        let msg = match err with
          | Grading.User_code_error err ->
              Format.asprintf [%if"Error in your solution:\n%a\n%!"]
                Location.print_report (Toploop_results.to_error err)
          | Grading.Internal_error (step, err) ->
              Format.asprintf [%if"Error in the exercise %s\n%a\n%!"]
                step
                Location.print_report (Toploop_results.to_error err)
          | Grading.Invalid_grader ->
              [%i"Internal error:\nThe grader did not return a report."] in
        let report = Learnocaml_report.[ Message ([ Code msg ], Failure) ] in
        Answer (report, stdout, stderr, outcomes)
    | exception exn ->
        let bt = Printexc.get_backtrace () in
        let open Learnocaml_report in
        let report =
          section ~title:"Internal error" [
            failure ~message:"The grader crashed";
            message ~message:(Printexc.to_string exn);
            info ~message:bt;
          ] in
        Answer ([report], "", "", "")
  in
  let json = Json_repr_browser.Json_encoding.construct from_worker_enc ans in
  Worker.post_message json
