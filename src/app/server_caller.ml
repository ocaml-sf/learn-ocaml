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

open Lwt.Infix

(*
let cannot_fetch msg = Lwt.fail (Cannot_fetch msg)

let fetch ?message filename =
  Lwt.catch
    (fun () -> Lwt_request.get filename [])
    (fun exn ->
       let message = match message with
         | None -> "cannot retrieve " ^ filename
         | Some message -> message in
       let msg = match exn with
         | Lwt_request.Request_failed (0, _) ->
             Printf.sprintf "%s (server unreachable)"
               message
         | Lwt_request.Request_failed (code, _) ->
             Printf.sprintf "%s (code %d)"
               message code
         | exn ->
             Printf.sprintf "%s\n%s"
               message (Printexc.to_string exn) in
       Lwt.fail (Cannot_fetch msg))
*)
module Json_codec = struct

  let decode enc s =
    Js._JSON##(parse (Js.string s)) |>
    Json_repr_browser.Json_encoding.destruct enc

  let encode enc x =
    let json = Json_repr_browser.Json_encoding.construct enc x in
    Js.to_string Js._JSON##(stringify json)

end

module Api_client = Learnocaml_api.Client (Json_codec)

type request_error = [
  | `Unreachable of string
  | `Not_found of string
  | `Http_error of int * string
  | `Exception of exn
  | `Invalid_response of exn
]

let string_of_error = function
  | `Unreachable s -> "Server unreachable: " ^ s
  | `Not_found s -> "URL not found: " ^ s
  | `Http_error (code, s) ->
      Printf.sprintf "HTTP error (%d): %s" code s
  | `Exception e ->
      "Server request failed: " ^ Printexc.to_string e
  | `Invalid_response e ->
      "Could not decode server response: " ^ Printexc.to_string e

let request req =
  let do_req = function
    | { Learnocaml_api.meth = `GET; path } ->
        Lwt_request.get ?headers:None ~url:(String.concat "/" path) ~args:[]
    | { Learnocaml_api.meth = `POST body; path } ->
        Lwt_request.post ?headers:None ?get_args:None
          ~url:(String.concat "/" path) ~body:(Some body)
  in
  Lwt.catch (fun () ->
      Api_client.make_request (fun http_request ->
          Lwt.catch (fun () -> do_req http_request >|= fun body -> Ok (body))
          @@ function
          | Lwt_request.Request_failed (0, s) ->
              Lwt.return (Error (`Unreachable s))
          | Lwt_request.Request_failed (404, s) ->
              Lwt.return (Error (`Not_found s))
          | Lwt_request.Request_failed (code, s) ->
              Lwt.return (Error (`Http_error (code, s)))
          | e ->
              Lwt.return (Error (`Exception e)))
        req)
  @@ fun e ->
  Lwt.return (Error (`Invalid_response e))

exception Cannot_fetch of string

let request_exn req =
  request req >>= function
  | Ok x -> Lwt.return x
  | Error e ->
      Lwt.fail (Cannot_fetch (string_of_error e))

(* FIXME: define proper API call *)
let fetch_lesson_index () =
  request_exn (Learnocaml_api.Static_json
                 (Learnocaml_index.lesson_index_path, Learnocaml_index.lesson_index_enc))

(* FIXME: define proper API call *)
let fetch_lesson id =
  request_exn (Learnocaml_api.Static_json
                 (Learnocaml_index.lesson_path id, Learnocaml_lesson.lesson_enc))

let fetch_exercise id =
  request_exn (Learnocaml_api.Static_json
                 (Learnocaml_index.exercise_path id, Learnocaml_exercise.enc))

let fetch_tutorial_index () =
  request_exn (Learnocaml_api.Static_json
                 (Learnocaml_index.tutorial_index_path, Learnocaml_index.tutorial_index_enc))

let fetch_tutorial id =
  request_exn (Learnocaml_api.Static_json
                 (Learnocaml_index.tutorial_path id, Learnocaml_tutorial.tutorial_enc))

(*
let fetch_json filename =
  fetch filename >>= fun text ->
  try Lwt.return (Js._JSON##(parse (Js.string text))) with Js.Error err ->
    let msg =
      Format.asprintf "bad format for %s\n%s"
        filename (Js.to_string err ##. message) in
    Lwt.fail (Cannot_fetch msg)

let fetch_and_decode_json enc filename =
  fetch_json filename >>= fun json ->
  try Lwt.return (Json_repr_browser.Json_encoding.destruct enc json) with exn ->
    let msg =
      Format.asprintf "bad structure for %s@.%a"
        filename
        (fun ppf -> Json_encoding.print_error ppf) exn in
    Lwt.fail (Cannot_fetch msg)

let fetch_exercise_index () =
  fetch_and_decode_json
    Learnocaml_index.exercise_index_enc
    Learnocaml_index.exercise_index_path

let fetch_exercise id =
  fetch_and_decode_json
    Learnocaml_exercise.enc
    (Learnocaml_index.exercise_path id)

let fetch_lesson_index () =
  fetch_and_decode_json
    Learnocaml_index.lesson_index_enc
    Learnocaml_index.lesson_index_path

let fetch_lesson id =
  fetch_and_decode_json
    Learnocaml_lesson.lesson_enc
    (Learnocaml_index.lesson_path id)

let fetch_tutorial_index () =
  fetch_and_decode_json
    Learnocaml_index.tutorial_index_enc
    Learnocaml_index.tutorial_index_path

let fetch_tutorial id =
  fetch_and_decode_json
    Learnocaml_tutorial.tutorial_enc
    (Learnocaml_index.tutorial_path id)

let gimme_sync_token () =
  fetch ~message: "cannot obtain a token" "/sync/gimme"

let fetch_save_file ~token =
  let message = "cannot download server data" in
  fetch ~message ("/sync/" ^ token) >>= function
  | "" -> Lwt.return_none
  | text ->
      (try Lwt.return (Js._JSON##(parse (Js.string text)))
       with Js.Error err ->
         let msg =
           Format.asprintf "bad format for server data\n%s"
             (Js.to_string err ##. message) in
         Lwt.fail (Cannot_fetch msg)) >>= fun json ->
      try Lwt.return_some @@
        Json_repr_browser.Json_encoding.destruct Learnocaml_sync.save_file_enc json
      with exn ->
        let msg =
          Format.asprintf "bad structure for server data@.%a"
            (fun ppf -> Json_encoding.print_error ppf) exn in
        Lwt.fail (Cannot_fetch msg)

let upload_save_file ~token save_file =
  let json =
    Json_repr_browser.Json_encoding.construct
      Learnocaml_sync.save_file_enc
      save_file in
  let body =
    Some (Js.to_string (Js._JSON##(stringify json))) in
  Lwt.catch
    (fun () -> Lwt_request.post ~headers: [] ~get_args: []
        ~url: ("/sync/" ^ token) ~body)
    (fun exn ->
       let message = "cannot upload data to the server" in
       let msg = match exn with
         | Lwt_request.Request_failed (0, _) ->
             Printf.sprintf "%s (server unreachable)"
               message
         | Lwt_request.Request_failed (code, _) ->
             Printf.sprintf "%s (code %d)"
               message code
         | exn ->
             Printf.sprintf "%s\n%s"
               message (Printexc.to_string exn) in
       Lwt.fail (Cannot_fetch msg)) >>= fun _ ->
  Lwt.return ()

let fetch filename =
  (* erase message argument in the interface *)
  fetch filename
*)
