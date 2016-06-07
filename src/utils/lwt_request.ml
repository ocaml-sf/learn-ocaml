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

open Js_utils

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let simple_fail msg code content =
  alert msg;
  Firebug.console##log (Js.string (Printf.sprintf "%s (%d: %S)" msg code content));
  Lwt.fail (Failure msg)

let log_failure (code, url, content) =
  Firebug.console##log(Js.string (Printf.sprintf "Request for %s failed. Code %d" url code));
  Firebug.console##log(Js.string (Printf.sprintf "Content: %s" content))


(** ... *)

exception Request_failed of (int * string * string)

let url_encode_list l =
  String.concat "&" (List.map (fun (name, arg) ->
      Printf.sprintf "%s=%s" name (Url.urlencode arg)) l)

let raw_get ?(headers=[]) ~url ~args =
  let (res, w) = Lwt.task () in
  let req = XmlHttpRequest.create () in
  let url = match args with
    | [] -> url
    | _ -> url ^ "?" ^ (url_encode_list args) in
  req##_open (Js.string "GET", Js.string url, Js._true);
  req##setRequestHeader (Js.string "Content-type",
			 Js.string "application/x-www-form-urlencoded");
  List.iter (fun (n, v) -> req##setRequestHeader (Js.string n, Js.string v))
    headers;
  let callback () =
    match req##status with
    | 0 | 200 -> Lwt.wakeup w (Js.to_string req##responseText)
    | 204 -> Lwt.wakeup w ""
    | code ->
      Lwt.wakeup_exn w
	(Request_failed (code, url, Js.to_string req##responseText))
  in
  req##onreadystatechange <- Js.wrap_callback
      (fun _ -> (match req##readyState with
	     XmlHttpRequest.DONE -> callback ()
	   | _ -> ()));
  req##send(Js.null);
  Lwt.on_cancel res (fun () -> req##abort ());
  res

let get_file path =
  Lwt.catch
    (fun () -> raw_get path [] >>= fun content -> Lwt.return (Some content))
    (fun _ -> Lwt.return None)

let raw_post ?(headers=[]) ?(get_args=[]) ~url ~body =
  let (res, w) = Lwt.task () in
  let req = XmlHttpRequest.create () in
  let url = match get_args with
    | [] -> url
    | _ -> url ^ "?" ^ (url_encode_list get_args) in
  req##_open (Js.string "POST", Js.string url, Js._true);
  req##setRequestHeader (Js.string "Content-type",
			 Js.string "application/x-www-form-urlencoded");
  List.iter (fun (n, v) -> req##setRequestHeader (Js.string n, Js.string v))
    headers;
  let callback () =
    match req##status with
    | 200 -> Lwt.wakeup w (Js.to_string req##responseText)
    | 204 -> Lwt.wakeup w ""
    | code ->
        Lwt.wakeup_exn w
	  (Request_failed (code, url, Js.to_string req##responseText))
  in
  req##onreadystatechange <- Js.wrap_callback
      (fun _ -> (match req##readyState with
	     XmlHttpRequest.DONE -> callback ()
	   | _ -> ()));
  let body = Js.Opt.map (Js.Opt.option body) Js.string in
  req##send(body);
  Lwt.on_cancel res (fun () -> req##abort ());
  res

(** Simple cache mechanism. *)

let get_preparsed_cached_data ?headers ?get_args ~parse ~url ~err () =
  let data = ref None in
  fun () ->
  match !data with
  | None ->
      let pending_request =
        let open XmlHttpRequest in
        perform_raw_url ?headers ?get_args url >>= fun { code; content } ->
        if code = 200 then
          Lwt.return (parse content)
        else begin
          data := None;
          err code content
        end in
      data := Some pending_request;
      pending_request
  | Some request -> request

let get_cached_data ?headers ?get_args ~url ~err () =
  get_preparsed_cached_data ?headers ?get_args ~parse:(fun id -> id) ~url ~err ()

let get_parametrized_cached_url
    (type a) ?(compare = Pervasives.compare) ~(url : a -> string) ~err () =
  let module Map = Map.Make(struct type t = a let compare = compare end) in
  let data = ref Map.empty in
  fun (id : a) ->
  try Map.find id !data
  with Not_found ->
    let open XmlHttpRequest in
    let pending_request =
      perform_raw_url (url id)
      >>= fun { code; content } ->
      if code = 200
      then Lwt.return content
      else begin
        data := Map.remove id !data;
        err id code content
      end
    in
    data := Map.add id pending_request !data;
    pending_request

(** Websockets *)

let make_preparsed_cached_websocket ~url ~parse =
  let scoreboard = ref None in
  let hooks = ref [] in
  let on_message
      (ws: WebSockets.webSocket Js.t)
      (ev : WebSockets.webSocket WebSockets.messageEvent Js.t) =
    let scores = parse (Js.to_string ev##data) in
    scoreboard := Some scores;
    List.iter (fun f -> Lwt.async (fun () -> f scores)) !hooks;
    Js._false in
  let ws = jsnew WebSockets.webSocket (Js.string url) in
  ws##onmessage <- Dom.handler (on_message ws);
  let add_hook f =
    hooks := f :: !hooks;
    match !scoreboard with
    | None -> ()
    | Some s -> Lwt.async (fun () -> f s) in
  let remove_hook f = hooks := List.filter (fun g -> f != g) !hooks in
  let get () =
    match !scoreboard with
    | None ->
        let (res, w) = Lwt.task () in
        let rec hook s = Lwt.wakeup w s; remove_hook hook; Lwt.return () in
        hooks := hook :: !hooks;
        res
    | Some s -> Lwt.return s in
  (get, add_hook)

