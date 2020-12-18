(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Js_of_ocaml

exception Request_failed of (int * string)

let url_encode_list l =
  String.concat "&" (List.map (fun (name, arg) ->
      Printf.sprintf "%s=%s" name (Url.urlencode arg)) l)

let get ?(headers=[]) ~url ~args =
  let (res, w) = Lwt.task () in
  let req = XmlHttpRequest.create () in
  let url = match args with
    | [] -> url
    | _ -> url ^ "?" ^ (url_encode_list args) in
  req##(_open (Js.string "GET") (Js.string url) (Js._true));
  req##(setRequestHeader (Js.string "Content-type")
       (Js.string "application/x-www-form-urlencoded"));
  List.iter (fun (n, v) -> req##(setRequestHeader (Js.string n) (Js.string v)))
    headers;
  let callback () =
    match req##.status with
    | 200 -> Lwt.wakeup w (Js.to_string req##.responseText)
    | 204 -> Lwt.wakeup w ""
    | code (* including 0 *) ->
        Lwt.wakeup_exn w
          (Request_failed (code, Js.to_string req##.responseText)) in
  req##.onreadystatechange := Js.wrap_callback
      (fun _ -> (match req##.readyState with
       XmlHttpRequest.DONE -> callback ()
     | _ -> ()));
  req##(send (Js.null));
  Lwt.on_cancel res (fun () -> req##abort);
  res

let post ?(headers=[]) ?(get_args=[]) ~url ~body =
  let (res, w) = Lwt.task () in
  let req = XmlHttpRequest.create () in
  let url = match get_args with
    | [] -> url
    | _ -> url ^ "?" ^ (url_encode_list get_args) in
  req##(_open (Js.string "POST") (Js.string url) (Js._true));
  req##(setRequestHeader (Js.string "Content-type")
       (Js.string "application/x-www-form-urlencoded"));
  List.iter (fun (n, v) -> req##(setRequestHeader (Js.string n) (Js.string v)))
    headers;
  let callback () =
    match req##.status with
    | 200 -> Lwt.wakeup w (Js.to_string req##.responseText)
    | 204 -> Lwt.wakeup w ""
    | code (* including 0 *) -> Lwt.wakeup_exn w
    (Request_failed (code, Js.to_string req##.responseText))
  in
  req##.onreadystatechange := Js.wrap_callback
      (fun _ -> (match req##.readyState with
       XmlHttpRequest.DONE -> callback ()
     | _ -> ()));
  let body = Js.Opt.map (Js.Opt.option body) Js.string in
  req##(send body);
  Lwt.on_cancel res (fun () -> req##abort);
  res
