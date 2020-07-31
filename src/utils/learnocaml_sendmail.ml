(* -*- coding: utf-8-unix; -*- *)
(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2020 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

let smtp_enabled_returnpath_email =
  match Sys.getenv_opt "SMTPSERVER" with
  | None -> None
  | Some _ ->
     match Sys.getenv_opt "EMAIL" with
     | None -> None
     | Some email -> Some email

(* Don't use /usr/sbin/sendmail but msmtp *)

let mailer =  "/usr/bin/msmtp" ^
                begin match Sys.getenv_opt "FROM_DOMAIN" with
                | Some domain -> " --domain " ^ domain
                | None -> ""
                end

open Netsendmail

(* todo: replace to_name with the Nickname *)
let send_email
      ?(from_name="Learn-OCaml") ?(to_name="")
      ~to_addr ~subject ?(pretext="") ~text (url:string) =
  match smtp_enabled_returnpath_email with
  | Some returnpath_email ->
     let str = pretext ^ Format.sprintf text url in
    let body = wrap_attachment
                 ~content_disposition:("inline", [])
                 ~content_type: ("text/plain",
                                 ["charset", Netmime_string.mk_param "utf-8"])
                 (new Netmime.memory_mime_body str) in
    let mail = wrap_mail
                 (* REM: as Netsendmail doesn't support Reply-To, we use From *)
                 ~from_addr: (from_name, returnpath_email)
                 ~to_addrs: [(to_name, to_addr)]
                 ~subject
                 body in
    sendmail ~mailer ~crlf:false mail
  | None -> Printf.printf "mailto:%s?subject=%s (%s)\n%!" to_addr subject url

(* If need be
let check_email email =
  try match Netaddress.parse email with
      | [`Mailbox _] (* a single mail *) -> ()
      | _ -> invalid_arg "check_email: no single email"
  with
  | Netaddress.Parse_error (_i, str) -> invalid_arg ("check_email: " ^ str)
 *)

let confirm_text : (string -> string, unit, string) format =
  {|Hello,

Please follow the link below to confirm your e-mail address:

%s

The Learn-OCaml server.|}

let change_common : (string -> string -> string, unit, string) format =
  {|Hello,

You requested to change your e-mail address on the server.
Old address: %s
New address: %s
|}

let change_old : (string -> string, unit, string) format =
  {|
An e-mail has been sent to the new address for you to confirm it.
Please check your corresponding mailbox (%s).

The Learn-OCaml server.|}

let change_new : (string -> string, unit, string) format =
  {|
Please follow the link below to confirm this change:

%s

The Learn-OCaml server.|}

let reset_text : (string -> string, unit, string) format =
  {|Hello,

Someone (probably you) requested changing your Learn-OCaml password.

Please follow the following link to do so:

%s

Otherwise, no further action is required.

The Learn-OCaml server.|}

let confirm_email ~(url:string) to_addr =
  send_email ~to_addr ~subject:"Confirm your e-mail address"
    ~text:confirm_text url

let change_email ~(url:string) old_email new_email =
  let () = send_email ~to_addr:new_email
             ~subject:"Confirm your new e-mail address"
             ~pretext:(Printf.sprintf change_common old_email new_email)
             ~text:change_new url in
  let () = send_email ~to_addr:old_email
             ~subject:"Changing your e-mail address"
             ~pretext:(Printf.sprintf change_common old_email new_email)
             ~text:change_old new_email in
  ()

let reset_password ~(url:string) to_addr =
  send_email ~to_addr ~subject:"Change your password"
    ~text:reset_text url
