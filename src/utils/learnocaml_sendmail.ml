(* -*- coding: utf-8-unix; -*- *)
(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2020 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

open Netsendmail
(* href: https://gitlab.com/gerdstolpmann/lib-ocamlnet3/-/blob/master/code/src/netstring/netsendmail_tut.txt *)

let smtp_enabled_returnpath_email =
  match Sys.getenv_opt "SMTPSERVER" with
  | None -> None
  | Some _ ->
     match Sys.getenv_opt "EMAIL" with
     | None -> None
     | Some email -> Some email

(* We don't use /usr/sbin/sendmail but msmtp (alpine package) *)
let mailer =  "/usr/bin/msmtp" ^
                begin match Sys.getenv_opt "FROM_DOMAIN" with
                | Some domain -> " --domain " ^ domain
                | None -> ""
                end

(* XXX The following format strings must not contain unsafe HTML chars
   ('<', '>', '"', '&'), as they are not escaped *)

let hello : (string -> string, unit, string) format =
  {|Hello%s,
|}

let confirm : (string -> string, unit, string) format =
  {|
Please follow the link below to confirm your e-mail address:

%s
|}

let confirm_subject = "Confirm your e-mail address"
let change_new_subject = "Confirm your new e-mail address"
let change_old_subject = "Changing your e-mail address"

let change_common : (string -> string -> string, unit, string) format =
  {|
You requested to change your e-mail address on the server.
Old address: %s
New address: %s
|}

let change_old : (string -> string, unit, string) format =
  {|
An e-mail has been sent to the new address for you to confirm it.
Please check your corresponding mailbox (%s).
|}

let change_new : (string -> string, unit, string) format =
  {|
Please follow the link below to confirm this change:

%s
|}

let reset : (string -> string, unit, string) format =
  {|
Someone (probably you) requested changing your Learn-OCaml password.

Please follow the following link to do so:

%s

Otherwise, no further action is required.
|}

let reset_subject = "Change your password"

let closing : string =
  {|
The Learn-OCaml server.|}

(***************************************************************)
(* Now the following helper strings & functions deal with HTML *)

let encode_html_utf8 =
  Netencoding.Html.encode
    ~in_enc:`Enc_utf8
    ~out_enc:`Enc_utf8
    ~prefer_name:true
    ~unsafe_chars:Netencoding.Html.unsafe_chars_html4 ()

(* If need be
let encode_url = Netencoding.Url.encode ~plus:false *)

let link_format : (string -> string -> string, unit, string) format =
  {|<a href="%s">%s</a>|}

(* XXX The message language is hardcoded here: "en" *)
let html_format : (string -> string -> string, unit, string) format =
  {|<!DOCTYPE html>
<html lang="en">
<head><meta charset="UTF-8"><title>%s</title></head>
<body>
<p>%s</p>
</body>
</html>|}

let wrap_url url =
  Printf.sprintf link_format url (encode_html_utf8 url)

let wrap_html ~title text =
  let lines = Str.global_replace (Str.regexp "$") "<br>" text in
  Printf.sprintf html_format ((*encode_html_utf8*) title) lines

let send_email
      ?(from_name="Learn-OCaml")
      ~(nick : string option) ~to_addr ~subject
      ?(hello=hello) ?(pretext="") ~text ?(posttext=closing) url =
  let padding, nickname =
    match nick with
    | None | Some "" -> "", ""
    | Some nickname -> " ", nickname in
  let str_plain = Printf.sprintf hello (padding ^ nickname)
                  ^ pretext
                  ^ Printf.sprintf text url
                  ^ posttext in
  match smtp_enabled_returnpath_email with
  | Some returnpath_email ->
     let str_html =
       wrap_html ~title:subject
         (Printf.sprintf hello (padding ^ nickname)
          ^ pretext
          ^ Printf.sprintf text (wrap_url url)
          ^ posttext) in
     let charset = ["charset", Netmime_string.mk_param "utf-8"] in
     let body =
       (wrap_parts
          ~content_type:("multipart/alternative", [])
          [ wrap_attachment
              ~content_type: ("text/plain", charset)
              (new Netmime.memory_mime_body str_plain);
            wrap_attachment
              ~content_type: ("text/html", charset)
              (new Netmime.memory_mime_body str_html)
       ]) in
     let mail = wrap_mail
                  (* XXX as Netsendmail doesn't support Reply-To, we use From *)
                  ~from_addr: (from_name, returnpath_email)
                  ~to_addrs: [(nickname, to_addr)]
                  ~subject
                  body in
     sendmail ~mailer ~crlf:false mail;
     Printf.printf {|[INFO] mailto:%s?subject="%s"
%!|} to_addr subject
  | None ->
     Printf.printf {|[WARNING] Environment variables SMTPSERVER and EMAIL must be set! (*
Can't mailto:%s?subject="%s" with body """
%s
""" *)
%!|} to_addr subject str_plain

(* If need be
let check_email email =
  try match Netaddress.parse email with
      | [`Mailbox _] (* a single mail *) -> ()
      | _ -> invalid_arg "check_email: no single email"
  with
  | Netaddress.Parse_error (_i, str) -> invalid_arg ("check_email: " ^ str)
 *)

let confirm_email ~(nick:string option) ~(url:string) to_addr =
  send_email ~nick ~to_addr ~subject:confirm_subject
    ~text:confirm url

let change_email ~(nick:string option) ~(url:string) old_email new_email =
  send_email ~nick ~to_addr:new_email
    ~subject:change_new_subject
    ~pretext:(Printf.sprintf change_common old_email new_email)
    ~text:change_new url;
  send_email ~nick ~to_addr:old_email
    ~subject:change_old_subject
    ~pretext:(Printf.sprintf change_common old_email new_email)
    ~text:change_old ("mailto:" ^ new_email)

let reset_password ~(nick:string option) ~(url:string) to_addr =
  send_email ~nick ~to_addr ~subject:reset_subject
    ~text:reset url
