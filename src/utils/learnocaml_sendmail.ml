(* -*- coding: utf-8-unix; -*- *)
(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2020 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(* open Netsendmail *)

(* let mailer = "/usr/lib/sendmail" *)
(* TODO: specify the SMTP relay hostname (SMTPHOST) *)

let confirm_email ~(url:string) email =
  let () = Printf.printf "[confirm_email]\nTo: %s (%s)\n%!" email url in ()

let change_email ~(url:string) old_email new_email =
  let () = Printf.printf "[change_email]\nNew: %s (%s)\n%!" new_email url in
  let () = Printf.printf "Old: %s\n%!" old_email in ()

let reset_password ~(url:string) email =
  let () = Printf.printf "[reset_password]\nTo: %s (%s)\n%!" email url in ()
