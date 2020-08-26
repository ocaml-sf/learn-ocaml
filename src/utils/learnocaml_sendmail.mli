(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2020 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** [confim_email ~nick ~url addr] will send an email to confirm that
    the user indeed owns this email address, e.g., at account creation. *)
val confirm_email: nick:string option -> url:string -> string -> unit

(** [change_email ~nick ~url old new] will send 2 emails, so (1) the
    user can confirm to indeed own the new email address and (2) the
    old email account also receives a message for informative purposes. *)
val change_email: nick:string option -> url:string -> string -> string -> unit

(** [reset_password ~nick ~url addr] helps users change their password. *)
val reset_password: nick:string option -> url:string -> string -> unit
