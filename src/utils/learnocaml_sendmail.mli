(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2020 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** [confim_email ~url address] will send an email to confirm the user
    indeed owns this email address, e.g., at account creation. *)
val confirm_email: url:string -> string -> unit

(** [change_email ~url old new] will send 2 emails so (1) the user can
    confirm it indeed owns the new email address, and (2) it receives
    a message to the old email address, for informative purposes. *)
val change_email: url:string -> string -> string -> unit

(** [reset_password ~url email] helps users that forgot their password. *)
val reset_password: url:string -> string -> unit
