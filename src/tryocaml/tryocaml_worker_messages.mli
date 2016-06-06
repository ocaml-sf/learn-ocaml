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

(** Types of the messages exchanged with a toplevel in aWeb Worker. *)

type _ host_msg =
  | Init : unit host_msg
  | Reset : unit host_msg
  | Execute : int option * bool * int * string -> bool host_msg
  | Use_string : string option * bool * int * string -> bool host_msg
  | Use_mod_string : int * bool * string * string option * string -> bool host_msg
  | Set_debug : bool -> unit host_msg
  | Register_callback : string * int -> unit host_msg
  | Set_checking_environment : unit host_msg
  | Check : string -> unit host_msg

type _ msg_ty =
  | Unit : unit msg_ty
  | Bool : bool msg_ty
  | Int : int msg_ty
  | String : Js.js_string Js.t msg_ty

type (_, _) eq = Eq : ('a, 'a) eq

type toploop_msg =
  | Write : int * string -> toploop_msg (* pseudo file descriptor * content *)
  | ReturnSuccess :
      int * 'a msg_ty * 'a * Toploop_results.warning list -> toploop_msg
  | ReturnError :
      int * Toploop_results.error * Toploop_results.warning list -> toploop_msg
