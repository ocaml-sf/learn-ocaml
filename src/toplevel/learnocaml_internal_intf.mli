(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2022 OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** Interface of the module that gets automatically injected in the environment
    before the Prelude is loaded. *)
module type CALLBACKS = sig
  val print_html: string -> unit
  val print_svg: string -> unit
end


(* (hidden) interface of the module that will be pre-loaded in the toplevel *)
module type INTERNAL = sig
  val install_printer: string -> string -> string -> ('a -> 'b) -> unit
  exception Undefined
end
