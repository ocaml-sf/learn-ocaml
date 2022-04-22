(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2022 OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(* This interface is used to pre-compile modules for the toplevel, giving them
   access to specific toplevel functions. It should not be made accessible to
   the non-precompiled code running in the toplevel *)
include Learnocaml_internal_intf.INTERNAL
