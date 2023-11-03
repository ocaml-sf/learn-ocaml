(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2022 OCaml Software Foundation.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(* These values are injected into the environment after the exercise and
   solutions are loaded, and before the tests are loaded *)

(* Loaded from the exercise: {[
     module Code
     module Solution
   ]} *)

include Introspection_intf.PRE_TEST
