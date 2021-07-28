(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type history

type snapshot = {phrases : string list; mtime : float}

val snapshot_enc : snapshot Json_encoding.encoding

val empty_snapshot : snapshot

val create :
     gettimeofday:(unit -> float)
  -> ?on_update:(history -> unit)
  -> ?max_size:int
  -> ?snapshot:snapshot
  -> unit
  -> history

val current : history -> string

val update : history -> string -> unit

val go_backward : history -> unit

val go_forward : history -> unit

val push : history -> unit

val discard : history -> unit

val snapshot : history -> snapshot
