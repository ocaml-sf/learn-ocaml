(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** To be called before using any [Toploop] function. *)
val initialize: unit -> unit

(** Load the given compiled code *)
val use_compiled_string: string -> unit

(** Materializes an output channel redirection. *)
type redirection

(** Redirects a channel.
    Instead of being output to the file underlying file descriptor,
    any data output to this channel will be passed to the callback
    functions.
    The string parameter is an identifier, that is passed back to the
    secondary callback [tee]. This is useful to use the same [tee]
    callback for several channels. *)
val redirect_channel:
  ?tee:(string -> string -> unit) ->
  string -> out_channel -> (string -> unit) ->
  redirection

(** Flushes the channel, calling the callbacks if necessary. *)
val flush_redirected_channel : redirection -> unit

(** Flushes the channel and then cancel the redirection.
    The redirection must be the last one performed, otherwise an
    [Invalid_argument] will be raised.
    A stack of redirections is maintained for all file descriptors. So
    the channel is then restored to either the previous redirection or
    to the original file descriptor. *)
val stop_channel_redirection : redirection -> unit
