(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
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

(** To be called before using any [Toploop] function. *)
val initialize: unit -> unit

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
    A stack of redirections is maintained for all fire descriptors. So
    the channel is then restored to either the previous redirection or
    to the original file descriptor. *)
val stop_channel_redirection : redirection -> unit
