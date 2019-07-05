(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type history =
  { mutable storage : string array ;
    mutable updated : string array ;
    mutable first : int ;
    mutable stored : int ;
    mutable current : [ `Floating | `Index of int ] ;
    mutable floating : string ;
    on_update : (history -> unit) option ;
    mutable mtime : float ;
    gettimeofday : unit -> float }

type snapshot =
  { phrases : string list ;
    mtime : float }

let empty_snapshot =
  { phrases = [] ; mtime = 0. }

let snapshot_enc =
  let open Json_encoding in
  union
    [ case (list string)
        (function { phrases ; mtime = 0. } -> Some phrases | _ -> None)
        (fun phrases -> { phrases ; mtime = 0. }) ;
      case (obj2 (req "phrases" (list string)) (req "mtime" float))
        (function { phrases ; mtime } -> Some (phrases, mtime))
        (fun (phrases, mtime) -> { phrases ; mtime }) ]

let snapshot { storage ; first ; stored ; mtime ; _} =
  let rec to_list acc i n =
    if n = 0 then
      List.rev acc
    else
      to_list
        (storage.(i) :: acc)
        ((i + 1) mod Array.length storage)
        (n - 1) in
  { phrases = to_list [] first stored ;
    mtime }

let call_on_update (self : history) =
  self.mtime <- self.gettimeofday () ;
  match self.on_update with
  | None -> ()
  | Some callback -> callback self

let create ~gettimeofday ?on_update ?(max_size = 0) ?snapshot () =
  let history =
    { storage = Array.make max_size "" ;
      updated = Array.make max_size "" ;
      first = 0 ;
      stored = 0 ;
      current = `Floating ;
      floating = "" ;
      on_update ;
      mtime = 0. ;
      gettimeofday } in
  begin match snapshot with
    | None -> ()
    | Some { phrases ; mtime } ->
        history.mtime <- mtime ;
        List.iteri (fun i code ->
            if i >= max_size then () else begin
              history.storage.(i) <- code ;
              history.updated.(i) <- code ;
              history.stored <- history.stored + 1
            end)
          phrases
  end ;
  history

let current history =
  match history with
  | { current = `Floating ; floating ; _ } -> floating
  | { current = `Index i ; updated ; _ } -> updated.(i)

let update history text =
  match history with
  | { current = `Floating ; _ } ->
      history.floating <- text
  | { current = `Index i ; updated ; _ } ->
      updated.(i) <- text ;
      call_on_update history

let go_forward history =
  match history with
  | { current = `Floating ; _ } -> ()
  | { current = `Index i ; _ } ->
      let size = Array.length history.storage in
      let last = (history.first + history.stored - 1) mod size in
      if i = last then
        history.current <- `Floating
      else
        history.current <- `Index ((i + 1) mod size)

let go_backward history =
  match history with
  | { current = `Floating ; _ } ->
      if history.stored > 0 then begin
        let size = Array.length history.storage in
        let last = (history.first + history.stored - 1) mod size in
        history.current <- `Index last
      end
  | { current = `Index i ; first ; _ } ->
      let size = Array.length history.storage in
      if i <> first then
        history.current <- `Index ((i + size - 1) mod size)

let push history =
  let text =
    match history with
    | { current = `Floating ; _ } -> history.floating
    | { current = `Index i ; _ } ->
        let updated = history.updated.(i) in
        history.updated.(i) <- history.storage.(i) ;
        updated in
  history.floating <- "" ;
  history.current <- `Floating ;
  let size = Array.length history.storage in
  if history.stored = 0
  || (let last = (history.stored + history.first - 1) mod size in
      (* don't insert duplicates *)
      history.storage.(last) <> text) then begin
    if text <> "" then begin
      if history.stored < size then begin
        history.stored <- history.stored + 1
      end else begin
        history.first <- (history.first + 1) mod size
      end ;
      let i = (history.stored + history.first - 1) mod size in
      history.storage.(i) <- text ;
      call_on_update history ;
      history.updated.(i) <- text
    end
  end

let discard history =
  match history with
  | { current = `Floating ; _ } ->
      history.floating <- "" ;
      history.current <- `Floating
  | { current = `Index i ; _ } ->
      history.updated.(i) <- history.storage.(i) ;
      history.current <- `Floating
