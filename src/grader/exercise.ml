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

module StringMap = Map.Make (String)
type t = string StringMap.t

type 'a field =
  { key : string ;
    ciphered : bool ;
    decode : string -> 'a ;
    encode : 'a -> string }

exception Missing_field of string

let get { key ; ciphered ; decode } ex =
  try
    let raw = StringMap.find key ex in
    if ciphered then
      let prefix =
        Digest.string (StringMap.find "id" ex  ^ "_" ^ key) in
      decode (Xor.decode ~prefix raw)
    else
      decode raw
  with Not_found -> raise (Missing_field key)

let has { key } ex =
  StringMap.mem key ex

let set { key ; ciphered ; encode } raw ex =
  if ciphered then
    let prefix =
      Digest.string (StringMap.find "id" ex  ^ "_" ^ key) in
    StringMap.add key (Xor.encode ~prefix (encode raw)) ex
  else
    StringMap.add key (encode raw) ex

let id =
  { key = "id" ; ciphered = false ;
    decode = (fun v -> v) ; encode = (fun v -> v) }
let title =
  { key = "title.txt" ; ciphered = false ;
    decode = (fun v -> String.trim v) ; encode = (fun v -> v) }
let max_score =
  { key = "max_score.txt" ; ciphered = false ;
    decode = (fun v -> int_of_string v) ; encode = (fun v -> string_of_int v) }
let prelude =
  { key = "prelude.ml" ; ciphered = false ;
    decode = (fun v -> v) ; encode = (fun v -> v) }
let template =
  { key = "template.ml" ; ciphered = false ;
    decode = (fun v -> v) ; encode = (fun v -> v) }
let descr =
  { key = "descr.html" ; ciphered = false ;
    decode = (fun v -> v) ; encode = (fun v -> v) }
let prepare =
  { key = "prepare.ml" ; ciphered = true ;
    decode = (fun v -> v) ; encode = (fun v -> v) }
let test =
  { key = "test.ml" ; ciphered = true ;
    decode = (fun v -> v) ; encode = (fun v -> v) }
let solution =
  { key = "solution.ml" ; ciphered = true ;
    decode = (fun v -> v) ; encode = (fun v -> v) }

module type Concur = sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : exn -> 'a t
  val join : unit t list -> unit t
end

module MakeReaderAnddWriter (Concur : Concur) = struct
  let read ~read_field ?id: ex_id ?(decipher = true) () =
    let open Concur in
    let ex = ref StringMap.empty in
    read_field "id" >>= fun pr_id ->
    begin match ex_id, pr_id with
      | None, None -> fail (Failure "Exercise.read: missing id")
      | Some id, None | None, Some id -> return id
      | Some id, Some id' ->
          if id = id' then return id else
            fail (Failure "Exercise.read: conficting ids")
    end >>= fun ex_id ->
    ex := set id ex_id !ex ;
    let read_field ({ key ; ciphered ; encode ; decode } as field) =
      read_field key >>= function
      | Some raw ->
          let deciphered =
            if ciphered && decipher then
              let prefix =
                Digest.string (ex_id  ^ "_" ^ key) in
              Xor.decode ~prefix raw
            else
              raw in
          (* decode / encode now to catch malformed fields earlier *)
          ex := set field (decode deciphered) !ex ;
          return ()
      | None -> return () in
    join
      [ read_field title ;
        read_field prelude ;
        read_field template ;
        read_field descr ;
        read_field prepare ;
        read_field solution ;
        read_field test ;
        read_field max_score ] >>= fun () ->
    return !ex

  let write ~write_field ex ?(cipher = true) acc =
    let open Concur in
    let acc = ref acc in
    let ex_id = get id ex in
    let write_field { key ; ciphered ; encode ; decode } =
      try
        let raw = StringMap.find key ex in
        let ciphered = if ciphered && (not cipher) then
            let prefix =
              Digest.string (ex_id  ^ "_" ^ key) in
            Xor.decode ~prefix raw
          else
            raw in
        write_field key ciphered !acc >>= fun nacc ->
        acc := nacc ;
        return ()
      with Not_found -> Concur.return () in
    join
      [ write_field id ;
        write_field title ;
        write_field prelude ;
        write_field template ;
        write_field descr ;
        write_field prepare ;
        write_field solution ;
        write_field test ;
        write_field max_score ] >>= fun () ->
    return !acc
end

module Seq = struct
  type 'a t = 'a
  let (>>=) x f = f x
  let return x = x
  let fail = raise
  let join l = ()
end

include MakeReaderAnddWriter (Seq)
module LwtReaderAnddWriter = MakeReaderAnddWriter (Lwt)
let read_lwt = LwtReaderAnddWriter.read
let write_lwt = LwtReaderAnddWriter.write

let enc =
  let open Json_encoding in
  conv
    (fun exo ->
       let obj =
         [ "id", get id exo ;
           "learnocaml_version", "1" ] in
       write
         ~write_field: (fun k v l -> (k, v) :: l)
         exo ~cipher: true obj)
    (fun l ->
       begin try
           if List.assoc "learnocaml_version" l <> "1" then
             raise (Cannot_destruct ([], Failure "unknown version"))
         with
           Not_found -> raise (Cannot_destruct ([], Missing_field "learnocaml_version"))
       end ;
       let id =
         try List.assoc "id" l with
           Not_found -> raise (Cannot_destruct ([], Missing_field "id")) in
       let read_field k =
         try Some (List.assoc k l) with
           Not_found -> None in
       read ~read_field ~id ~decipher: true ())
    (assoc string)
