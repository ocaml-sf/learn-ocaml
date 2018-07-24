
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

open Learnocaml_meta
open Learnocaml_index

type t =
  { id : string ;
    meta : meta ;
    prelude : string ;
    template : string ;
    descr : string ;
    prepare : string ;
    test : string ;
    solution : string ;
  }

let encoding =
  let open Json_encoding in
  conv
    (fun { id; meta; prelude; template; descr; prepare; test; solution } ->
       id, meta, prelude, template, descr, prepare, test, solution)
    (fun (id, meta, prelude, template, descr, prepare, test, solution) ->
       { id ; meta ; prelude ; template ; descr ; prepare ; test ; solution })
    (obj8
       (req "id" string)
       (req "meta" Learnocaml_meta.encoding)
       (req "prelude" string)
       (req "template" string)
       (req "descr" string)
       (req "prepare" string)
       (req "test" string)
       (req "solution" string))

let meta_from_string m =
  Ezjsonm.from_string m
  |> Json_encoding.destruct Learnocaml_meta.encoding

let meta_to_string m =
  Json_encoding.construct Learnocaml_meta.encoding m
  |> (function
    | `A _ | `O _ as d -> d
    | v -> `A [ v ])
  |> Ezjsonm.to_string ~minify:true

module type Concur = sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : exn -> 'a t
  val join : unit t list -> unit t
end

module Seq = struct
  type 'a t = 'a
  let (>>=) x f = f x
  let return x = x
  let fail = raise
  let join _ = ()
end

module File = struct

  module StringMap = Map.Make (String)
  type files = string StringMap.t

  type 'a file =
    { key : string ;
      ciphered : bool ;
      decode : string -> 'a ;
      encode : 'a -> string ;
      field : t -> 'a ;
      update : 'a -> t -> t ;
    }

  exception Missing_file of string

  let get { key ; ciphered ; decode ; _ } ex =
    try
      let raw = StringMap.find key ex in
      if ciphered then
        let prefix =
          Digest.string (StringMap.find "id" ex  ^ "_" ^ key) in
        decode (Xor.decode ~prefix raw)
      else
        decode raw
    with Not_found -> raise (Missing_file ("get  " ^ key))

  let has { key ; _ } ex =
    StringMap.mem key ex

  let set { key ; ciphered ; encode  ; _ } raw ex =
    if ciphered then
      let prefix =
        Digest.string (StringMap.find "id" ex  ^ "_" ^ key) in
      StringMap.add key (Xor.encode ~prefix (encode raw)) ex
    else
      StringMap.add key (encode raw) ex

  let id =
    { key = "id" ; ciphered = false ;
      decode = (fun v -> v) ; encode = (fun v -> v) ;
      field = (fun ex -> ex.id) ;
      update = (fun id ex -> { ex with id })
    }
  let meta =
    { key = "meta.json" ; ciphered = false ;
      decode = meta_from_string ; encode = meta_to_string ;
      field = (fun ex -> ex.meta) ;
      update = (fun meta ex -> { ex with meta })
    }
  let title =
    let key = "title.txt" in
    { key ; ciphered = false ;
      decode = (fun v -> String.trim v) ; encode = (fun v -> v) ;
      field = (fun ex ->
          match ex.meta.meta_title with
            None -> raise (Missing_file ("field " ^ key))
          | Some t -> t) ;
      update = (fun title ex ->
          { ex with meta = { ex.meta with meta_title = Some title }})
     }
  let max_score =
    let key = "max_score.txt" in
    { key ; ciphered = false ;
      decode = (fun v -> int_of_string v) ; encode = (fun v -> string_of_int v) ;
      field = (fun ex ->
          match ex.meta.meta_max_score with
            None -> raise (Missing_file key)
          | Some v -> v);
      update = (fun score ex ->
          { ex with meta = { ex.meta with meta_max_score = Some score }})
     }
  let prelude =
    { key = "prelude.ml" ; ciphered = false ;
      decode = (fun v -> v) ; encode = (fun v -> v) ;
      field = (fun ex -> ex.prelude) ;
      update = (fun prelude ex -> { ex with prelude })
     }
  let template =
    { key = "template.ml" ; ciphered = false ;
      decode = (fun v -> v) ; encode = (fun v -> v) ;
      field = (fun ex -> ex.template) ;
      update = (fun template ex -> { ex with template })
     }
  let descr =
    { key = "descr.html" ; ciphered = false ;
      decode = (fun v -> v) ; encode = (fun v -> v) ;
      field = (fun ex -> ex.descr) ;
      update = (fun descr ex -> { ex with descr })
     }
  let prepare =
    { key = "prepare.ml" ; ciphered = true ;
      decode = (fun v -> v) ; encode = (fun v -> v) ;
      field = (fun ex -> ex.prepare) ;
      update = (fun prepare ex -> { ex with prepare })
     }
  let test =
    { key = "test.ml" ; ciphered = true ;
      decode = (fun v -> v) ; encode = (fun v -> v) ;
      field = (fun ex -> ex.test) ;
      update = (fun test ex -> { ex with test })
     }
  let solution =
    { key = "solution.ml" ; ciphered = true ;
      decode = (fun v -> v) ; encode = (fun v -> v) ;
      field = (fun ex -> ex.solution) ;
      update = (fun solution ex -> { ex with solution })
     }

  module MakeReader (Concur : Concur) = struct
    let read ~read_field ?id: ex_id ?(decipher = true) () =
      let open Concur in
      let ex = ref StringMap.empty in
      read_field id.key >>= fun pr_id ->
      begin match ex_id, pr_id with
        | None, None -> fail (Failure "Exercise.read: missing id")
        | Some id, None | None, Some id -> return id
        | Some id, Some id' ->
            if id = id' then return id else
              fail (Failure "Exercise.read: conficting ids")
      end >>= fun ex_id ->
      ex := set id ex_id !ex ;
      read_field meta.key >>=
      begin function
          None -> fail (Missing_file meta.key)
        | Some meta_json ->
            return (meta_from_string meta_json)
      end >>= fun meta_json ->
      ex := set meta meta_json !ex;
      let read_field ({ key ; ciphered ; decode ; _ } as field) =
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
      let read_title () =
        match meta_json.meta_title with
          Some t ->
            ex := set title t !ex;
            return ()
        | None ->
            read_field title >>= fun () ->
            try
              let t = get title !ex in
              ex := set meta { meta_json with meta_title = Some t } !ex;
              return ()
            with _ -> return ()
      in
      let read_max_score () =
        match meta_json.meta_max_score with
          Some score ->
            ex := set max_score (score) !ex;
            return ()
        | None ->
            read_field max_score >>= fun () ->
            try
              let score = get max_score !ex in
              ex := set meta
                  { meta_json with meta_max_score = Some score } !ex;
              return ()
            with _ -> return ()
      in
      join
        [ read_title () ;
          read_field prelude ;
          read_field template ;
          read_field descr ;
          read_field prepare ;
          read_field solution ;
          read_field test ;
          read_max_score () ] >>= fun () ->
      return !ex
  end

  include MakeReader (Seq)
end

let access f ex =
  f.File.field ex

let decipher f ex =
  let open File in
  let raw = f.field ex in
  if f.ciphered then
    let prefix =
      Digest.string (ex.id  ^ "_" ^ f.key) in
    f.decode (Xor.decode ~prefix raw)
  else
    f.decode raw

let update f v ex =
  f.File.update v ex

let cipher f v ex =
  let open File in
  if f.ciphered then
    let prefix =
      Digest.string (ex.id  ^ "_" ^ f.key) in
    f.update (Xor.encode ~prefix (f.encode v)) ex
  else
    f.update (f.encode v) ex


let meta_from_index ex =
  { meta_kind = ex.exercise_kind ;
    meta_title = Some ex.exercise_title ;
    meta_short_description = ex.exercise_short_description ;
    meta_stars = ex.exercise_stars ;
    meta_identifier = ex.exercise_identifier ;
    meta_author = ex.exercise_author ;
    meta_focus = ex.exercise_focus ;
    meta_requirements = ex.exercise_requirements ;
    meta_forward = ex.exercise_forward ;
    meta_backward = ex.exercise_backward ;
    meta_max_score = ex.exercise_max_score ;
  }

let to_index exercise =
  { exercise_kind = exercise.meta.meta_kind ;
    exercise_stars = exercise.meta.meta_stars ;
    exercise_title = access File.title exercise ;
    exercise_short_description = exercise.meta.meta_short_description;
    exercise_identifier = exercise.meta.meta_identifier ;
    exercise_author = exercise.meta.meta_author ;
    exercise_focus = exercise.meta.meta_focus ;
    exercise_requirements =  exercise.meta.meta_requirements ;
    exercise_forward = exercise.meta.meta_forward ;
    exercise_backward = exercise.meta.meta_backward ;
    exercise_max_score = exercise.meta.meta_max_score ;
  }

let field_from_file file files =
  try File.(StringMap.find file.key files |> file.decode)
  with Not_found -> raise File.(Missing_file file.key)

module MakeReaderAnddWriter (Concur : Concur) = struct
  module FileReader = File.MakeReader(Concur)
  let read ~read_field ?id ?decipher () =
    let open Concur in
    FileReader.read ~read_field ?id ?decipher () >>= fun ex ->
    try
      return
        { id = field_from_file File.id ex;
          meta = field_from_file File.meta ex;
          prelude = field_from_file File.prelude ex ;
          template = field_from_file File.template ex ;
          descr = field_from_file File.descr ex ;
          prepare = field_from_file File.prepare ex ;
          test = field_from_file File.test ex ;
          solution = field_from_file File.solution ex ;
        }
    with File.Missing_file _ as e -> fail e

  let write ~write_field ex ?(cipher = true) acc =
    let open Concur in
    let open File in
    let acc = ref acc in
    let ex_id = ex.id in
    let write_field { key ; ciphered ; encode ; field ; _  } =
      try
        let raw = field ex |> encode in
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
        write_field meta ;
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

include MakeReaderAnddWriter (Seq)

module LwtReaderAnddWriter = MakeReaderAnddWriter (Lwt)
let read_lwt = LwtReaderAnddWriter.read
let write_lwt = LwtReaderAnddWriter.write

let enc = encoding
