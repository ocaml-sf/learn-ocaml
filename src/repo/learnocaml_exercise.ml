
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

type id = string

type t =
  { id : id ;
    prelude : string ;
    template : string ;
    descr : (string * string) list ;
    prepare : string ;
    test : string ;
    solution : string ;
    max_score : int ;
  }

let encoding =
  let open Json_encoding in
  conv
    (fun { id; prelude; template; descr; prepare; test; solution; max_score } ->
       id, prelude, template, descr, prepare, test, solution, max_score)
    (fun (id, prelude, template, descr, prepare, test, solution, max_score) ->
       { id ; prelude ; template ; descr ; prepare ; test ; solution ; max_score })
    (obj8
       (req "id" string)
       (req "prelude" string)
       (req "template" string)
       (req "descr" (list (tup2 string string)))
       (req "prepare" string)
       (req "test" string)
       (req "solution" string)
       (req "max-score" int))

(* let meta_from_string m =
 *   Ezjsonm.from_string m
 *   |> Json_encoding.destruct Learnocaml_meta.encoding
 * 
 * let meta_to_string m =
 *   Json_encoding.construct Learnocaml_meta.encoding m
 *   |> (function
 *     | `A _ | `O _ as d -> d
 *     | v -> `A [ v ])
 *   |> Ezjsonm.to_string ~minify:true *)

let descrs_from_string descrs =
  Ezjsonm.from_string descrs
  |> Json_encoding.(destruct (list (tup2 string string)))

let descrs_to_string s =
  Json_encoding.(construct (list (tup2 string string))) s
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
  (* let meta =
   *   { key = "meta.json" ; ciphered = false ;
   *     decode = meta_from_string ; encode = meta_to_string ;
   *     field = (fun ex -> ex.meta) ;
   *     update = (fun meta ex -> { ex with meta })
   *   } *)
  (* let title =
   *   let key = "title.txt" in
   *   { key ; ciphered = false ;
   *     decode = (fun v -> String.trim v) ; encode = (fun v -> v) ;
   *     field = (fun ex ->
   *         match ex.meta.meta_title with
   *           None -> raise (Missing_file ("field " ^ key))
   *         | Some t -> t) ;
   *     update = (fun title ex ->
   *         { ex with meta = { ex.meta with meta_title = Some title }})
   *    } *)
  let max_score =
    let key = "max_score.txt" in
    { key ; ciphered = false ;
      decode = (fun v -> int_of_string v) ; encode = (fun v -> string_of_int v) ;
      field = (fun ex -> ex.max_score);
      update = (fun max_score ex -> { ex with max_score });
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
  let descr : (string * string) list file =
    { key = "descr.html" ; ciphered = false ;
      decode = descrs_from_string ; encode = descrs_to_string ;
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
      (* read_field meta.key >>=
       * begin function
       *     None -> fail (Missing_file meta.key)
       *   | Some meta_json ->
       *       return (meta_from_string meta_json)
       * end >>= fun meta_json ->
       * ex := set meta meta_json !ex; *)
      let read_file ({ key ; ciphered ; decode ; _ } as field) =
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
      (* let read_title () =
       *   match meta_json.meta_title with
       *     Some t ->
       *       ex := set title t !ex;
       *       return ()
       *   | None ->
       *       read_file title >>= fun () ->
       *       try
       *         let t = get title !ex in
       *         ex := set meta { meta_json with meta_title = Some t } !ex;
       *         return ()
       *       with _ -> return ()
       * in
       * let read_max_score () =
       *   match meta_json.meta_max_score with
       *     Some score ->
       *       ex := set max_score (score) !ex;
       *       return ()
       *   | None ->
       *       read_file max_score >>= fun () ->
       *       try
       *         let score = get max_score !ex in
       *         ex := set meta
       *             { meta_json with meta_max_score = Some score } !ex;
       *         return ()
       *       with _ -> return ()
       * in *)
      let descrs = ref [] in
      let rec read_descr lang = function
        | [] ->
           (* If there are no extensions to try, we just give up. *)
           return ()
        | (ext, f) :: other_exts ->
           (* If there are, we create the [filename] containing the
              given [lang] and the first [ext] of the list (as well as
              the function to apply to its result [f]. *)
           let filename =
             (Filename.remove_extension descr.key)
             ^ (match lang with
                | None -> ""
                | Some lang -> "." ^ lang)
             ^ ext
           in
           (* And we try to read that file. *)
           read_field filename
           >>= function
           | None ->
              (* If it does not work, we go on and try other extensions. *)
              read_descr lang other_exts
           | Some raw ->
              (* If it does, we apply the function, add the
                 description to [!descrs] and return. *)
              descrs := (lang, f raw) :: !descrs;
              return ()
      in
      let read_descrs () =
        let langs = [] in
        let exts = (Filename.extension descr.key, fun h -> h) :: [] in
        join (read_descr None exts :: List.map (fun l -> read_descr (Some l) exts) langs)
        >>= fun () ->
        ex := set descr
                (List.map (function (None, v) -> "", v | (Some l, v) -> l, v) !descrs)
                !ex;
        return ()
      in
      join
        [ (* read_title () ; *)
          read_file prelude ;
          read_file template ;
          read_descrs () ;
          read_file prepare ;
          read_file solution ;
          read_file test ;
          (* read_max_score () *) ] >>= fun () ->
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
          (* meta = field_from_file File.meta ex; *)
          prelude = field_from_file File.prelude ex ;
          template = field_from_file File.template ex ;
          descr = field_from_file File.descr ex ;
          prepare = field_from_file File.prepare ex ;
          test = field_from_file File.test ex ;
          solution = field_from_file File.solution ex ;
          max_score = 0 ;
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
        (* write_field meta ;
         * write_field title ; *)
        write_field prelude ;
        write_field template ;
        write_field descr ;
        write_field prepare ;
        write_field solution ;
        write_field test ;
        (* write_field max_score *) ] >>= fun () ->
    return !acc
end

include MakeReaderAnddWriter (Seq)

module LwtReaderAnddWriter = MakeReaderAnddWriter (Lwt)
let read_lwt = LwtReaderAnddWriter.read
let write_lwt = LwtReaderAnddWriter.write

let enc = encoding
