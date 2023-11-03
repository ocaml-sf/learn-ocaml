(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019-2023 OCaml Software Foundation.
 * Copyright (C) 2015-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type id = string

type compiled_lib = { cma: string; js: string }

type compiled = {
  prelude_cmi: string;
  prepare_cmi: string;
  solution_cmi: string;
  test_cmi: string;
  exercise_lib: compiled_lib; (* includes prelude, prepare and solution *)
  test_lib: compiled_lib;
}

type t =
  { id : id ;
    prelude_ml : string ;
    prepare_ml : string ;
    (* absent from the json, empty except when building the exercises *)
    template : string ;
    solution : string ;
    (* absent from the json, empty except when building the exercises *)
    descr : (string * string) list ;
    compiled : compiled ;
    max_score : int ;
    depend : string option ;
    dependencies : string list; (* TODO: move to test.cma + list of cmi file contents *)
  }

let encoding =
  let open Json_encoding in
  let b64 =
    (* TODO: try to use the native implementation on browsers ? *)
    conv
      (fun s -> Base64.encode_string s)
      (fun b -> Result.get_ok (Base64.decode b))
      string
  in
  let compiled_lib_encoding =
    conv
      (fun {cma; js} -> cma, js)
      (fun (cma, js) -> {cma; js})
      (obj2
        (dft "cma" b64 "")
        (dft "js" string ""))
  in
  let compiled_encoding =
    conv
      (fun {prelude_cmi; prepare_cmi; solution_cmi; test_cmi; exercise_lib; test_lib} ->
         (prelude_cmi, prepare_cmi, solution_cmi, test_cmi, exercise_lib, test_lib))
      (fun (prelude_cmi, prepare_cmi, solution_cmi, test_cmi, exercise_lib, test_lib) ->
         {prelude_cmi; prepare_cmi; solution_cmi; test_cmi; exercise_lib; test_lib})
      (obj6
         (req "prelude_cmi" b64)
         (req "prepare_cmi" b64)
         (req "solution_cmi" b64)
         (req "test_cmi" b64)
         (req "exercise_lib" compiled_lib_encoding)
         (req "test_lib" compiled_lib_encoding))
  in
  conv
    (fun { id ; prelude_ml ; prepare_ml = _; template ; descr ; compiled ; max_score ; depend ; dependencies ; solution = _} ->
       (id, prelude_ml, template, descr, compiled, max_score, depend, dependencies))
    (fun ((id, prelude_ml, template, descr, compiled, max_score, depend, dependencies)) ->
       { id ; prelude_ml ; prepare_ml = ""; template ; descr ; compiled ; max_score ; depend ; dependencies; solution = ""})
    (obj8
       (req "id" string)
       (req "prelude_ml" string)
       (req "template" string)
       (req "descr" (list (tup2 string string)))
       (req "compiled" compiled_encoding)
       (req "max-score" int)
       (opt "depend" string)
       (dft "dependencies" (list string) []))

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
      decode : string -> 'a ;
      encode : 'a -> string ;
      field : t -> 'a ;
      update : 'a -> t -> t ;
    }

  exception Missing_file of string

  let get { key ; decode ; _ } ex =
    try
      let raw = StringMap.find key ex in
      decode raw
    with Not_found -> raise (Missing_file ("get  " ^ key))

  let get_opt file ex =
    try (* a missing file here is necessarily [file] *)
      get file ex
    with Missing_file _ -> None

  let has { key ; _ } ex =
    StringMap.mem key ex

  let set { key ; encode  ; _ } raw ex =
    StringMap.add key (encode raw) ex

  let key file = file.key

  let id =
    { key = "id" ;
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
    { key ;
      decode = (fun v -> int_of_string v) ; encode = (fun v -> string_of_int v) ;
      field = (fun ex -> ex.max_score);
      update = (fun max_score ex -> { ex with max_score });
     }
  let prelude_ml =
    { key = "prelude.ml" ;
      decode = (fun v -> v) ; encode = (fun v -> v) ;
      field = (fun ex -> ex.prelude_ml) ;
      update = (fun prelude_ml ex -> { ex with prelude_ml })
     }
  let prepare_ml =
    { key = "prepare.ml" ;
      decode = (fun v -> v) ; encode = (fun v -> v) ;
      field = (fun ex -> ex.prepare_ml) ;
      update = (fun prepare_ml ex -> { ex with prepare_ml })
     }
  let template =
    { key = "template.ml" ;
      decode = (fun v -> v) ; encode = (fun v -> v) ;
      field = (fun ex -> ex.template) ;
      update = (fun template ex -> { ex with template })
     }
  let solution =
    { key = "solution.ml" ;
      decode = (fun v -> v) ; encode = (fun v -> v) ;
      field = (fun ex -> ex.solution) ;
      update = (fun solution ex -> { ex with solution })
     }
  let descr : (string * string) list file =
    { key = "descr.html" ;
      decode = descrs_from_string ; encode = descrs_to_string ;
      field = (fun ex -> ex.descr) ;
      update = (fun descr ex -> { ex with descr })
     }
  let compiled key get set =
    { key;
      decode = (fun v -> v) ; encode = (fun v -> v) ;
      field = (fun ex -> get ex.compiled) ;
      update = (fun v ex -> { ex with compiled = set v ex.compiled }) }
  let prelude_cmi =
    compiled "prelude.cmi"
      (fun comp -> comp.prelude_cmi)
      (fun prelude_cmi c -> { c with prelude_cmi })
  let prepare_cmi =
    compiled "prepare.cmi"
      (fun comp -> comp.prepare_cmi)
      (fun prepare_cmi c -> { c with prepare_cmi })
  let solution_cmi =
    compiled "solution.cmi"
      (fun comp -> comp.solution_cmi)
      (fun solution_cmi c -> { c with solution_cmi })
  let test_cmi =
    compiled "test.cmi"
      (fun comp -> comp.test_cmi)
      (fun test_cmi c -> { c with test_cmi })
  let compiled_lib key get set =
    compiled (key^".cma")
      (fun comp -> (get comp).cma)
      (fun cma c -> let l = get c in set { l with cma } c),
    compiled (key^".js")
      (fun comp -> (get comp).js)
      (fun js c -> let l = get c in set { l with js } c)
  let exercise_cma, exercise_js =
    compiled_lib "exercise"
      (fun comp -> comp.exercise_lib)
      (fun exercise_lib c -> { c with exercise_lib })
  let test_cma, test_js =
    compiled_lib "test"
      (fun comp -> comp.test_lib)
      (fun test_lib c -> { c with test_lib })
  let depend =
    { key = "depend.txt" ;
      decode = (fun v -> Some v) ;
      encode = (function
                | None -> "" (* no `depend` ~ empty `depend` *)
                | Some txt -> txt) ;
      field = (fun ex -> ex.depend) ;
      update = (fun depend ex -> { ex with depend })
     }

  (* [parse_dependencies txt] extracts dependencies from the string [txt].
    Dependencies are file names separated by at least one line break.
    [txt] may contain comments starting with characters ';' or '#'
    and ending by a line break. *)
  let parse_dependencies txt =
    let remove_comment ~start:c line =
          match String.index_opt line c with
          | None -> line
          | Some index -> String.sub line 0 index in
    let lines = String.split_on_char '\n' txt in
    List.filter ((<>) "") @@
    List.map (fun line -> String.trim (remove_comment ~start:'#' line)) lines

  let dependencies = function
    | None -> []
    | Some txt ->
      let filenames = parse_dependencies txt in
      List.mapi
        (fun pos filename ->
          { key = filename ;
            decode = (fun v -> v) ; encode = (fun v -> v) ;
            field = (fun ex -> List.nth ex.dependencies pos) ;
            update = (fun v ex ->
                        let dependencies =
                          List.mapi (fun i v' -> if i = pos then v else v')
                            ex.dependencies in { ex with dependencies }) })
        filenames

  module MakeReader (Concur : Concur) = struct
    let read ~read_field ?id: ex_id () =
      let open Concur in
      let ex = ref StringMap.empty in
      read_field id.key >>= fun pr_id ->
      begin match ex_id, pr_id with
        | None, None -> fail (Failure "Exercise.read: missing id")
        | Some id, None | None, Some id -> return id
        | Some id, Some id' ->
            if id = id' then return id else
              fail (Failure "Exercise.read: conflicting ids")
      end >>= fun ex_id ->
      ex := set id ex_id !ex ;
      (* read_field meta.key >>=
       * begin function
       *     None -> fail (Missing_file meta.key)
       *   | Some meta_json ->
       *       return (meta_from_string meta_json)
       * end >>= fun meta_json ->
       * ex := set meta meta_json !ex; *)
      let read_file ({ key ; decode ; _ } as field) =
        read_field key >>= function
        | Some raw ->
            (* decode / encode now to catch malformed fields earlier *)
            ex := set field (decode raw) !ex ;
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
  let override_url = function
    | Omd_representation.Url(href,s,title) ->
       if String.length href > 0 then
         if Char.equal (String.get href 0) '#' then
           None
         else
           let title_url =
             if title <> "" then Printf.sprintf {| title="%s"|}
                               (Omd_utils.htmlentities ~md:true title) else "" in
           let html =
             Printf.sprintf
              {|<a href="%s" target="_blank" rel="noopener noreferrer"%s>%s</a>|}
              (Omd_utils.htmlentities ~md:true href) title_url
                 (Omd_backend.html_of_md s) in
           Some html
         else None
    | _ -> None in
      let markdown_to_html md =
        Omd.(md |> of_string |> to_html ~override:override_url)
      in
      let read_descrs () =
        let langs = [] in
        let exts = [
            (Filename.extension descr.key, fun h -> h) ;
            (".md", markdown_to_html)
          ] in
        join (read_descr None exts :: List.map (fun l -> read_descr (Some l) exts) langs)
        >>= fun () ->
        ex := set descr
                (List.map (function (None, v) -> "", v | (Some l, v) -> l, v) !descrs)
                !ex;
        return ()
      in
      join
        [ (* read_title () ; *)
          read_file prelude_ml ;
          read_file prepare_ml ;
          read_file template ;
          read_file solution ;
          read_descrs () ;
          read_file prelude_cmi ;
          read_file prepare_cmi ;
          read_file solution_cmi ;
          read_file test_cmi ;
          read_file exercise_cma ;
          read_file exercise_js ;
          read_file test_cma ;
          read_file test_js ;
          read_file depend ;
          (* read_max_score () *) ] >>= fun () ->
      join (List.map read_file (dependencies (get_opt depend !ex))) >>= fun () ->
      return !ex
  end

  include MakeReader (Seq)
end

let access f ex =
  f.File.field ex

let decipher f ex =
  let open File in
  let raw = f.field ex in
  f.decode raw

let update f v ex =
  f.File.update v ex

let cipher f v ex =
  let open File in
  f.update (f.encode v) ex

let field_from_file file files =
  try File.(StringMap.find file.key files |> file.decode)
  with Not_found -> raise File.(Missing_file file.key)

let strip need_js ex =
  let f {cma; js} =
    if need_js then {cma= ""; js} else {cma; js = ""}
  in
  { ex with
    compiled =
      { ex.compiled with
        exercise_lib = f ex.compiled.exercise_lib;
        test_lib = f ex.compiled.test_lib } }


module MakeReaderAnddWriter (Concur : Concur) = struct

  module FileReader = File.MakeReader(Concur)

  let read ~read_field ?id () =
    let open Concur in
    FileReader.read ~read_field ?id () >>= fun ex ->
    try
      let depend = File.get_opt File.depend ex in
      return
        { id = field_from_file File.id ex;
          (* meta = field_from_file File.meta ex; *)
          prelude_ml = field_from_file File.prelude_ml ex ;
          prepare_ml = field_from_file File.prepare_ml ex ;
          template = field_from_file File.template ex ;
          solution = field_from_file File.solution ex ;
          descr = field_from_file File.descr ex ;
          compiled = {
            prelude_cmi = field_from_file File.prelude_cmi ex;
            prepare_cmi = field_from_file File.prepare_cmi ex;
            solution_cmi = field_from_file File.solution_cmi ex;
            test_cmi = field_from_file File.test_cmi ex;
            exercise_lib = {
              cma = field_from_file File.exercise_cma ex;
              js = field_from_file File.exercise_js ex;
            };
            test_lib = {
              cma = field_from_file File.test_cma ex;
              js = field_from_file File.test_js ex;
            };
          };
          max_score = 0 ;
          depend ;
          dependencies =
            let field_from_dependency file =
              try field_from_file file ex
              with File.Missing_file msg
              -> let msg' = msg ^ ": dependency declared in "
                                ^ File.(key depend) ^ ", but not found" in
                 raise (File.Missing_file msg')
            in
            List.map field_from_dependency (File.dependencies depend)
        }
    with File.Missing_file _ as e -> fail e

  let write ~write_field ex acc =
    let open Concur in
    let open File in
    let acc = ref acc in
    let write_field { key ; encode ; field ; _  } =
      try
        let raw = field ex |> encode in
        write_field key raw !acc >>= fun nacc ->
        acc := nacc ;
        return ()
      with Not_found -> Concur.return () in
    join
      ([ write_field id ;
        (* write_field meta ;
         * write_field title ; *)
         write_field prelude_ml ;
         (* prepare not written on purpose *)
        write_field template ;
         (* solution not written on purpose *)
        write_field descr ;
        write_field prelude_cmi ;
        write_field prepare_cmi ;
        write_field solution_cmi ;
        write_field exercise_cma ;
        write_field exercise_js ;
        write_field test_cma ;
        write_field test_js ;
        write_field depend ;
        (* write_field max_score *) ]
        @ (List.map write_field (dependencies (access depend ex))) )
        >>= fun () ->
    return !acc
end

include MakeReaderAnddWriter (Seq)

module LwtReaderAnddWriter = MakeReaderAnddWriter (Lwt)
let read_lwt = LwtReaderAnddWriter.read
let write_lwt = LwtReaderAnddWriter.write

let enc = encoding
