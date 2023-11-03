open Lwt.Infix

let (/) dir f =
  String.concat Filename.dir_sep [ dir ; f ]

let to_file encoding fn value =
  Lwt_io.(with_file ~mode: Output) fn @@ fun chan ->
  let json = Json_encoding.construct encoding value in
  let json = match json with
    | `A _ | `O _ as d -> d
    | v -> `A [ v ] in
  let str = Ezjsonm.to_string ~minify:false (json :> Ezjsonm.t) in
  Lwt_io.write chan str

let from_file encoding fn =
  Lwt_io.(with_file ~mode: Input) fn @@ fun chan ->
  Lwt_io.read chan >>= fun str ->
  let json =
    match Ezjsonm.from_string_result str with
    | Ok json -> json
    | Error err ->
        let loc = match Ezjsonm.read_error_location err with
          | None -> fn
          | Some ((li, col), _) ->
              Printf.sprintf "%s, line %d, column %d" fn li col
        in
        Printf.ksprintf failwith
          "Parse error in %s:\n  %s" loc
          (Ezjsonm.read_error_description err);
  in
  Lwt.return (Json_encoding.destruct encoding json)
