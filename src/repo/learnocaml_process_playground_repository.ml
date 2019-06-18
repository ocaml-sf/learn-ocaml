open Lwt.Infix

let exercises_dir = ref "./playground"
let exercises_index = ref None

let errored exn =
  let print_unknown ppf = function
    | Failure msg -> Format.fprintf ppf "Cannot process exercises: %s" msg
    | exn -> Format.fprintf ppf "Cannot process exercises: %s"  (Printexc.to_string exn) in
  Json_encoding.print_error ~print_unknown Format.err_formatter exn ;
  Format.eprintf "@." ;
  Lwt.return false
                    
let catched = failwith "todo"

let main dest_dir =
  let (/) dir f =
    String.concat Filename.dir_sep [ dir ; f ] in
  let exercises_index =
    match !exercises_index with
    | Some exercises_index -> exercises_index
    | None -> !exercises_dir / "index.json" in
  let playground_dest_dir = dest_dir / Learnocaml_index.exercises_dir in
  Lwt_utils.mkdir_p playground_dest_dir >>= fun () ->
  Lwt.catch
    catched
    errored
