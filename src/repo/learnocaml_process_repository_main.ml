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

open Lwt.Infix

let dest_dir = ref "./www"

let args = Arg.align @@
  [ "-dest-dir", Arg.Set_string dest_dir,
    "PATH path to the exercise repository (default: [./www])" ] @
  Learnocaml_process_exercise_repository.args @
  Learnocaml_process_tutorial_repository.args

let () =
  Arg.parse args
    (fun anon -> raise (Arg.Bad "unexpected anonymous argument"))
    (Printf.sprintf "Usage: %s [options]" Sys.argv.(0));
  let ret = Lwt_main.run @@
    (Learnocaml_process_tutorial_repository.main !dest_dir >>= fun e_ret ->
     Learnocaml_process_exercise_repository.main !dest_dir >>= fun t_ret ->
     Lwt.return (e_ret && t_ret)) in
  exit (if ret then 0 else 1)
