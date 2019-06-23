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


type save_file =
  { all_index_states :
      Learnocaml_exercise_state.index_state Map.Make(String).t;
    all_editor_states :
      Learnocaml_exercise_state.editor_state Map.Make (String).t  ;      
    all_exercise_states :      
      Learnocaml_exercise_state.exercise_state Map.Make (String).t  ;
    all_toplevel_histories :
      Learnocaml_toplevel_history.snapshot Map.Make (String).t ;
    all_exercise_toplevel_histories :
      Learnocaml_toplevel_history.snapshot Map.Make (String).t }


val save_file_enc : save_file Json_encoding.encoding

val sync : save_file -> save_file -> save_file
