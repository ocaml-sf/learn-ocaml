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

type repr = Parsetree.core_type
type 'a ty = Ty of repr

let obj (Ty ty) = ty

let repr ty = Ty ty

let print (Ty ty) =
  Format.asprintf "%a%!" Pprintast.core_type ty

let domains = function
  | Ty { Parsetree.ptyp_desc = Parsetree.Ptyp_arrow (_, arg, ret) } ->
      (Ty arg, Ty ret)
  | _ -> invalid_arg "Ty.domains"

let curry (Ty arg) (Ty ret) =
  Ty { Parsetree.ptyp_desc = Parsetree.Ptyp_arrow (Asttypes.Nolabel, arg, ret) ;
    ptyp_loc = Location.none ;
    ptyp_attributes = [] }
