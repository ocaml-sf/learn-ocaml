(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

type repr = Parsetree.core_type
type 'a ty = Ty of repr

let obj (Ty ty) = ty

let repr ty = Ty (ty)

let print (Ty ty) =
  Format.asprintf "%a%!" Pprintast.core_type ty

let domains = function
  | Ty { Parsetree.ptyp_desc = Parsetree.Ptyp_arrow (_, arg, ret) ; _ } ->
      (Ty arg, Ty ret)
  | _ -> invalid_arg "Ty.domains"

let curry (Ty arg) (Ty ret) =
  Ty { Parsetree.ptyp_desc = Parsetree.Ptyp_arrow (Asttypes.Nolabel, arg, ret) ;
    ptyp_loc = Location.none ;
    ptyp_attributes = [] }
