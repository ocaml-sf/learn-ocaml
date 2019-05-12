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

let pair2 (Ty t1) (Ty t2) =
  Ty {Parsetree.ptyp_desc = Parsetree.Ptyp_tuple [t1; t2];
      ptyp_loc = Location.none;
      ptyp_attributes = []}

 let pair3 (Ty t1) (Ty t2) (Ty t3) =
  Ty {Parsetree.ptyp_desc = Parsetree.Ptyp_tuple [t1; t2; t3];
      ptyp_loc = Location.none;
      ptyp_attributes = []}

 let pair4 (Ty t1) (Ty t2) (Ty t3) (Ty t4) =
  Ty {Parsetree.ptyp_desc = Parsetree.Ptyp_tuple [t1; t2; t3; t4];
      ptyp_loc = Location.none;
      ptyp_attributes = []}

 let lst (Ty ty) =
  Ty {Parsetree.ptyp_desc =
        Parsetree.Ptyp_constr ({Asttypes.txt = Longident.Lident "list";
                                loc = Location.none},
                               [ty]);
      ptyp_loc = Location.none;
      ptyp_attributes = []}
