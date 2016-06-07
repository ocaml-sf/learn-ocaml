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

type any

(* Not for the faint of heart. *)

module Repr : Json_repr.Repr with type value = any Js.t = struct
  type value = any Js.t
  let repr = function
    | `String s -> Js.Unsafe.coerce (Js.string s)
    | `Float f -> Js.Unsafe.coerce (Obj.magic f)
    | `Bool true -> Js.Unsafe.coerce Js._true
    | `Bool false -> Js.Unsafe.coerce Js._false
    | `Null -> Obj.magic Js.null (* Oh, nom nom nom! *)
    | `O fields ->
        let obj = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "Object") [||] in
        List.iter
          (fun (n, v) -> Js.Unsafe.set obj (Js.string n) v)
          fields ;
        obj
    | `A cells ->
        Js.Unsafe.coerce (Js.array (Array.of_list cells))

  let view v =
    match Js.to_string (Js.typeof v) with
    | "string" -> `String (Js.to_string (Js.Unsafe.coerce v))
    | "number" -> `Float (Obj.magic v)
    | "bool" -> `Bool (Js.to_bool (Obj.magic v))
    | "undefined" -> `Null (* Oh yeah! *)
    | "object" ->
        if v == Js.Unsafe.pure_js_expr "null" then
          `Null
        else if Js.instanceof v (Js.Unsafe.pure_js_expr "Array") then
          let rec loop acc n =
            if n < 0 then
              `A acc
            else
              loop (Js.Unsafe.get v n :: acc) (n - 1)
          in
          loop [] (Js.Unsafe.get v (Js.string "length") - 1)
        else
          let fields : Js.js_string Js.t list =
            Array.to_list @@ Js.to_array
              (Js.Unsafe.fun_call
                 (Js.Unsafe.js_expr
                    "(function(o){\
                    \  var p=[];\
                    \  for(var n in o){if(o.hasOwnProperty(n)){p.push(n);}}\
                    \  return p;\
                     })")
                 [| Js.Unsafe.inject v |]) in
          `O (List.map
                (fun f -> Js.to_string f, Js.Unsafe.get v f)
                fields)
    | _ -> invalid_arg "Browser_json.Repr.view"
end

module Json_encoding = Json_encoding.Make (Repr)
module Json_query = Json_query.Make (Repr)
