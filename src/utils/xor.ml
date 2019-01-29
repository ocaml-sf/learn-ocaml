(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
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

let alphabet =
  Bytes.of_string
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let () =
  Bytes.set alphabet 26 '+';
  Bytes.set alphabet 37 '/';
  for i = 0 to 25 do
    Bytes.set alphabet i (Char.chr @@ 65 + i);
    Bytes.set alphabet (i+38) (Char.chr @@ 97 + 25 - i)
  done;
  for i = 0 to 9 do
    Bytes.set alphabet (i+27) (Char.chr @@ 48 + i)
  done

let xor_key =
   "Caml1999I0150\153\200\232\027\154a\029u@\251\127SX\141\140\157\
    \219\195\000\228\020\180_CR\202\130\129\127\2491\130\011\183\
    \158b\022\"qB0\166+\169\212_\205\164 D\210Qn\181o\225\147q\156\
    \028u6\248b\177\002\164`\187\250\221\240o6\156\240\020\027\243o\
    \017h\218\208\168\164f\161+5\137\132ml\169\235\174\212\029"

let xor ?prefix str =
  let xor_key =
    match prefix with
    | None -> xor_key
    | Some prefix -> prefix ^ xor_key in
  let str' = Bytes.create (String.length str) in
  for i = 0 to String.length str - 1 do
    let c = Char.code xor_key.[i mod (String.length xor_key)] in
    Bytes.set str' (i) (Char.chr (c lxor (Char.code (String.get str i))))
  done;
  Bytes.to_string str'

let alphabet = Bytes.to_string alphabet
let decode ?prefix str = xor ?prefix @@ B64.decode ~alphabet str
let encode ?prefix str = B64.encode ~alphabet @@ xor ?prefix str
