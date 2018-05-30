(* Copyright (c) 2018 OCamlPro
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software. *)

let known_lang_ids = [%lang_ids_array]

let lang_id = ref 0

let s_ arr = arr.(!lang_id)

let set_lang lang =
  let lang = List.hd (String.split_on_char '-' lang) in
  let rec aux i =
    if i <= 0 then 0
    else if known_lang_ids.(i) = lang then i
    else aux (i-1)
  in
  lang_id := aux (Array.length known_lang_ids - 1)
