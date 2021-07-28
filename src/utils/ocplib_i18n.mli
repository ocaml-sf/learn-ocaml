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

val known_lang_ids : string array
(** Lists all the supported languages *)

val s_ : 'a array -> 'a
(** Gets the right index from the given array for the currently selected
    language (should be used by the preprocessor only) *)

val set_lang : string -> unit
(** Select the currently active language (two-letter code, possibly with country
    suffix e.g. [fr-FR]. *)
