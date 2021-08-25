(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

(** {2 Formatted report output} *)

type t = item list

and item =
  | Section of text * t  (** A titled block that groups subreports *)
  | SectionMin of text * t * int  (** A section with a minimum score *)
  | Message of text * status  (** Basic report block *)

and status =
  | Success of int  (** With given points *)
  | Penalty of int  (** With taken points *)
  | Failure  (** With missed points *)
  | Warning  (** A student error without influence on the grade *)
  | Informative  (** A message for the student *)
  | Important  (** An important message *)

and text = inline list

and inline =
  | Text of string  (** A word *)
  | Break  (** Line separator *)
  | Code of string  (** For expressions *)
  | Output of string  (** For output *)

val result : t -> int * bool
(** Gets the total successes of a report, and tells if a failure happened *)

val scale : ?penalties:bool -> int -> t -> t
(** Scales all of the point values of the items in a report by an integer
    factor. Useful for weighting different components of an exercise.
    If [penalties] ([true] by default), scales the values of [Penalty]
    items and the minimum values for [SectionMin], otherwise leaves them
    untouched. *)

val to_html : ?bare:bool -> t -> string
(** Gets a report as HTML in a string
    (if [not bare] add a container div and inline style) *)

val print : Format.formatter -> t -> unit
(** Outputs a report in text format *)

val output_html : ?bare:bool -> Format.formatter -> t -> unit
(** Prints a report as HTML
    (if [not bare] add a container div and inline style) *)

val enc : t Json_encoding.encoding
(** JSON serializer *)

(** {2 Learnocaml_report building combinators} *)

val failure : message:string -> item

val success : points:int -> message:string -> item

val warning : message:string -> item

val message : message:string -> item

val info : message:string -> item

val section : title:string -> t -> item
