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

(** {2 Formatted report output} *)

type report = item list

and item =
  | Section of text * report
  (** A titled block that groups subreports *)
  | Message of text * status
  (** Basic report block *)

and status =
  | Success of int (** With given points *)
  | Failure (** With missed points *)
  | Warning (** A student error without influence on the grade *)
  | Informative (** A message for the student *)
  | Important (** An important message *)

and text = inline list

and inline =
  | Text of string (** A word *)
  | Break (** Line separator *)
  | Code of string (** For expressions *)
  | Output of string (** For output *)

(** Gets the total successes of a report, and tells if a failure happened *)
val result_of_report : report -> int * bool

(** Gets a report as HTML in a string
    (if [not bare] add a container div and inline style) *)
val html_of_report :  ?bare: bool -> report -> string

(** Outputs a report in text format *)
val print_report : Format.formatter -> report -> unit

(** Prints a report as HTML
    (if [not bare] add a container div and inline style) *)
val output_html_of_report : ?bare: bool -> Format.formatter -> report -> unit

(** JSON serializer *)
val report_enc : report Json_encoding.encoding

(** {2 Learnocaml_report building combinators} *)

val failure : message:string -> item
val success : points:int -> message:string -> item
val warning : message:string -> item
val message : message:string -> item
val info : message:string -> item
val section : title:string -> report -> item
