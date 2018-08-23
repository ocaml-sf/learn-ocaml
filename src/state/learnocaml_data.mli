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

module SMap: sig

  include Map.S with type key = string

  val enc: 'a Json_encoding.encoding -> 'a t Json_encoding.encoding

end

module SSet: sig

  include Set.S with type elt = string

  val enc: t Json_encoding.encoding

end

module Report = Learnocaml_report

module Answer: sig

  type t = {
    solution: string ;
    grade: int (* \in [0, 100] *) option ;
    report: Report.t option ;
    mtime: float
  }

  val enc: t Json_encoding.encoding

end

module Save: sig

  type t = {
    nickname: string ;
    all_exercise_states: Answer.t SMap.t;
    all_toplevel_histories: Learnocaml_toplevel_history.snapshot SMap.t;
    all_exercise_toplevel_histories: Learnocaml_toplevel_history.snapshot SMap.t;
  }

  val enc: t Json_encoding.encoding

  (** Merges two save files, trusting the [mtime] fields to take the most recent
      versions of every item. All other things equal, the fields from the second
      argument are preferred. *)
  val sync: t -> t -> t

  (** Checks all [mtime] fields to get them back to now in case they are in the
      future. Needed for save files that come from clients with possibly bad
      clocks. *)
  val fix_mtimes: t -> t

end

module Token: sig
  type t

  val enc: t Json_encoding.encoding

  val to_path: t -> string
  val to_string: t -> string
  val parse: string -> t
  val check: string -> bool
  val random: unit -> t
  val random_teacher: unit -> t
  val is_teacher: t -> bool
  val is_student: t -> bool

  (** The relative path containing teacher tokens *)
  val teacher_tokens_path: string

  module Set: Set.S with type elt = t

  module Map: Map.S with type key = t
end

type 'a token = Token.t

type student
type teacher

module Student: sig

  type t = {
    token: student token;
    nickname: string option;
    results: (float * int option) SMap.t;
    tags: string list;
  }

  val enc: t Json_encoding.encoding

end

module Exercise: sig

  type id = string

  type t = Learnocaml_exercise.t

  val enc: t Json_encoding.encoding

  module Meta: sig

    type kind =
      | Project
      | Problem
      | Exercise

    type t = {
      kind: kind;
      title: string;
      short_description: string option;
      stars: float (* \in [0.,4.] *);
      id: id option;
      author: (string * string) list;
      focus: string list;
      requirements: string list;
      forward: id list;
      backward: id list;
      max_score: int option;
    }

    val enc: t Json_encoding.encoding

  end

  module Status: sig

    type tag = string

    type status = Open | Closed | Readonly

    type assignment = {
      start: float;
      stop: float;
    }

    type t = {
      id: id;
      tags: tag list;
      status: status;
      assigned: assignment Token.Map.t;
    }

    val enc: t Json_encoding.encoding

  end

  module Index: sig

    type t =
      | Exercises of (id * Meta.t option) list
      | Groups of (string * group) list
    and group =
      { title : string;
        contents : t }

    val enc: t Json_encoding.encoding

    val find: t -> id -> Meta.t

    val find_opt: t -> id -> Meta.t option

    val filter: (id -> Meta.t -> bool) -> t -> t

  end

end

module Lesson: sig

  type id = string

  type phrase =
    | Text of string
    | Code of string

  type step = {
    step_title: string;
    step_phrases: phrase list;
  }

  type t = {
    title: string;
    steps: step list;
  }

  val enc: t Json_encoding.encoding

  module Index: sig

    type t = (id * string) list

    val enc: t Json_encoding.encoding

  end

end

module Tutorial: sig

  type id = string

  type code = {
    code: string;
    runnable: bool;
  }

  type word =
    | Text of string
    | Code of code
    | Emph of text
    | Image of { alt : string ; mime : string ; contents : bytes }
    | Math of string

  and text =
    word list

  type phrase =
    | Paragraph of text
    | Enum of phrase list list
    | Code_block of code

  type step = {
    step_title: text;
    step_contents: phrase list;
  }

  type t = {
    title: text;
    steps: step list;
  }

  val enc: t Json_encoding.encoding

  module Index: sig

    type entry = {
      name: string;
      title: text;
    }

    type series = {
      series_title: string;
      series_tutorials: entry list;
    }

    type t = (id * series) list

    val enc: t Json_encoding.encoding

  end

end
