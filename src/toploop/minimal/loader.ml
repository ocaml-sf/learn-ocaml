(* $Id: loader.ml,v 1.3 1999/08/11 12:27:36 garrigue Exp $ *)

do_list load_object
  [ "map"; "misc"; "common"; "syntax"; "parser"; "lexer"; "types";
    "define"; "unify"; "predef"; "printer"; "typechk"; "untyped";
    "translate"; "compile"; "builtins"; "graph"; "toploop" ]
;;
