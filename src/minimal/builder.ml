(* $Id: builder.ml,v 1.2 1999/08/11 12:27:35 garrigue Exp $ *)

do_list compile
  [ "map.mli"; "map.ml"; "misc.mli"; "misc.ml"; "common.ml"; "syntax.ml";
    "parser.mli"; "parser.ml"; "lexer.ml"; "types.ml";
    "define.ml"; "unify.mli"; "unify.ml"; "predef.ml";
    "printer.mli"; "printer.ml"; "typechk.mli"; "typechk.ml"; "untyped.ml";
    "translate.mli"; "translate.ml"; "compile.mli"; "compile.ml";
    "builtins.mli"; "builtins.ml"; "graph.ml"; "toploop.ml" ]
;;
