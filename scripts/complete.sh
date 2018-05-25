_learn_ocaml_add_f()
{
  local cmd
  cmd=$1; shift
  _learn_ocaml_add "$($cmd "$@" 2>/dev/null)"
}

_learn_ocaml_flags()
{
  learn-ocaml --help=groff 2>/dev/null | \
      sed -n \
      -e 's%\\-\|\\N'"'45'"'%-%g' \
      -e 's%, \\fB%\n\\fB%g' \
      -e '/^\\fB-/p' | \
      sed -e 's%^\\fB\(-[^\\]*\).*%\1%'
}

_learn_ocaml_commands()
{
  learn-ocaml "$@" --help=groff 2>/dev/null | \
      sed -n \
      -e 's%\\-\|\\N'"'45'"'%-%g' \
      -e '/^\.SH COMMANDS$/,/^\.SH/ s%^\\fB\([^,= ]*\)\\fR.*%\1%p'
  echo '--help'
}

_learn_ocaml_argtype()
{
  local flag
  flag="$1"; shift
  case "$flag" in
      -*)
          learn-ocaml --help=groff 2>/dev/null | \
          sed -n \
              -e 's%\\-\|\\N'"'45'"'%-%g' \
              -e 's%.*\\fB'"$flag"'\\fR[= ]\\fI\([^, ]*\)\\fR.*%\1%p'
          ;;
  esac
}

_learn_ocaml()
{
  local IFS cmd subcmd cur prev compgen_opt

  COMPREPLY=()
  cur=${COMP_WORDS[COMP_CWORD]}
  prev=${COMP_WORDS[COMP_CWORD-1]}
  compgen_opt=()
  _learn_ocaml_reply=()

  case "$(_learn_ocaml_argtype $prev)" in
      INT) ;;
      PORT) IFS=' '; _learn_ocaml_reply+=(80 591 8008 8080);;
      FILE|FILENAME|PREFIX) compgen_opt+=(-o filenames -f);;
      DIR*) compgen_opt+=(-o filenames -d);;
      STRING) ;;
      "")
      _learn_ocaml_add_f _learn_ocaml_commands
      _learn_ocaml_add_f _learn_ocaml_flags;;
  esac

  COMPREPLY=($(compgen -W "${_learn_ocaml_reply[*]}" "${compgen_opt[@]}" -- "$cur"))
  unset _learn_ocaml_reply
  return 0
}

complete -F _learn_ocaml learn-ocaml
