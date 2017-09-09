(* $Id: misc.mli,v 1.3 2004/09/24 00:51:15 garrigue Exp $ *)

type wchar = int ;;

val charset : string ;;
val print_wchar : wchar -> unit ;;

val string_of_array : wchar array -> string
val array_of_string : string -> wchar array

val split_last : 'a list -> 'a list * 'a
val do_list3 :
      ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> unit
