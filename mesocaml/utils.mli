val maybe : 'b -> ('a -> 'b) -> 'a option -> 'b

val fmapOption : ('a -> 'b) -> 'a option -> 'b option

val bindOption : ('a -> 'b option) -> 'a option -> 'b option
