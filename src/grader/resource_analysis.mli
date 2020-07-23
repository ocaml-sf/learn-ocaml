val coerce : 'a option -> 'a


type 'a tree =  Leaf | Node of 'a * ('a tree) * ('a tree)

val show_tree : int tree -> string

val sample_tree : int tree

val report_of_string : string -> Learnocaml_report.t option
