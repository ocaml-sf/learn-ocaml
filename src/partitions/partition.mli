open Learnocaml_data
   
type func_res = Asttypes.rec_flag * Parsetree.value_binding list
   
type partition_result =
  {
    not_graded : Token.t list;
    bad_type : Token.t list;
    patition_by_grade :
      (int *
         ((string * Token.t list) Clustering.tree list))
        list;
  }

val parititon : string -> string -> partition_result
