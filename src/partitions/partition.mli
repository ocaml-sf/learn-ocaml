open Learnocaml_data
   
type func_res = Asttypes.rec_flag * Parsetree.value_binding list
   
type partition_result =
  {
    not_graded : Token.t list;
    bad_type : Token.t list;
    patition_by_grade :
      (int *
         ((Token.t * Report.t * func_res) list *
            Token.t list Clustering.tree list))
        list;
  }

val parititon : string -> string -> partition_result
