let () =
  Migrate_parsetree.Driver.register ~name:"ppx_metaquot"
    (module Migrate_parsetree.OCaml_405)
    (fun _config _cookies -> Ppx_metaquot.expander [])
