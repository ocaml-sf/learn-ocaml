let () =
  Migrate_parsetree.Driver.register ~name:"ppx_metaquot" (module Migrate_parsetree.OCaml_412)
    (fun _config _cookies -> Ppx_metaquot.Main.expander []);
  Ppxlib.Driver.register_transformation "sample_recorder" ~impl:Recorder.expand
