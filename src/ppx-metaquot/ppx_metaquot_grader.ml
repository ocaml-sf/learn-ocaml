module Sampler_recorder = Ppx_autoregister.Make(struct
    let val_prefix = "sample"
    let inject_def name var =
      let open Ppxlib in
      let open Ast_builder.Default in
      let loc = var.Location.loc in
      pexp_apply ~loc
        (evar ~loc "Introspection.register_sampler")
        [ Nolabel, estring ~loc name
        ; Nolabel, evar ~loc var.txt]
  end)

let () =
  Migrate_parsetree.Driver.register ~name:"ppx_metaquot" (module Migrate_parsetree.OCaml_412)
    (fun _config _cookies -> Ppx_metaquot.Main.expander []);
  Ppxlib.Driver.register_transformation "sample_recorder" ~impl:Sampler_recorder.expand
