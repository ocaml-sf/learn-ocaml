
let modname var =
  (* This is fragile. Do we have a better way to recover the current
     compilation unit name in a ppx ? *)
  String.capitalize_ascii @@
  Filename.basename @@
  Filename.remove_extension @@
  var.Location.loc.Location.loc_start.Lexing.pos_fname

module Printer_recorder = Ppx_autoregister.Make(struct
    let val_prefix = "print"
    let inject_def id name var =
      let open Ppxlib in
      let open Ast_builder.Default in
      let loc = var.Location.loc in
      pexp_apply ~loc
        (evar ~loc "Introspection.install_printer_internal")
        [ Nolabel, estring ~loc (modname var);
          Nolabel, estring ~loc id;
          Nolabel, estring ~loc name;
          Nolabel, evar ~loc var.txt ]
  end)

module Sampler_recorder = Ppx_autoregister.Make(struct
    let val_prefix = "sample"
    let inject_def id name var =
      let open Ppxlib in
      let open Ast_builder.Default in
      let loc = var.Location.loc in
      pexp_apply ~loc
        (evar ~loc "Introspection.register_sampler")
        [ Nolabel, estring ~loc (modname var);
          Nolabel, estring ~loc id;
          Nolabel, estring ~loc name;
          Nolabel, evar ~loc var.txt]
  end)

let () =
  Migrate_parsetree.Driver.register ~name:"ppx_metaquot" (module Migrate_parsetree.OCaml_412)
    (fun _config _cookies -> Ppx_metaquot.Main.expander []);
  Ppxlib.Driver.register_transformation "print_recorder" ~impl:Printer_recorder.expand;
  Ppxlib.Driver.register_transformation "sample_recorder" ~impl:Sampler_recorder.expand
