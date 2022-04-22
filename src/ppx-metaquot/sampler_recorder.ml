include Ppx_autoregister.Make(struct
    let val_prefix = "sample"
    let inject_def id name var =
      let open Ppxlib in
      let open Ast_builder.Default in
      let loc = var.Location.loc in
      pexp_apply ~loc
        (evar ~loc "Introspection.register_sampler")
        [ Nolabel, estring ~loc (Ppx_autoregister.modname var);
          Nolabel, estring ~loc id;
          Nolabel, estring ~loc name;
          Nolabel, evar ~loc var.txt]
  end)