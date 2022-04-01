let () =
  Migrate_parsetree.Driver.run_as_ppx_rewriter ~exit_on_error:true ()
