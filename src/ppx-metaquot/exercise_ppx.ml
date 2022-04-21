let () =
  Ppxlib.Driver.register_transformation "print_recorder" ~impl:Printer_recorder.expand
