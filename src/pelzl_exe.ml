let mode = ref Pelzl_model.Modern

let speclist = [
  ("--modern", Arg.Unit (fun () -> mode := Modern), "Use modern REPL UI");
  ("--classic", Arg.Unit (fun () -> mode := Classic), "Use classic Orpie UI");
]

let usage_msg = "Pelzl -- a modern RPN calculator for the console"

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  Mosaic.run (Pelzl_app.app !mode)
