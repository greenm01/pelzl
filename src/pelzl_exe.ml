let mode = ref Pelzl_model.Repl

let speclist = [
  ("--repl", Arg.Unit (fun () -> mode := Repl), "Use modern REPL UI (default)");
  ("--orpie", Arg.Unit (fun () -> mode := Classic), "Use classic Orpie UI");
]

let usage_msg = "Pelzl -- a modern RPN calculator for the console"

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  Mosaic.run (Pelzl_app.app !mode)
