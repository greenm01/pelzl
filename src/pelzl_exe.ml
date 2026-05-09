let mode = ref Pelzl_model.Repl

let speclist = [
  ("--repl",  Arg.Unit (fun () -> mode := Repl),
              "Use sober algebraic REPL (default)");
  ("--orpie", Arg.Unit (fun () -> mode := Classic),
              "Use classic Orpie RPN UI");
]

let usage_msg = "Pelzl -- a calculator for the console"

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let mode_kind =
    match !mode with
    | Pelzl_model.Repl -> `Primary    (* inline; native scrollback *)
    | Pelzl_model.Classic -> `Alt     (* full alt-screen TUI *)
  in
  let matrix = Matrix.create ~mode:mode_kind () in
  Mosaic.run ~matrix (Pelzl_app.app !mode)
