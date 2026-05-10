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
  (* In Primary mode the Matrix runtime queries the cursor position to
     anchor its inline render area.  Emit a newline first so the cursor
     is reliably at column 1 when that query fires; without this the
     render-offset calculation can be wrong, causing the live area to
     appear at the wrong row and leaving stale content on exit. *)
  if mode_kind = `Primary then (print_char '\n'; flush stdout);
  let matrix = Matrix.create ~mode:mode_kind () in
  let editor_runner path =
    Matrix.suspend matrix;
    Fun.protect
      ~finally:(fun () -> Matrix.resume matrix)
      (fun () -> ignore (Sys.command (!(Rcfile.editor) ^ " " ^ Filename.quote path)))
  in
  Mosaic.run ~matrix (Pelzl_app.app ~editor_runner !mode)
