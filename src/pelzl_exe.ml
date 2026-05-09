(*  Pelzl -- a modern RPN calculator for the console
 *  Copyright (C) 2026 Mason Austin Green
 *)
let run_classic () =
  let open Curses in
  (* load orpierc *)
  Rcfile.process_rcfile None;

  let initialize_screen () =
    let std = initscr () in
    assert (keypad std true);
    assert (cbreak ());
    assert (noecho ());
    Interface_main.create_windows std
  in

  (* Global: this is the interface state variable used for the calculator *)
  let calc = new Rpc_calc.rpc_calc !Rcfile.conserve_memory in
  let iface = Interface.make calc (initialize_screen ()) in

  (* initialize the error handler *)
  Gsl.Error.init ();

  try
    Interface_main.run iface;
    endwin ()
  with error ->
    endwin ();
    Printf.fprintf stderr "Caught error at toplevel:\n%s\n" (Printexc.to_string error)

let () =
  let use_modern = ref false in
  let speclist = [
    ("--modern", Arg.Set use_modern, "Use the modern Mosaic UI");
    ("--classic", Arg.Clear use_modern, "Use the classic ncurses UI (default)");
  ] in
  let usage_msg = "Pelzl: A modern RPN calculator. Options available:" in
  Arg.parse speclist (fun _ -> ()) usage_msg;

  if !use_modern then
    Modern_ui.run_modern ()
  else
    run_classic ()
