open Alcotest
open Pelzl_model
open Pelzl_update

let test_init_empty () =
  let model, _cmd = init Repl () in
  check int "stack len" 0 (Pelzl_engine.stack_length model.calc.Pelzl_engine.stack);
  check string "entry" "" model.entry;
  check bool "show_help" false model.show_help;
  check (option string) "error" None model.error_msg;
  check bool "slogan not empty" true (String.length model.slogan > 0)

let test_random_slogan () =
  let rec collect n acc =
    if n = 0 then acc
    else
      let model, _ = init Repl () in
      collect (n - 1) (model.slogan :: acc)
  in
  let slogans = collect 100 [] in
  let unique_slogans = List.sort_uniq String.compare slogans in
  check bool "multiple slogans possible" true (List.length unique_slogans > 1)

let test_push_entry () =
  let model, _cmd = init Repl () in
  let model = { model with entry = "42" } in
  let model', _cmd = update Enter model in
  check string "entry cleared" "" model'.entry;
  check int "stack len" 1 (Pelzl_engine.stack_length model'.calc.Pelzl_engine.stack);
  check (option string) "no error" None model'.error_msg

let test_backspace () =
  let model, _cmd = init Repl () in
  let model = { model with entry = "123" } in
  let model', _cmd = update Backspace model in
  check string "entry" "12" model'.entry

let test_clear_error () =
  let model, _cmd = init Repl () in
  let model = { model with error_msg = Some "oops" } in
  let model', _cmd = update Clear_error model in
  check (option string) "error cleared" None model'.error_msg

let test_toggle_help () =
  let model, _cmd = init Repl () in
  let model', _cmd = update Toggle_help model in
  check bool "help on" true model'.show_help;
  let model'', _cmd = update Toggle_help model' in
  check bool "help off" false model''.show_help

let test_resize () =
  let model, _cmd = init Repl () in
  let model', _cmd = update (Resize (120, 40)) model in
  check int "width" 120 model'.width;
  check int "height" 40 model'.height

let test_ui_modes () =
  let model_m, _cmd = init Repl () in
  check bool "is modern" true (model_m.ui_mode = Repl);
  let model_c, _cmd = init Classic () in
  check bool "is classic" true (model_c.ui_mode = Classic)

let test_algebraic_eval () =
  let model, _cmd = init Repl () in
  let model = { model with entry = "1+2*3" } in
  let model', _cmd = update Enter model in
  check string "entry cleared" "" model'.entry;
  let res_str = Pelzl_engine.get_display_line 1 model'.calc in
  check string "result correct" "7" (String.trim res_str);
  check (option string) "no error" None model'.error_msg

let test_preview_trailing_operator_does_not_use_stack () =
  let model, _cmd = init Repl () in
  let calc =
    { model.calc with stack =
        Pelzl_engine.stack_push
          (Pelzl_engine.RpcFloatUnit (1.0, Units.empty_unit))
          model.calc.Pelzl_engine.stack }
  in
  check (option string) "no preview" None (Pelzl_view.preview_for calc "1+")

let test_preview_does_not_mutate_stack () =
  let model, _cmd = init Repl () in
  let calc =
    { model.calc with stack =
        Pelzl_engine.stack_push
          (Pelzl_engine.RpcFloatUnit (99.0, Units.empty_unit))
          model.calc.Pelzl_engine.stack }
  in
  check (option string) "preview" (Some "1") (Pelzl_view.preview_for calc "1");
  check string "stack display unchanged" "99"
    (String.trim (Pelzl_engine.get_display_line 1 calc))

let key ?(modifier = Input.Key.no_modifier) k =
  Mosaic.Event.Key.of_input (Input.Key.make ~modifier k)

let raw_ctrl_char code =
  key (Input.Key.Char (Uchar.of_int code))

let ctrl_char ch =
  let modifier = { Input.Key.no_modifier with ctrl = true } in
  key ~modifier (Input.Key.Char (Uchar.of_char ch))

let cmd_is_quit = function
  | Mosaic.Cmd.Quit -> true
  | _ -> false

let test_raw_ctrl_q_quits_repl_even_with_entry () =
  let model, _cmd = init Repl () in
  let model = { model with entry = "123" } in
  let _model', cmd = update (Key_input (raw_ctrl_char 0x11)) model in
  check bool "quit command" true (cmd_is_quit cmd)

let test_uppercase_ctrl_q_quits_repl_even_with_entry () =
  let model, _cmd = init Repl () in
  let model = { model with entry = "123" } in
  let _model', cmd = update (Key_input (ctrl_char 'Q')) model in
  check bool "quit command" true (cmd_is_quit cmd)

let test_raw_ctrl_d_quits_repl_only_when_entry_empty () =
  let model, _cmd = init Repl () in
  let _model', cmd = update (Key_input (raw_ctrl_char 0x04)) model in
  check bool "empty entry quits" true (cmd_is_quit cmd);
  let model = { model with entry = "123" } in
  let model', cmd = update (Key_input (raw_ctrl_char 0x04)) model in
  check bool "non-empty entry does not quit" false (cmd_is_quit cmd);
  check string "entry preserved" "123" model'.entry

let test_uppercase_ctrl_d_quits_repl_only_when_entry_empty () =
  let model, _cmd = init Repl () in
  let _model', cmd = update (Key_input (ctrl_char 'D')) model in
  check bool "empty entry quits" true (cmd_is_quit cmd);
  let model = { model with entry = "123" } in
  let model', cmd = update (Key_input (ctrl_char 'D')) model in
  check bool "non-empty entry does not quit" false (cmd_is_quit cmd);
  check string "entry preserved" "123" model'.entry

let test_raw_ctrl_q_quits_classic () =
  let model, _cmd = init Classic () in
  let _model', cmd = update (Key_input (raw_ctrl_char 0x11)) model in
  check bool "classic quit command" true (cmd_is_quit cmd)

let ui_tests = [
  ("model init creates empty state", `Quick, test_init_empty);
  ("random slogan initialization", `Quick, test_random_slogan);
  ("update push entry on enter", `Quick, test_push_entry);
  ("update backspace removes char", `Quick, test_backspace);
  ("update clear error", `Quick, test_clear_error);
  ("update toggle help", `Quick, test_toggle_help);
  ("update resize changes dimensions", `Quick, test_resize);
  ("ui mode selection", `Quick, test_ui_modes);
  ("algebraic evaluation", `Quick, test_algebraic_eval);
  ("preview trailing operator does not use stack", `Quick, test_preview_trailing_operator_does_not_use_stack);
  ("preview does not mutate stack", `Quick, test_preview_does_not_mutate_stack);
  ("raw Ctrl-Q quits repl even with entry", `Quick, test_raw_ctrl_q_quits_repl_even_with_entry);
  ("uppercase Ctrl-Q quits repl even with entry", `Quick, test_uppercase_ctrl_q_quits_repl_even_with_entry);
  ("raw Ctrl-D quits repl only when empty", `Quick, test_raw_ctrl_d_quits_repl_only_when_entry_empty);
  ("uppercase Ctrl-D quits repl only when empty", `Quick, test_uppercase_ctrl_d_quits_repl_only_when_entry_empty);
  ("raw Ctrl-Q quits classic", `Quick, test_raw_ctrl_q_quits_classic);
]
