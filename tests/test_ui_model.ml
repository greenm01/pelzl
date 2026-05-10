open Alcotest
open Pelzl_model
open Pelzl_update

let test_init_empty () =
  let model, _cmd = init Repl () in
  check int "stack len" 0 (Pelzl_engine.stack_length model.calc.Pelzl_engine.stack);
  check string "entry" "" model.entry;
  check int "cursor" 0 model.entry_cursor;
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
  let model = { model with entry = "123"; entry_cursor = 3 } in
  let model', _cmd = update Backspace model in
  check string "entry" "12" model'.entry;
  check int "cursor" 2 model'.entry_cursor

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

let test_classic_initial_modes_match_orpie () =
  let model, _cmd = init Classic () in
  let modes = Pelzl_engine.get_modes model.calc in
  check bool "angle rad" true (modes.angle = Pelzl_engine.Rad);
  check bool "base dec" true (modes.base = Pelzl_engine.Dec);
  check bool "complex rect" true (modes.complex = Pelzl_engine.Rect)

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

let plain_char ch =
  key (Input.Key.Char (Uchar.of_char ch))

let contains_substring haystack needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with Not_found -> false

let stack_with_ints ints stack =
  List.fold_left
    (fun stack n ->
      Pelzl_engine.stack_push
        (Pelzl_engine.RpcInt (Big_int.big_int_of_int n))
        stack)
    stack ints

let classic_with_ints ints =
  let model, _cmd = init Classic () in
  let calc =
    { model.calc with
      Pelzl_engine.stack = stack_with_ints ints model.calc.Pelzl_engine.stack }
  in
  { model with calc }

let top_display model =
  String.trim (Pelzl_engine.get_display_line 1 model.calc)

let top_int model =
  match Pelzl_engine.stack_peek 1 model.calc.Pelzl_engine.stack with
  | Pelzl_engine.RpcInt i -> Big_int.int_of_big_int i
  | _ -> fail "expected integer on stack"

let cmd_is_quit = function
  | Mosaic.Cmd.Quit -> true
  | _ -> false

let test_classic_enter_pushes_entry () =
  let model, _cmd = init Classic () in
  let model = { model with entry = "42"; entry_cursor = 2 } in
  let model, _cmd = update (Key_input (key Input.Key.Enter)) model in
  check string "entry pushed" "42" (top_display model);
  check string "entry cleared" "" model.entry;
  check int "cursor reset" 0 model.entry_cursor

let test_classic_drop_key_drops () =
  let model = classic_with_ints [42] in
  let model, _cmd = update (Key_input (plain_char '\\')) model in
  check int "stack len" 0 (Pelzl_engine.stack_length model.calc.Pelzl_engine.stack)

let test_classic_pagedown_swaps () =
  let model = classic_with_ints [1; 2] in
  let model, _cmd = update (Key_input (key Input.Key.Page_down)) model in
  check int "top after swap" 1 (top_int model)

let test_classic_backspace_key_backspaces () =
  let model, _cmd = init Classic () in
  let model = { model with entry = "12"; entry_cursor = 2 } in
  let model, _cmd = update (Key_input (key Input.Key.Backspace)) model in
  check string "backspace entry" "1" model.entry;
  check int "backspace cursor" 1 model.entry_cursor

let test_classic_del_backspaces () =
  let model, _cmd = init Classic () in
  let model = { model with entry = "12"; entry_cursor = 2 } in
  let model, _cmd = update (Key_input (raw_ctrl_char 0x7f)) model in
  check string "del/backspace entry" "1" model.entry;
  check int "del/backspace cursor" 1 model.entry_cursor

let test_classic_space_enters_scientific_notation () =
  let model, _cmd = init Classic () in
  let model = { model with entry = "1"; entry_cursor = 1 } in
  let model, _cmd = update (Key_input (plain_char ' ')) model in
  check string "scientific notation marker" "1e" model.entry;
  check int "cursor" 2 model.entry_cursor

let test_classic_arithmetic_keys () =
  let apply ints ch =
    let model, _cmd = update (Key_input (plain_char ch)) (classic_with_ints ints) in
    top_int model
  in
  check int "add" 5 (apply [2; 3] '+');
  check int "subtract" (-1) (apply [2; 3] '-');
  check int "multiply" 6 (apply [2; 3] '*');
  check int "divide" 2 (apply [6; 3] '/');
  check int "power" 8 (apply [2; 3] '^');
  check int "negate" (-5) (apply [5] 'n')

let test_classic_refresh_keys_are_accepted () =
  let assert_refresh ev label =
    let model = classic_with_ints [9] in
    let model, cmd = update (Key_input ev) model in
    check bool (label ^ " no quit") false (cmd_is_quit cmd);
    check int (label ^ " preserves stack") 9 (top_int model);
    check (option string) (label ^ " no error") None model.error_msg
  in
  assert_refresh (ctrl_char 'l') "modified ctrl-l";
  assert_refresh (raw_ctrl_char 0x0c) "raw ctrl-l"

let test_classic_q_quits () =
  let model, _cmd = init Classic () in
  let _model, cmd = update (Key_input (plain_char 'Q')) model in
  check bool "Q quits" true (cmd_is_quit cmd)

let test_classic_misc_keys_are_bound () =
  let _model, _cmd = init Classic () in
  let command key =
    Rcfile.command_of_key (Pelzl_engine.decode_single_key_string key)
  in
  check bool "apostrophe starts abbrev" true
    (command "'" = Operations.BeginAbbrev);
  check bool "up starts browse" true
    (command "<up>" = Operations.BeginBrowse);
  check bool "ctrl-l refresh" true
    (command "\\Cl" = Operations.Refresh)

let test_repl_cursor_movement_keys () =
  let model, _cmd = init Repl () in
  let model = { model with entry = "abcd"; entry_cursor = 2 } in
  let model, _ = update (Key_input (key Input.Key.Left)) model in
  check int "left" 1 model.entry_cursor;
  let model, _ = update (Key_input (key Input.Key.Right)) model in
  check int "right" 2 model.entry_cursor;
  let model, _ = update (Key_input (key Input.Key.Home)) model in
  check int "home" 0 model.entry_cursor;
  let model, _ = update (Key_input (key Input.Key.End)) model in
  check int "end" 4 model.entry_cursor

let test_repl_delete_and_middle_insert () =
  let model, _cmd = init Repl () in
  let model = { model with entry = "abcd"; entry_cursor = 2 } in
  let model, _ = update (Key_input (key Input.Key.Delete)) model in
  check string "delete at cursor" "abd" model.entry;
  check int "cursor after delete" 2 model.entry_cursor;
  let model, _ = update (Key_input (plain_char 'X')) model in
  check string "insert at cursor" "abXd" model.entry;
  check int "cursor after insert" 3 model.entry_cursor

let test_repl_backspace_middle () =
  let model, _cmd = init Repl () in
  let model = { model with entry = "abcd"; entry_cursor = 2 } in
  let model, _ = update (Key_input (key Input.Key.Backspace)) model in
  check string "backspace before cursor" "acd" model.entry;
  check int "cursor after backspace" 1 model.entry_cursor

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

let test_repl_colon_quit_enter_returns_quit () =
  let model, _cmd = init Repl () in
  let model = { model with entry = ":quit"; entry_cursor = 5 } in
  let _model', cmd = update (Key_input (key Input.Key.Enter)) model in
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

let test_classic_view_rows_are_fixed_width () =
  let model = { (classic_with_ints [1; 2; 3]) with height = 7 } in
  let help_rows = Pelzl_view.classic_help_rows model 38 5 in
  let stack_rows = Pelzl_view.classic_stack_rows model 42 5 in
  check int "help rows" 5 (List.length help_rows);
  check int "stack rows" 5 (List.length stack_rows);
  List.iter
    (fun row ->
      check int "help row width" 38 (String.length row);
      check bool "help row no newline" false (String.contains row '\n'))
    help_rows;
  List.iter
    (fun (_selected, row) ->
      check int "stack row width" 42 (String.length row);
      check bool "stack row no newline" false (String.contains row '\n'))
    stack_rows

let test_classic_browse_view_marks_one_fixed_row () =
  let model = classic_with_ints [1; 2; 3] in
  let model, _ = update (Key_input (key Input.Key.Up)) model in
  let model, _ = update (Key_input (key Input.Key.Up)) model in
  let rows = Pelzl_view.classic_stack_rows model 42 5 in
  let selected =
    rows |> List.filter (fun (is_selected, _row) -> is_selected)
  in
  check int "one selected row" 1 (List.length selected);
  match selected with
  | [(_selected, row)] ->
      check bool "selected row is level 2" true (contains_substring row "|  2:");
      check bool "selected row includes value" true (contains_substring row "# 2`d")
  | _ -> fail "expected one selected row"

let test_classic_modal_help_is_clipped_to_panel () =
  let model =
    { (classic_with_ints []) with
      height = 10;
      classic_mode = ClassicAbbrev OperationAbbrev;
      entry = "s";
      entry_cursor = 1 }
  in
  let rows = Pelzl_view.classic_help_rows model 38 8 in
  check int "row count" 8 (List.length rows);
  List.iter (fun row -> check int "row width" 38 (String.length row)) rows

let ui_tests = [
  ("model init creates empty state", `Quick, test_init_empty);
  ("random slogan initialization", `Quick, test_random_slogan);
  ("update push entry on enter", `Quick, test_push_entry);
  ("update backspace removes char", `Quick, test_backspace);
  ("repl cursor movement keys", `Quick, test_repl_cursor_movement_keys);
  ("repl delete and middle insert", `Quick, test_repl_delete_and_middle_insert);
  ("repl backspace in middle", `Quick, test_repl_backspace_middle);
  ("update clear error", `Quick, test_clear_error);
  ("update toggle help", `Quick, test_toggle_help);
  ("update resize changes dimensions", `Quick, test_resize);
  ("ui mode selection", `Quick, test_ui_modes);
  ("classic starts in Orpie modes", `Quick, test_classic_initial_modes_match_orpie);
  ("classic enter pushes entry", `Quick, test_classic_enter_pushes_entry);
  ("classic drop key drops", `Quick, test_classic_drop_key_drops);
  ("classic pagedown swaps", `Quick, test_classic_pagedown_swaps);
  ("classic backspace key backspaces", `Quick, test_classic_backspace_key_backspaces);
  ("classic del backspaces", `Quick, test_classic_del_backspaces);
  ("classic space enters scientific notation", `Quick, test_classic_space_enters_scientific_notation);
  ("classic arithmetic keys", `Quick, test_classic_arithmetic_keys);
  ("classic refresh keys accepted", `Quick, test_classic_refresh_keys_are_accepted);
  ("classic Q quits", `Quick, test_classic_q_quits);
  ("classic misc keys are bound", `Quick, test_classic_misc_keys_are_bound);
  ("algebraic evaluation", `Quick, test_algebraic_eval);
  ("preview trailing operator does not use stack", `Quick, test_preview_trailing_operator_does_not_use_stack);
  ("preview does not mutate stack", `Quick, test_preview_does_not_mutate_stack);
  ("raw Ctrl-Q quits repl even with entry", `Quick, test_raw_ctrl_q_quits_repl_even_with_entry);
  ("uppercase Ctrl-Q quits repl even with entry", `Quick, test_uppercase_ctrl_q_quits_repl_even_with_entry);
  ("repl :quit enter returns quit", `Quick, test_repl_colon_quit_enter_returns_quit);
  ("raw Ctrl-D quits repl only when empty", `Quick, test_raw_ctrl_d_quits_repl_only_when_entry_empty);
  ("uppercase Ctrl-D quits repl only when empty", `Quick, test_uppercase_ctrl_d_quits_repl_only_when_entry_empty);
  ("raw Ctrl-Q quits classic", `Quick, test_raw_ctrl_q_quits_classic);
  ("classic view rows are fixed width", `Quick, test_classic_view_rows_are_fixed_width);
  ("classic browse view marks one fixed row", `Quick, test_classic_browse_view_marks_one_fixed_row);
  ("classic modal help is clipped to panel", `Quick, test_classic_modal_help_is_clipped_to_panel);
]
