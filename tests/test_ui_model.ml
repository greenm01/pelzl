open Alcotest
open Pelzl_model
open Pelzl_update

let test_init_empty () =
  let model, _cmd = init () in
  check int "stack len" 0 (Pelzl_engine.stack_length model.calc.Pelzl_engine.stack);
  check string "entry" "" model.entry;
  check bool "show_help" false model.show_help;
  check (option string) "error" None model.error_msg

let test_push_entry () =
  let model, _cmd = init () in
  let model = { model with entry = "42" } in
  let model', _cmd = update Enter model in
  check string "entry cleared" "" model'.entry;
  check int "stack len" 1 (Pelzl_engine.stack_length model'.calc.Pelzl_engine.stack);
  check (option string) "no error" None model'.error_msg

let test_backspace () =
  let model, _cmd = init () in
  let model = { model with entry = "123" } in
  let model', _cmd = update Backspace model in
  check string "entry" "12" model'.entry

let test_clear_error () =
  let model, _cmd = init () in
  let model = { model with error_msg = Some "oops" } in
  let model', _cmd = update Clear_error model in
  check (option string) "error cleared" None model'.error_msg

let test_toggle_help () =
  let model, _cmd = init () in
  let model', _cmd = update Toggle_help model in
  check bool "help on" true model'.show_help;
  let model'', _cmd = update Toggle_help model' in
  check bool "help off" false model''.show_help

let test_resize () =
  let model, _cmd = init () in
  let model', _cmd = update (Resize (120, 40)) model in
  check int "width" 120 model'.width;
  check int "height" 40 model'.height

let ui_tests = [
  ("model init creates empty state", `Quick, test_init_empty);
  ("update push entry on enter", `Quick, test_push_entry);
  ("update backspace removes char", `Quick, test_backspace);
  ("update clear error", `Quick, test_clear_error);
  ("update toggle help", `Quick, test_toggle_help);
  ("update resize changes dimensions", `Quick, test_resize);
]
