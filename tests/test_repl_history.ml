open Alcotest
open Pelzl_model
open Pelzl_update

let model_repl () =
  let m, _cmd = init Repl () in
  m

let contains_substring haystack needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with Not_found -> false

let with_history hist m =
  { m with history = hist; history_idx = None; history_save = "";
           entry_cursor = String.length m.entry }

(* ------------------------------------------------------------------ *)
(* trim_ws                                                             *)
(* ------------------------------------------------------------------ *)

let test_trim_ws () =
  check string "leading" "hello" (trim_ws "  hello");
  check string "trailing" "hello" (trim_ws "hello  ");
  check string "both" "hello" (trim_ws "  hello  ");
  check string "tabs" "hello" (trim_ws "\thello\t");
  check string "empty" "" (trim_ws "   ");
  check string "none" "hello" (trim_ws "hello")

(* ------------------------------------------------------------------ *)
(* handle_meta                                                        *)
(* ------------------------------------------------------------------ *)

let test_meta_quit () =
  let m = model_repl () in
  check (of_pp (fun ppf -> function
      | `Quit -> Format.pp_print_string ppf "quit"
      | `Commit _ -> Format.pp_print_string ppf "commit"
      | `Switch _ -> Format.pp_print_string ppf "switch"
      | `Unknown -> Format.pp_print_string ppf "unknown"))
    ":quit" `Quit (handle_meta m ":quit")

let test_meta_exit () =
  let m = model_repl () in
  match handle_meta m ":exit" with
  | `Quit -> ()
  | _ -> fail "expected `Quit"

let test_meta_rpn () =
  let m = model_repl () in
  match handle_meta m ":rpn" with
  | `Switch Classic -> ()
  | _ -> fail "expected `Switch Classic"

let test_meta_orpie_is_unknown () =
  let m = model_repl () in
  match handle_meta m ":orpie" with
  | `Commit (_m', Repl_msg txt) ->
      check string "unknown orpie" "  unknown command: :orpie" txt
  | _ -> fail "expected unknown command"

let test_meta_help () =
  let m = model_repl () in
  match handle_meta m ":help" with
  | `Commit (m', Repl_msg txt) ->
      check string "entry cleared" "" m'.entry;
      check bool "omits :rpn" false (contains_substring txt ":rpn");
      check bool "contains [Alt-R] RPN" true
        (contains_substring txt "[Alt-R] RPN");
      check bool "contains [Ctrl-D] Quit" true
        (contains_substring txt "[Ctrl-D] Quit");
      check bool "omits :exit" false (contains_substring txt ":exit");
      check bool "does not contain :orpie" false (contains_substring txt ":orpie")
  | _ -> fail "expected `Commit with Repl_msg"

let test_meta_unknown () =
  let m = model_repl () in
  match handle_meta m ":frobnicate" with
  | `Commit (m', Repl_msg txt) ->
      check string "entry cleared" "" m'.entry;
      check string "mentions unknown" "  unknown command: :frobnicate" txt
  | _ -> fail "expected `Commit with Repl_msg"

(* ------------------------------------------------------------------ *)
(* push_history                                                       *)
(* ------------------------------------------------------------------ *)

let test_push_history () =
  let m = with_history [] (model_repl ()) in
  let m1 = push_history m "1+1" in
  check (list string) "prepend" ["1+1"] m1.history;
  let m2 = push_history m1 "2+2" in
  check (list string) "prepend again" ["2+2"; "1+1"] m2.history;
  let m3 = push_history m2 "2+2" in
  check (list string) "dedup" ["2+2"; "1+1"] m3.history

let test_push_history_cap () =
  let m = model_repl () in
  let m = ref m in
  for i = 1 to 1005 do
    m := push_history !m (string_of_int i)
  done;
  check int "capped at 1000" 1000 (List.length !m.history);
  check string "most recent" "1005" (List.hd !m.history)

(* ------------------------------------------------------------------ *)
(* history_prev / history_next                                        *)
(* ------------------------------------------------------------------ *)

let test_history_nav () =
  let m = with_history ["c"; "b"; "a"] (model_repl ()) in
  let m = { m with entry = "live"; entry_cursor = 2 } in

  let m1 = history_prev m in
  check (option int) "idx 0" (Some 0) m1.history_idx;
  check string "entry c" "c" m1.entry;
  check int "cursor c end" 1 m1.entry_cursor;
  check string "save live" "live" m1.history_save;

  let m2 = history_prev m1 in
  check (option int) "idx 1" (Some 1) m2.history_idx;
  check string "entry b" "b" m2.entry;
  check int "cursor b end" 1 m2.entry_cursor;

  let m3 = history_next m2 in
  check (option int) "back to 0" (Some 0) m3.history_idx;
  check string "entry c" "c" m3.entry;
  check int "cursor c restored end" 1 m3.entry_cursor;

  let m4 = history_next m3 in
  check (option int) "back to live" None m4.history_idx;
  check string "restored live" "live" m4.entry;
  check int "cursor live end" 4 m4.entry_cursor;
  check string "save cleared" "" m4.history_save

let test_history_next_empty () =
  let m = with_history [] (model_repl ()) in
  let m' = history_prev m in
  check (option int) "no history" None m'.history_idx

let test_history_prev_bounds () =
  let m = with_history ["a"] (model_repl ()) in
  let m = { m with entry = ""; entry_cursor = 0 } in
  let m1 = history_prev m in
  let m2 = history_prev m1 in
  check (option int) "stops at last" (Some 0) m2.history_idx;
  check string "still a" "a" m2.entry

(* ------------------------------------------------------------------ *)
(* submit_repl                                                        *)
(* ------------------------------------------------------------------ *)

let test_submit_empty () =
  let m = model_repl () in
  let m', quit = submit_repl m "" in
  check bool "no quit" false quit;
  check string "entry cleared" "" m'.entry;
  check (option string) "no error" None m'.error_msg

let test_submit_algebraic () =
  let m = model_repl () in
  let m', quit = submit_repl m "1+2*3" in
  check bool "no quit" false quit;
  check string "entry cleared" "" m'.entry;
  let res = Pelzl_engine.get_display_line 1 m'.calc in
  check string "result" "7" (String.trim res)

let pp_repl_record ppf = function
  | Repl_ok r -> Format.fprintf ppf "Repl_ok(%S -> %S)" r.input r.result
  | Repl_err r -> Format.fprintf ppf "Repl_err(%S: %S)" r.input r.error
  | Repl_msg s -> Format.fprintf ppf "Repl_msg(%S)" s

let equal_repl_record a b = match a, b with
  | Repl_ok ra, Repl_ok rb -> ra.input = rb.input && ra.result = rb.result
  | Repl_err ra, Repl_err rb -> ra.input = rb.input && ra.error = rb.error
  | Repl_msg sa, Repl_msg sb -> sa = sb
  | _ -> false

let repl_record = testable pp_repl_record equal_repl_record

let test_submit_error () =
  let m = model_repl () in
  let m', quit = submit_repl m "nosuchvar + 1" in
  check bool "no quit" false quit;
  check string "entry cleared" "" m'.entry;
  check (option repl_record) "pending commit is error"
    (Some (Repl_err { input = "nosuchvar + 1"; error = "unknown variable: nosuchvar" }))
    m'.pending_commit

let test_submit_quit () =
  let m = model_repl () in
  let m', quit = submit_repl m ":quit" in
  check bool "quit" true quit;
  check string "entry cleared" "" m'.entry

let test_submit_rpn_switch () =
  let m = model_repl () in
  let m = { m with entry = ":rpn"; entry_cursor = 4; error_msg = Some "old" } in
  let m', action = submit_repl_action m ":rpn" in
  check bool "switch action" true
    (match action with Repl_switch Classic -> true | _ -> false);
  check bool "classic mode" true (m'.ui_mode = Classic);
  check string "entry cleared" "" m'.entry;
  check (option string) "error cleared" None m'.error_msg

(* ------------------------------------------------------------------ *)
(* bind_ans                                                           *)
(* ------------------------------------------------------------------ *)

let test_bind_ans () =
  let m = model_repl () in
  let m', _quit = submit_repl m "42" in
  let vars = Pelzl_engine.get_variables m'.calc in
  check bool "ans bound" true (Hashtbl.mem vars "ans");
  let ans_val = Hashtbl.find vars "ans" in
  let tmp_calc = Pelzl_engine.empty_state in
  let tmp_calc = { tmp_calc with stack = Pelzl_engine.stack_push ans_val tmp_calc.stack } in
  let line = Pelzl_engine.get_display_line 1 tmp_calc in
  check string "ans value" "42" (String.trim line)

(* ------------------------------------------------------------------ *)
(* Test list                                                          *)
(* ------------------------------------------------------------------ *)

let repl_history_tests = [
  ("trim_ws strips spaces and tabs", `Quick, test_trim_ws);
  ("meta :quit returns `Quit", `Quick, test_meta_quit);
  ("meta :exit returns `Quit", `Quick, test_meta_exit);
  ("meta :rpn requests classic switch", `Quick, test_meta_rpn);
  ("meta :orpie is unknown", `Quick, test_meta_orpie_is_unknown);
  ("meta :help returns help message", `Quick, test_meta_help);
  ("meta unknown command", `Quick, test_meta_unknown);
  ("push_history prepends and dedups", `Quick, test_push_history);
  ("push_history caps at 1000", `Quick, test_push_history_cap);
  ("history prev/next navigation", `Quick, test_history_nav);
  ("history next with empty history", `Quick, test_history_next_empty);
  ("history prev stops at last entry", `Quick, test_history_prev_bounds);
  ("submit empty clears entry", `Quick, test_submit_empty);
  ("submit algebraic evaluates", `Quick, test_submit_algebraic);
  ("submit error records Repl_err", `Quick, test_submit_error);
  ("submit :quit requests exit", `Quick, test_submit_quit);
  ("submit :rpn requests mode switch", `Quick, test_submit_rpn_switch);
  ("bind_ans stores result", `Quick, test_bind_ans);
]
