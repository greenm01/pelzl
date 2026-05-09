open Alcotest
open Pelzl_engine

let test_simple_char () =
  let kb = decode_single_key_string "a" in
  check bool "ctrl" false kb.ctrl;
  check bool "meta" false kb.meta;
  match kb.key with
  | Key_char c -> check char "key" 'a' (Uchar.to_char c)
  | _ -> fail "expected Key_char"

let test_ctrl_char () =
  let kb = decode_single_key_string "\\Ca" in
  check bool "ctrl" true kb.ctrl;
  check bool "meta" false kb.meta;
  match kb.key with
  | Key_char c -> check int "key code" 1 (Uchar.to_int c)
  | _ -> fail "expected Key_char"

let test_meta_char () =
  let kb = decode_single_key_string "\\Mx" in
  check bool "ctrl" false kb.ctrl;
  check bool "meta" true kb.meta;
  match kb.key with
  | Key_char c -> check int "key code" 248 (Uchar.to_int c)
  | _ -> fail "expected Key_char"

let test_meta_ctrl_char () =
  let kb = decode_single_key_string "\\M\\Ca" in
  check bool "ctrl" true kb.ctrl;
  check bool "meta" true kb.meta;
  match kb.key with
  | Key_char c -> check int "key code" 129 (Uchar.to_int c)
  | _ -> fail "expected Key_char"

let test_alias_enter () =
  let kb = decode_single_key_string "<enter>" in
  check bool "ctrl" false kb.ctrl;
  match kb.key with
  | Key_enter -> ()
  | _ -> fail "expected Key_enter"

let test_alias_backspace () =
  let kb = decode_single_key_string "<backspace>" in
  match kb.key with
  | Key_backspace -> ()
  | _ -> fail "expected Key_backspace"

let test_alias_f12 () =
  let kb = decode_single_key_string "<f12>" in
  match kb.key with
  | Key_f 12 -> ()
  | _ -> fail "expected Key_f 12"

let test_alias_space () =
  let kb = decode_single_key_string "<space>" in
  match kb.key with
  | Key_space -> ()
  | _ -> fail "expected Key_space"

let test_alias_esc () =
  let kb = decode_single_key_string "<esc>" in
  match kb.key with
  | Key_escape -> ()
  | _ -> fail "expected Key_escape"

let test_roundtrip_simple () =
  let kb = { key = Key_char (Uchar.of_char 'q'); ctrl = false; meta = false } in
  check string "round-trip" "q" (string_of_key_binding kb)

let test_roundtrip_ctrl () =
  let kb = { key = Key_char (Uchar.of_char 'a'); ctrl = true; meta = false } in
  check string "round-trip" "\\Ca" (string_of_key_binding kb)

let test_roundtrip_meta () =
  let kb = { key = Key_char (Uchar.of_char 'x'); ctrl = false; meta = true } in
  check string "round-trip" "\\Mx" (string_of_key_binding kb)

let test_roundtrip_alias () =
  let kb = { key = Key_enter; ctrl = false; meta = false } in
  check string "round-trip" "<enter>" (string_of_key_binding kb)

let test_invalid_alias () =
  check_raises "invalid alias" (Invalid_argument "Unrecognized key \"<invalid>\"") (fun () ->
    ignore (decode_single_key_string "<invalid>"))

let test_empty_binding () =
  check_raises "empty binding" (Invalid_argument "empty key binding in \"\\M\"") (fun () ->
    ignore (decode_single_key_string "\\M"))

let binding_tests = [
  ("decode simple char", `Quick, test_simple_char);
  ("decode control char", `Quick, test_ctrl_char);
  ("decode meta char", `Quick, test_meta_char);
  ("decode meta-control char", `Quick, test_meta_ctrl_char);
  ("decode alias <enter>", `Quick, test_alias_enter);
  ("decode alias <backspace>", `Quick, test_alias_backspace);
  ("decode alias <f12>", `Quick, test_alias_f12);
  ("decode alias <space>", `Quick, test_alias_space);
  ("decode alias <esc>", `Quick, test_alias_esc);
  ("string round-trip simple", `Quick, test_roundtrip_simple);
  ("string round-trip ctrl", `Quick, test_roundtrip_ctrl);
  ("string round-trip meta", `Quick, test_roundtrip_meta);
  ("string round-trip alias", `Quick, test_roundtrip_alias);
  ("invalid alias raises", `Quick, test_invalid_alias);
  ("empty binding raises", `Quick, test_empty_binding);
]
