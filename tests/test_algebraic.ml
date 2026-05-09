open Alcotest
open Pelzl_algebraic

let calc () = Pelzl_engine.empty_state

let display_of = function
  | Ok (_, s) -> s
  | Error e -> Printf.sprintf "ERROR(%s)" (pp_error e)

let trim s = String.trim s

let test_simple_arith () =
  let r = run (calc ()) "1+2*3" in
  check string "1+2*3" "7" (trim (display_of r))

let test_parens () =
  let r = run (calc ()) "(1+2)*3" in
  check string "(1+2)*3" "9" (trim (display_of r))

let test_implicit_mul () =
  let r = run (calc ()) "2(3+4)" in
  check string "2(3+4)" "14" (trim (display_of r))

let test_pow_right_assoc () =
  let r = run (calc ()) "2^3^2" in
  check string "2^3^2" "512" (trim (display_of r))

let test_function_call () =
  let r = run (calc ()) "sqrt(16)" in
  check string "sqrt(16)" "4" (trim (display_of r))

let test_assignment_and_use () =
  let c = calc () in
  let r1 = run c "x = 5" in
  let c1 = match r1 with Ok (c, _) -> c | _ -> failwith "assign failed" in
  let r2 = run c1 "x*2" in
  check string "x*2 after x=5" "10" (trim (display_of r2))

let test_unknown_var () =
  match run (calc ()) "nope+1" with
  | Error (E_unknown_var "nope") -> ()
  | _ -> failwith "expected unknown variable error"

let test_unknown_fun () =
  match run (calc ()) "frob(2)" with
  | Error (E_unknown_fun "frob") -> ()
  | _ -> failwith "expected unknown function error"

let test_lex_error () =
  match run (calc ()) "1 $ 2" with
  | Error (E_lex _) -> ()
  | _ -> failwith "expected lex error"

let test_parse_error_unclosed () =
  match run (calc ()) "(1+2" with
  | Error (E_parse _) -> ()
  | _ -> failwith "expected parse error for unclosed paren"

let test_empty_input () =
  match parse "" with
  | Ok _ ->
      (match run (calc ()) "" with
       | Error (E_parse _) -> ()
       | _ -> failwith "expected error on empty expression")
  | Error _ -> ()

let test_int_base_suffix () =
  let r = run (calc ()) "ffh" in
  check string "ffh" "# 255`d" (trim (display_of r))

let test_division () =
  let r = run (calc ()) "10/4" in
  check string "10/4" "2.5" (trim (display_of r))

let algebraic_tests = [
  ("simple arithmetic", `Quick, test_simple_arith);
  ("parentheses", `Quick, test_parens);
  ("implicit multiplication", `Quick, test_implicit_mul);
  ("right-assoc power", `Quick, test_pow_right_assoc);
  ("function call", `Quick, test_function_call);
  ("assignment and use", `Quick, test_assignment_and_use);
  ("unknown variable error", `Quick, test_unknown_var);
  ("unknown function error", `Quick, test_unknown_fun);
  ("lex error", `Quick, test_lex_error);
  ("unclosed paren", `Quick, test_parse_error_unclosed);
  ("empty input", `Quick, test_empty_input);
  ("integer base suffix h", `Quick, test_int_base_suffix);
  ("division", `Quick, test_division);
]
