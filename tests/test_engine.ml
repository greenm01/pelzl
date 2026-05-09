open Alcotest
open Pelzl_engine

let empty = empty_state

let push_int n st =
  { st with stack = stack_push (RpcInt (Big_int.big_int_of_int n)) st.stack }

let push_float f st =
  { st with stack = stack_push (RpcFloatUnit (f, Units.empty_unit)) st.stack }

let get_float st =
  match stack_peek 1 st.stack with
  | RpcFloatUnit (f, _) -> f
  | _ -> 0.0

let get_int st =
  match stack_peek 1 st.stack with
  | RpcInt i -> Big_int.int_of_big_int i
  | _ -> 0

let test_add_floats () =
  let st = push_float 3.0 (push_float 2.0 empty) in
  let st' = calc_add st in
  check (float 0.0001) "sum" 5.0 (get_float st');
  check int "len after add" 1 (stack_length st'.stack)

let test_sub_floats () =
  let st = push_float 3.0 (push_float 5.0 empty) in
  let st' = calc_sub st in
  check (float 0.0001) "diff" 2.0 (get_float st')

let test_mult_floats () =
  let st = push_float 3.0 (push_float 4.0 empty) in
  let st' = calc_mult st in
  check (float 0.0001) "prod" 12.0 (get_float st')

let test_div_floats () =
  let st = push_float 2.0 (push_float 6.0 empty) in
  let st' = calc_div st in
  check (float 0.0001) "quot" 3.0 (get_float st')

let test_neg_float () =
  let st = push_float 5.0 empty in
  let st' = calc_neg st in
  check (float 0.0001) "neg" (-5.0) (get_float st')

let test_sq_float () =
  let st = push_float 3.0 empty in
  let st' = calc_sq st in
  check (float 0.0001) "sq" 9.0 (get_float st')

let test_sqrt_float () =
  let st = push_float 9.0 empty in
  let st' = calc_sqrt st in
  check (float 0.0001) "sqrt" 3.0 (get_float st')

let test_inv_float () =
  let st = push_float 2.0 empty in
  let st' = calc_inv st in
  check (float 0.0001) "inv" 0.5 (get_float st')

let test_abs_float () =
  let st = push_float (-7.0) empty in
  let st' = calc_abs st in
  check (float 0.0001) "abs" 7.0 (get_float st')

let test_add_ints () =
  let st = push_int 3 (push_int 2 empty) in
  let st' = calc_add st in
  check int "sum" 5 (get_int st')

let test_neg_int () =
  let st = push_int 42 empty in
  let st' = calc_neg st in
  check int "neg" (-42) (get_int st')

let test_fact_int () =
  let st = push_int 5 empty in
  let st' = calc_fact st in
  check int "fact" 120 (get_int st')

let test_gcd_int () =
  let st = push_int 18 (push_int 48 empty) in
  let st' = calc_gcd st in
  check int "gcd" 6 (get_int st')

let test_drop () =
  let st = push_float 1.0 (push_float 2.0 empty) in
  let st' = cmd_drop st in
  check int "len after drop" 1 (stack_length st'.stack);
  check (float 0.0001) "remaining" 2.0 (get_float st')

let test_dup () =
  let st = push_float 3.14 empty in
  let st' = cmd_dup st in
  check int "len after dup" 2 (stack_length st'.stack);
  check (float 0.0001) "top" 3.14 (get_float st')

let test_swap () =
  let st = push_float 1.0 (push_float 2.0 empty) in
  let st' = cmd_swap st in
  check (float 0.0001) "top after swap" 2.0 (get_float st');
  check (float 0.0001) "second after swap" 1.0 (match stack_peek 2 st'.stack with RpcFloatUnit (f, _) -> f | _ -> 0.0)

let test_clear () =
  let st = push_float 1.0 (push_float 2.0 empty) in
  let st' = cmd_clear st in
  check int "len after clear" 0 (stack_length st'.stack)

let test_toggle_angle () =
  let st = mode_rad empty in
  check bool "rad" true (match get_modes st with { angle = Rad; _ } -> true | _ -> false);
  let st' = toggle_angle_mode st in
  check bool "deg" true (match get_modes st' with { angle = Deg; _ } -> true | _ -> false)

let test_cycle_base () =
  let st = cycle_base (cycle_base (cycle_base empty)) in
  check bool "oct" true (match get_modes st with { base = Oct; _ } -> true | _ -> false)

let test_insufficient_args () =
  check_raises "add on empty" (Invalid_argument "empty stack") (fun () -> ignore (calc_add empty))

let test_variable_not_evaluated () =
  let st = { empty with stack = stack_push (RpcVariable "x") empty.stack } in
  check_raises "eval unknown var" (Invalid_argument "variable \"x\" has not been evaluated") (fun () -> ignore (eval1 st))

let engine_tests = [
  ("calc_add adds two floats", `Quick, test_add_floats);
  ("calc_sub subtracts two floats", `Quick, test_sub_floats);
  ("calc_mult multiplies two floats", `Quick, test_mult_floats);
  ("calc_div divides two floats", `Quick, test_div_floats);
  ("calc_neg negates float", `Quick, test_neg_float);
  ("calc_sq squares float", `Quick, test_sq_float);
  ("calc_sqrt square root", `Quick, test_sqrt_float);
  ("calc_inv reciprocal", `Quick, test_inv_float);
  ("calc_abs absolute value", `Quick, test_abs_float);
  ("calc_add adds two ints", `Quick, test_add_ints);
  ("calc_neg negates int", `Quick, test_neg_int);
  ("calc_fact factorial int", `Quick, test_fact_int);
  ("calc_gcd computes gcd", `Quick, test_gcd_int);
  ("cmd_drop removes top", `Quick, test_drop);
  ("cmd_dup duplicates top", `Quick, test_dup);
  ("cmd_swap exchanges top two", `Quick, test_swap);
  ("cmd_clear empties stack", `Quick, test_clear);
  ("mode toggle angle", `Quick, test_toggle_angle);
  ("mode cycle base", `Quick, test_cycle_base);
  ("insufficient args raises", `Quick, test_insufficient_args);
  ("variable not evaluated raises", `Quick, test_variable_not_evaluated);
]
