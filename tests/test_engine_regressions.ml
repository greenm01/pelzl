open Alcotest
open Pelzl_engine
open Support_engine

let test_undo_restores_pre_operation_stack () =
  let st = push_float 3.0 (push_float 2.0 empty_state) in
  let st' = calc_add st in
  check_float "sum" 5.0 (top st');
  let restored = cmd_undo st' in
  check_len "restored length" 2 restored;
  check_float "restored top" 3.0 (top restored);
  check_float "restored second" 2.0 (second restored)

let test_complex_matrix_plus_float_matrix_adds () =
  let st =
    empty_state
    |> push_cmat [[c 1.0 2.0; c 3.0 4.0]]
    |> push_fmat [[10.0; 20.0]]
  in
  let st' = calc_add st in
  check_cmat "cmat+fmat"
    [[c 11.0 2.0; c 23.0 4.0]]
    (top st')

let test_matrix_inverse_real () =
  let st = push_fmat [[1.0; 2.0]; [3.0; 4.0]] empty_state in
  let st' = calc_inv st in
  check_fmat "inverse" [[-2.0; 1.0]; [1.5; -0.5]] (top st')

let test_factorial_zero_and_float_gamma () =
  check_int "0!" 1 (top (calc_fact (push_int 0 empty_state)));
  check_float "3.5!" 11.63172839656745
    (top (calc_fact (push_float 3.5 empty_state)))

let test_mod_matches_orpie_integer_coercion () =
  check_int "int mod real" 1
    (top (calc_mod (push_float 3.8 (push_int 10 empty_state))));
  check_int "real mod int" 1
    (top (calc_mod (push_int 3 (push_float 10.8 empty_state))));
  check_int "real mod real" 1
    (top (calc_mod (push_float 3.2 (push_float 10.8 empty_state))))

let test_to_int_and_to_float_parity () =
  check_int "float to int truncates" 3
    (top (calc_to_int (push_float 3.9 empty_state)));
  check_int "int to int passthrough" 7
    (top (calc_to_int (push_int 7 empty_state)));
  check_float "1x1 matrix to float" 42.0
    (top (calc_to_float (push_fmat [[42.0]] empty_state)))

let test_gcd_lcm_accept_real_and_negative_args () =
  check_int "gcd abs negative" 6
    (top (calc_gcd (push_int 18 (push_int (-48) empty_state))));
  check_int "gcd real truncation" 6
    (top (calc_gcd (push_float 18.8 (push_float 48.2 empty_state))));
  check_int "lcm preserves Orpie sign" (-144)
    (top (calc_lcm (push_int 18 (push_int (-48) empty_state))))

let test_extra_scalar_special_functions () =
  check_float "asinh int" (Gsl.Math.asinh 2.0)
    (top (calc_asinh (push_int 2 empty_state)));
  check_complex "asinh complex"
    (Gsl.Gsl_complex.arcsinh (c 1.0 2.0)).Complex.re
    (Gsl.Gsl_complex.arcsinh (c 1.0 2.0)).Complex.im
    (top (calc_asinh (push_complex 1.0 2.0 empty_state)));
  check_float "gamma int" 6.0
    (top (calc_gamma (push_int 4 empty_state)));
  check_float "erf int" (Gsl.Sf.erf 1.0)
    (top (calc_erf (push_int 1 empty_state)))

let test_invalid_store_and_eval_do_not_destroy_input_state () =
  let st = push_float 1.0 (push_float 2.0 empty_state) in
  check_raises "store into non-variable"
    (Invalid_argument "cannot store inside non-variable")
    (fun () -> ignore (cmd_store st));
  check_len "store input length" 2 st;
  let var_st = push_var "missing" empty_state in
  check_raises "unknown variable"
    (Invalid_argument "variable \"missing\" is not bound")
    (fun () -> ignore (cmd_eval var_st));
  check_len "eval input length" 1 var_st

let regression_tests =
  [
    ("undo restores pre-operation stack", `Quick, test_undo_restores_pre_operation_stack);
    ("complex matrix plus float matrix adds", `Quick, test_complex_matrix_plus_float_matrix_adds);
    ("real matrix inverse", `Quick, test_matrix_inverse_real);
    ("factorial zero and float gamma", `Quick, test_factorial_zero_and_float_gamma);
    ("mod integer coercion", `Quick, test_mod_matches_orpie_integer_coercion);
    ("toint and toreal parity", `Quick, test_to_int_and_to_float_parity);
    ("gcd/lcm real and negative args", `Quick, test_gcd_lcm_accept_real_and_negative_args);
    ("extra scalar special functions", `Quick, test_extra_scalar_special_functions);
    ("invalid store/eval preserve input", `Quick, test_invalid_store_and_eval_do_not_destroy_input_state);
  ]
