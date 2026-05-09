open Pelzl_engine
open Support_engine

let test_orpie_bug_fixed_complex_matrix_times_real_scalar () =
  let st =
    empty_state
    |> push_cmat [[c 1.0 2.0; c 3.0 4.0]]
    |> push_float 3.0
  in
  let st' = calc_mult st in
  check_cmat "cmat * real scalar"
    [[c 3.0 6.0; c 9.0 12.0]]
    (top st')

let test_orpie_bug_fixed_complex_matrix_times_complex_scalar () =
  let st =
    empty_state
    |> push_cmat [[c 1.0 2.0]]
    |> push_complex 3.0 4.0
  in
  let st' = calc_mult st in
  check_cmat "cmat * complex scalar"
    [[c (-5.0) 10.0]]
    (top st')

let improvement_tests =
  [
    ("orpie_bug_fixed: complex matrix times real scalar", `Quick,
     test_orpie_bug_fixed_complex_matrix_times_real_scalar);
    ("orpie_bug_fixed: complex matrix times complex scalar", `Quick,
     test_orpie_bug_fixed_complex_matrix_times_complex_scalar);
  ]
