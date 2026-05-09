open Pelzl_engine
open Support_engine

let test_orpie_parity_mixed_scalar_arithmetic () =
  check_float "int + float" 20.0
    (top (calc_add (push_float 10.0 (push_int 10 empty_state))));
  check_complex "complex - int" 20.0 20.0
    (top (calc_sub (push_int 10 (push_complex 30.0 20.0 empty_state))));
  check_complex "float * complex" (-800.0) 160.0
    (top (calc_mult (push_complex 40.0 (-8.0) (push_float (-20.0) empty_state))));
  check_complex "complex / complex" (-0.2) 0.4
    (top (calc_div (push_complex 30.0 (-40.0) (push_complex 10.0 20.0 empty_state))))

let test_orpie_parity_matrix_add_sub_mult () =
  check_fmat "fmat + fmat"
    [[6.0; 8.0]; [10.0; 12.0]]
    (top
       (calc_add
          (push_fmat [[5.0; 6.0]; [7.0; 8.0]]
             (push_fmat [[1.0; 2.0]; [3.0; 4.0]] empty_state))));
  check_fmat "fmat - fmat"
    [[-4.0; -1.0]; [2.0; 6.0]]
    (top
       (calc_sub
          (push_fmat [[5.0; 6.0]; [7.0; 8.0]]
             (push_fmat [[1.0; 5.0]; [9.0; 14.0]] empty_state))));
  check_fmat "fmat * fmat"
    [[19.0; 22.0]; [43.0; 50.0]]
    (top
       (calc_mult
          (push_fmat [[5.0; 6.0]; [7.0; 8.0]]
             (push_fmat [[1.0; 2.0]; [3.0; 4.0]] empty_state))))

let test_orpie_parity_units_helpers_are_active () =
  Units.unit_table :=
    Units.add_base_unit "m" (Units.prefix_of_string "") Units.empty_unit_table;
  let meter = Units.units_of_string "m" !Units.unit_table in
  let centimeter = Units.units_of_string "cm" !Units.unit_table in
  let st =
    empty_state
    |> push (RpcFloatUnit (100.0, centimeter))
    |> push (RpcFloatUnit (1.0, meter))
  in
  check_float "100 cm + 1 m = 2 m" 2.0 (top (calc_add st))

let parity_tests =
  [
    ("orpie_parity: mixed scalar arithmetic", `Quick,
     test_orpie_parity_mixed_scalar_arithmetic);
    ("orpie_parity: matrix add/sub/mult", `Quick,
     test_orpie_parity_matrix_add_sub_mult);
    ("orpie_parity: units helper conversion", `Quick,
     test_orpie_parity_units_helpers_are_active);
  ]
