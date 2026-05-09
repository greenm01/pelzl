open Alcotest
open Pelzl_engine

let eps = 1e-9

let bi n = Big_int.big_int_of_int n

let c re im = { Complex.re; im }

let push value st = { st with stack = stack_push value st.stack }
let push_int n st = push (RpcInt (bi n)) st
let push_float f st = push (RpcFloatUnit (f, Units.empty_unit)) st
let push_complex re im st = push (RpcComplexUnit (c re im, Units.empty_unit)) st
let push_var name st = push (RpcVariable name) st

let fmat rows =
  let arr = Array.of_list (List.map Array.of_list rows) in
  Gsl.Matrix.of_arrays arr

let cmat rows =
  let arr = Array.of_list (List.map Array.of_list rows) in
  Gsl.Matrix_complex.of_arrays arr

let push_fmat rows st = push (RpcFloatMatrixUnit (fmat rows, Units.empty_unit)) st
let push_cmat rows st = push (RpcComplexMatrixUnit (cmat rows, Units.empty_unit)) st

let top st = stack_peek 1 st.stack
let second st = stack_peek 2 st.stack

let check_len label expected st =
  check int label expected (stack_length st.stack)

let check_int label expected = function
  | RpcInt i -> check int label expected (Big_int.int_of_big_int i)
  | _ -> fail (label ^ ": expected integer")

let check_float label expected = function
  | RpcFloatUnit (f, _) -> check (float eps) label expected f
  | _ -> fail (label ^ ": expected real")

let check_complex label expected_re expected_im = function
  | RpcComplexUnit (z, _) ->
      check (float eps) (label ^ " real") expected_re z.Complex.re;
      check (float eps) (label ^ " imag") expected_im z.Complex.im
  | _ -> fail (label ^ ": expected complex")

let check_fmat label expected = function
  | RpcFloatMatrixUnit (m, _) ->
      let rows = List.length expected in
      let cols = List.length (List.hd expected) in
      let got_rows, got_cols = Gsl.Matrix.dims m in
      check int (label ^ " rows") rows got_rows;
      check int (label ^ " cols") cols got_cols;
      List.iteri
        (fun r row ->
          List.iteri
            (fun c expected_value ->
              check (float eps)
                (Printf.sprintf "%s[%d,%d]" label r c)
                expected_value m.{r, c})
            row)
        expected
  | _ -> fail (label ^ ": expected real matrix")

let check_cmat label expected = function
  | RpcComplexMatrixUnit (m, _) ->
      let rows = List.length expected in
      let cols = List.length (List.hd expected) in
      let got_rows, got_cols = Gsl.Matrix_complex.dims m in
      check int (label ^ " rows") rows got_rows;
      check int (label ^ " cols") cols got_cols;
      List.iteri
        (fun r row ->
          List.iteri
            (fun c expected_value ->
              let actual = m.{r, c} in
              check (float eps)
                (Printf.sprintf "%s[%d,%d].re" label r c)
                expected_value.Complex.re actual.Complex.re;
              check (float eps)
                (Printf.sprintf "%s[%d,%d].im" label r c)
                expected_value.Complex.im actual.Complex.im)
            row)
        expected
  | _ -> fail (label ^ ": expected complex matrix")

