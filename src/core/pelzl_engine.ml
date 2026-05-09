(*  Pelzl -- a modern RPN calculator for the console
 *  Copyright (C) 2003-2004, 2005, 2006-2007, 2010, 2018 Paul Pelzl
 *  Copyright (C) 2026 Mason Austin Green
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License, Version 3,
 *  as published by the Free Software Foundation.
 *)

open Big_int
open Printf

exception Stack_error of string
let stack_failwith s = raise (Stack_error s)

(**********************************************************************)
(* TYPES                                                              *)
(**********************************************************************)

type angle_mode   = | Rad | Deg
type base_mode    = | Bin | Oct | Hex | Dec
type complex_mode = | Rect | Polar

type calculator_modes = {
  angle   : angle_mode;
  base    : base_mode;
  complex : complex_mode
}

type display_mode = | Line | Fullscreen

type pelzl_data_t =
  | RpcInt                of big_int
  | RpcFloatUnit          of float * Units.unit_set_t
  | RpcComplexUnit        of Complex.t * Units.unit_set_t
  | RpcFloatMatrixUnit    of Gsl.Matrix.matrix * Units.unit_set_t
  | RpcComplexMatrixUnit  of Gsl.Matrix_complex.matrix * Units.unit_set_t
  | RpcVariable           of string

let default_float = RpcFloatUnit (0.0, Units.empty_unit)

type stack_state = {
  data : pelzl_data_t array;
  len  : int;
}

type calc_state = {
  stack     : stack_state;
  modes     : calculator_modes;
  variables : (string, pelzl_data_t) Hashtbl.t;
  backup    : calc_state option;
}

(**********************************************************************)
(* STACK PRIMITIVES                                                   *)
(**********************************************************************)

let stack_size_inc = 100

let empty_stack = {
  data = Array.make stack_size_inc default_float;
  len = 0
}

let stack_length st = st.len

let stack_push v st =
  let new_data =
    if st.len >= Array.length st.data then
      let nd = Array.make (Array.length st.data + stack_size_inc) default_float in
      Array.blit st.data 0 nd 0 (Array.length st.data);
      nd
    else
      st.data
  in
  new_data.(st.len) <- v;
  { data = new_data; len = st.len + 1 }

let stack_pop st =
  if st.len > 0 then
    (st.data.(st.len - 1), { data = st.data; len = st.len - 1 })
  else
    stack_failwith "cannot pop empty stack"

let stack_peek n st =
  if n > 0 && n <= st.len then
    st.data.(st.len - n)
  else
    let s = sprintf "cannot access nonexistent stack element %d" n in
    stack_failwith s

let stack_dup st =
  if st.len > 0 then
    stack_push st.data.(st.len - 1) st
  else
    stack_failwith "cannot dup with empty stack"

let stack_swap st =
  if st.len > 1 then
    let temp = st.data.(st.len - 1) in
    let nd = Array.copy st.data in
    nd.(st.len - 1) <- nd.(st.len - 2);
    nd.(st.len - 2) <- temp;
    { st with data = nd }
  else
    stack_failwith "cannot swap with less than two elements"

let stack_rolldown num st =
  if num <= st.len then
    let temp = st.data.(st.len - 1) in
    let nd = Array.copy st.data in
    for i = st.len - 1 downto st.len - num + 1 do
      nd.(i) <- nd.(i - 1)
    done;
    nd.(st.len - num) <- temp;
    { st with data = nd }
  else
    stack_failwith "insufficient stack elements"

let stack_rollup num st =
  if num <= st.len then
    let temp = st.data.(st.len - num) in
    let nd = Array.copy st.data in
    for i = st.len - num to st.len - 2 do
      nd.(i) <- nd.(i + 1)
    done;
    nd.(st.len - 1) <- temp;
    { st with data = nd }
  else
    stack_failwith "insufficient stack elements"

let stack_delete num st =
  if num <= st.len then
    let nd = Array.copy st.data in
    for i = st.len - num to st.len - 2 do
      nd.(i) <- nd.(i + 1)
    done;
    { data = nd; len = st.len - 1 }
  else
    stack_failwith "insufficient stack elements"

let stack_deleteN num st =
  if num <= st.len then
    { st with len = st.len - num }
  else
    stack_failwith "insufficient stack elements"

let stack_keep num st =
  if num <= st.len then
    let nd = Array.copy st.data in
    nd.(0) <- st.data.(st.len - num);
    { data = nd; len = 1 }
  else
    stack_failwith "insufficient stack elements"

let stack_keepN num st =
  if num <= st.len then
    let nd = Array.copy st.data in
    for i = 0 to num - 1 do
      nd.(i) <- st.data.(i + st.len - num)
    done;
    { data = nd; len = num }
  else
    stack_failwith "insufficient stack elements"

let stack_echo el_num st =
  if el_num <= st.len then
    let actual = st.len - el_num in
    stack_push st.data.(actual) st
  else
    raise (Invalid_argument "cannot echo nonexistant element")

(**********************************************************************)
(* CALC STATE                                                         *)
(**********************************************************************)

let empty_state = {
  stack = empty_stack;
  modes = { angle = Rad; base = Dec; complex = Rect };
  variables = Hashtbl.create 10;
  backup = None;
}

let state_backup st = { st with backup = Some st }
let state_restore st =
  match st.backup with
  | Some b -> { b with backup = st.backup }
  | None -> st

let with_stack f st = { st with stack = f st.stack }

let mode_rad st = { st with modes = { angle = Rad; base = st.modes.base; complex = st.modes.complex } }
let mode_deg st = { st with modes = { angle = Deg; base = st.modes.base; complex = st.modes.complex } }
let mode_rect st = { st with modes = { angle = st.modes.angle; base = st.modes.base; complex = Rect } }
let mode_polar st = { st with modes = { angle = st.modes.angle; base = st.modes.base; complex = Polar } }
let mode_bin st = { st with modes = { angle = st.modes.angle; base = Bin; complex = st.modes.complex } }
let mode_oct st = { st with modes = { angle = st.modes.angle; base = Oct; complex = st.modes.complex } }
let mode_dec st = { st with modes = { angle = st.modes.angle; base = Dec; complex = st.modes.complex } }
let mode_hex st = { st with modes = { angle = st.modes.angle; base = Hex; complex = st.modes.complex } }

let toggle_angle_mode st =
  match st.modes.angle with
  | Rad -> mode_deg st
  | Deg -> mode_rad st

let toggle_complex_mode st =
  match st.modes.complex with
  | Rect -> mode_polar st
  | Polar -> mode_rect st

let cycle_base st =
  match st.modes.base with
  | Bin -> mode_oct st
  | Oct -> mode_dec st
  | Dec -> mode_hex st
  | Hex -> mode_bin st

let get_modes st = st.modes
let set_modes m st = { st with modes = m }

(**********************************************************************)
(* HELPERS                                                            *)
(**********************************************************************)

let pi = 3.14159265358979323846

let c_of_f ff = { Complex.re = ff; Complex.im = 0.0 }
let funit_of_float ff = (ff, Units.empty_unit)
let cunit_of_cpx cc = (cc, Units.empty_unit)
let cmpx_of_int i = { Complex.re = float_of_big_int i; Complex.im = 0.0 }
let cmpx_of_float f = { Complex.re = f; Complex.im = 0.0 }

let has_units uu = uu <> Units.empty_unit

let check_args n fn_str st =
  if stack_length st.stack >= n then ()
  else if stack_length st.stack = 0 then
    raise (Invalid_argument "empty stack")
  else
    raise (Invalid_argument ("insufficient arguments for " ^ fn_str))

let pop2 st =
  let el2, st1 = stack_pop st.stack in
  let el1, st0 = stack_pop st1 in
  (el1, el2, { st with stack = st0 })

let pop1 st =
  let el, s = stack_pop st.stack in
  (el, { st with stack = s })

let push1 v st = { st with stack = stack_push v st.stack }

(**********************************************************************)
(* EVAL                                                               *)
(**********************************************************************)

let eval1 st =
  let el, s = stack_pop st.stack in
  match el with
  | RpcVariable vname ->
      begin try
        let value = Hashtbl.find st.variables vname in
        { st with stack = stack_push value s }
      with Not_found ->
        raise (Invalid_argument ("variable \"" ^ vname ^ "\" has not been evaluated"))
      end
  | _ -> { st with stack = stack_push el s }

let rec evaln n st =
  if n <= 0 then st else evaln (n - 1) (eval1 st)

let pop1_eval st =
  let st_eval = evaln 1 (state_backup st) in
  let el, st' = pop1 st_eval in
  (el, st')

let pop2_eval st =
  let st_eval = evaln 2 (state_backup st) in
  let el2, st1 = pop1 st_eval in
  let el1, st0 = pop1 st1 in
  (el1, el2, st0)

let pop3_eval st =
  let st_eval = evaln 3 (state_backup st) in
  let el3, st2 = pop1 st_eval in
  let el2, st1 = pop1 st2 in
  let el1, st0 = pop1 st1 in
  (el1, el2, el3, st0)

(**********************************************************************)
(* UNARY OPERATIONS                                                   *)
(**********************************************************************)

let calc_neg st =
  check_args 1 "neg" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcInt i -> push1 (RpcInt (minus_big_int i)) st'
  | RpcFloatUnit (f, u) -> push1 (RpcFloatUnit (~-. f, u)) st'
  | RpcComplexUnit (c, u) -> push1 (RpcComplexUnit (Complex.neg c, u)) st'
  | RpcFloatMatrixUnit (m, u) ->
      let copy = Gsl.Matrix.copy m in
      Gsl.Matrix.scale copy (-1.0);
      push1 (RpcFloatMatrixUnit (copy, u)) st'
  | RpcComplexMatrixUnit (m, u) ->
      let copy = Gsl.Matrix_complex.copy m in
      Gsl.Matrix_complex.scale copy { Complex.re = (-1.0); Complex.im = 0.0 };
      push1 (RpcComplexMatrixUnit (copy, u)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_inv st =
  check_args 1 "inv" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatUnit (f, u) -> push1 (RpcFloatUnit (1.0 /. f, Units.div Units.empty_unit u)) st'
  | RpcComplexUnit (c, u) -> push1 (RpcComplexUnit (Complex.inv c, Units.div Units.empty_unit u)) st'
  | _ -> raise (Invalid_argument "inversion is undefined for this data type")

let calc_sqrt st =
  check_args 1 "sqrt" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatUnit (f, u) ->
      if f < 0.0 then
        push1 (RpcComplexUnit (Complex.sqrt (c_of_f f), Units.pow u 0.5)) st'
      else
        push1 (RpcFloatUnit (sqrt f, Units.pow u 0.5)) st'
  | RpcComplexUnit (c, u) -> push1 (RpcComplexUnit (Complex.sqrt c, Units.pow u 0.5)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_sq st =
  check_args 1 "sq" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcInt i -> push1 (RpcInt (mult_big_int i i)) st'
  | RpcFloatUnit (f, u) -> push1 (RpcFloatUnit (f *. f, Units.pow u 2.0)) st'
  | RpcComplexUnit (c, u) -> push1 (RpcComplexUnit (Complex.mul c c, Units.pow u 2.0)) st'
  | RpcFloatMatrixUnit (m, u) ->
      let n, m' = Gsl.Matrix.dims m in
      if n = m' then
        let result = Gsl.Matrix.create n m' in
        Gsl.Blas.gemm ~ta:Gsl.Blas.NoTrans ~tb:Gsl.Blas.NoTrans
          ~alpha:1.0 ~a:m ~b:m ~beta:0.0 ~c:result;
        push1 (RpcFloatMatrixUnit (result, Units.pow u 2.0)) st'
      else
        raise (Invalid_argument "matrix is non-square")
  | RpcComplexMatrixUnit (m, u) ->
      let n, m' = Gsl.Matrix_complex.dims m in
      if n = m' then
        let result = Gsl.Matrix_complex.create n m' in
        Gsl.Blas.Complex.gemm ~ta:Gsl.Blas.NoTrans ~tb:Gsl.Blas.NoTrans
          ~alpha:Complex.one ~a:m ~b:m ~beta:Complex.zero ~c:result;
        push1 (RpcComplexMatrixUnit (result, Units.pow u 2.0)) st'
      else
        raise (Invalid_argument "matrix is non-square")
  | _ -> raise (Invalid_argument "invalid argument")

let calc_abs st =
  check_args 1 "abs" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcInt i -> push1 (RpcInt (abs_big_int i)) st'
  | RpcFloatUnit (f, u) -> push1 (RpcFloatUnit (abs_float f, u)) st'
  | RpcComplexUnit (c, u) -> push1 (RpcFloatUnit (Gsl.Gsl_complex.abs c, u)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_arg st =
  check_args 1 "arg" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcComplexUnit (c, u) ->
      let f =
        match st'.modes.angle with
        | Rad -> Gsl.Gsl_complex.arg c
        | Deg -> 180.0 /. pi *. Gsl.Gsl_complex.arg c
      in
      push1 (RpcFloatUnit (f, funit_of_float f |> snd)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_exp st =
  check_args 1 "exp" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcInt i -> push1 (RpcFloatUnit (exp (float_of_big_int i), Units.empty_unit)) st'
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot exponentiate dimensioned value")
      else push1 (RpcFloatUnit (exp f, Units.empty_unit)) st'
  | RpcComplexUnit (c, u) ->
      if has_units u then raise (Invalid_argument "cannot exponentiate dimensioned value")
      else push1 (RpcComplexUnit (Gsl.Gsl_complex.exp c, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_ln st =
  check_args 1 "ln" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcInt i -> push1 (RpcFloatUnit (log (float_of_big_int i), Units.empty_unit)) st'
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute logarithm of dimensioned value")
      else if f >= 0.0 then push1 (RpcFloatUnit (log f, Units.empty_unit)) st'
      else push1 (RpcComplexUnit (Gsl.Gsl_complex.log (c_of_f f), Units.empty_unit)) st'
  | RpcComplexUnit (c, u) ->
      if has_units u then raise (Invalid_argument "cannot compute logarithm of dimensioned value")
      else push1 (RpcComplexUnit (Gsl.Gsl_complex.log c, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_ten_x st =
  check_args 1 "10^x" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcInt i -> push1 (RpcFloatUnit (10.0 ** (float_of_big_int i), Units.empty_unit)) st'
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot exponentiate dimensioned value")
      else push1 (RpcFloatUnit (10.0 ** f, Units.empty_unit)) st'
  | RpcComplexUnit (c, u) ->
      if has_units u then raise (Invalid_argument "cannot exponentiate dimensioned value")
      else push1 (RpcComplexUnit (Complex.pow (cmpx_of_float 10.0) c, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_log10 st =
  check_args 1 "log10" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcInt i -> push1 (RpcFloatUnit (log10 (float_of_big_int i), Units.empty_unit)) st'
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute logarithm of dimensioned value")
      else if f >= 0.0 then push1 (RpcFloatUnit (log10 f, Units.empty_unit)) st'
      else push1 (RpcComplexUnit (Gsl.Gsl_complex.log10 (c_of_f f), Units.empty_unit)) st'
  | RpcComplexUnit (c, u) ->
      if has_units u then raise (Invalid_argument "cannot compute logarithm of dimensioned value")
      else push1 (RpcComplexUnit (Gsl.Gsl_complex.log10 c, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_conj st =
  check_args 1 "conj" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcInt i -> push1 (RpcInt i) st'
  | RpcFloatUnit (f, u) -> push1 (RpcFloatUnit (f, u)) st'
  | RpcComplexUnit (c, u) -> push1 (RpcComplexUnit (Gsl.Gsl_complex.conjugate c, u)) st'
  | RpcFloatMatrixUnit (m, u) -> push1 (RpcFloatMatrixUnit (m, u)) st'
  | RpcComplexMatrixUnit (m, u) ->
      let rows, cols = Gsl.Matrix_complex.dims m in
      let arr = Gsl.Matrix_complex.to_array m in
      let conj_arr = Array.map Gsl.Gsl_complex.conjugate arr in
      let conj_mat = Gsl.Matrix_complex.of_array conj_arr rows cols in
      push1 (RpcComplexMatrixUnit (conj_mat, u)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let apply_trig fn_rad fn_deg fn_complex st =
  check_args 1 "trig" st;
  let el, st' = pop1_eval st in
  let angle_mult =
    match st'.modes.angle with
    | Rad -> 1.0
    | Deg -> pi /. 180.0
  in
  match el with
  | RpcInt i ->
      let f = fn_rad (float_of_big_int i *. angle_mult) in
      push1 (RpcFloatUnit (f, Units.empty_unit)) st'
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute trig of dimensioned value")
      else push1 (RpcFloatUnit (fn_rad (f *. angle_mult), Units.empty_unit)) st'
  | RpcComplexUnit (c, u) ->
      if has_units u then raise (Invalid_argument "cannot compute trig of dimensioned value")
      else push1 (RpcComplexUnit (fn_complex c, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_sin st = apply_trig sin sin Gsl.Gsl_complex.sin st
let calc_cos st = apply_trig cos cos Gsl.Gsl_complex.cos st
let calc_tan st = apply_trig tan tan Gsl.Gsl_complex.tan st

let calc_asin st =
  check_args 1 "asin" st;
  let el, st' = pop1_eval st in
  let angle_mult = match st'.modes.angle with | Rad -> 1.0 | Deg -> 180.0 /. pi in
  match el with
  | RpcInt i -> push1 (RpcFloatUnit (asin (float_of_big_int i) *. angle_mult, Units.empty_unit)) st'
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute arcsine of dimensioned value")
      else push1 (RpcFloatUnit (asin f *. angle_mult, Units.empty_unit)) st'
  | RpcComplexUnit (c, u) ->
      if has_units u then raise (Invalid_argument "cannot compute arcsine of dimensioned value")
      else push1 (RpcComplexUnit (Gsl.Gsl_complex.arcsin c, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_acos st =
  check_args 1 "acos" st;
  let el, st' = pop1_eval st in
  let angle_mult = match st'.modes.angle with | Rad -> 1.0 | Deg -> 180.0 /. pi in
  match el with
  | RpcInt i -> push1 (RpcFloatUnit (acos (float_of_big_int i) *. angle_mult, Units.empty_unit)) st'
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute arccos of dimensioned value")
      else push1 (RpcFloatUnit (acos f *. angle_mult, Units.empty_unit)) st'
  | RpcComplexUnit (c, u) ->
      if has_units u then raise (Invalid_argument "cannot compute arccos of dimensioned value")
      else push1 (RpcComplexUnit (Gsl.Gsl_complex.arccos c, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_atan st =
  check_args 1 "atan" st;
  let el, st' = pop1_eval st in
  let angle_mult = match st'.modes.angle with | Rad -> 1.0 | Deg -> 180.0 /. pi in
  match el with
  | RpcInt i -> push1 (RpcFloatUnit (atan (float_of_big_int i) *. angle_mult, Units.empty_unit)) st'
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute arctan of dimensioned value")
      else push1 (RpcFloatUnit (atan f *. angle_mult, Units.empty_unit)) st'
  | RpcComplexUnit (c, u) ->
      if has_units u then raise (Invalid_argument "cannot compute arctan of dimensioned value")
      else push1 (RpcComplexUnit (Gsl.Gsl_complex.arctan c, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_sinh st =
  check_args 1 "sinh" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcInt i -> push1 (RpcFloatUnit (sinh (float_of_big_int i), Units.empty_unit)) st'
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute sinh of dimensioned value")
      else push1 (RpcFloatUnit (sinh f, Units.empty_unit)) st'
  | RpcComplexUnit (c, u) ->
      if has_units u then raise (Invalid_argument "cannot compute sinh of dimensioned value")
      else push1 (RpcComplexUnit (Gsl.Gsl_complex.sinh c, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_cosh st =
  check_args 1 "cosh" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcInt i -> push1 (RpcFloatUnit (cosh (float_of_big_int i), Units.empty_unit)) st'
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute cosh of dimensioned value")
      else push1 (RpcFloatUnit (cosh f, Units.empty_unit)) st'
  | RpcComplexUnit (c, u) ->
      if has_units u then raise (Invalid_argument "cannot compute cosh of dimensioned value")
      else push1 (RpcComplexUnit (Gsl.Gsl_complex.cosh c, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_tanh st =
  check_args 1 "tanh" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcInt i -> push1 (RpcFloatUnit (tanh (float_of_big_int i), Units.empty_unit)) st'
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute tanh of dimensioned value")
      else push1 (RpcFloatUnit (tanh f, Units.empty_unit)) st'
  | RpcComplexUnit (c, u) ->
      if has_units u then raise (Invalid_argument "cannot compute tanh of dimensioned value")
      else push1 (RpcComplexUnit (Gsl.Gsl_complex.tanh c, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_asinh st =
  check_args 1 "asinh" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute asinh of dimensioned value")
      else push1 (RpcFloatUnit (Gsl.Math.asinh f, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_acosh st =
  check_args 1 "acosh" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute acosh of dimensioned value")
      else push1 (RpcFloatUnit (Gsl.Math.acosh f, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_atanh st =
  check_args 1 "atanh" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute atanh of dimensioned value")
      else push1 (RpcFloatUnit (Gsl.Math.atanh f, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_re st =
  check_args 1 "re" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcComplexUnit (c, u) -> push1 (RpcFloatUnit (c.Complex.re, u)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_im st =
  check_args 1 "im" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcComplexUnit (c, u) -> push1 (RpcFloatUnit (c.Complex.im, u)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_gamma st =
  check_args 1 "gamma" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute gamma of dimensioned value")
      else push1 (RpcFloatUnit (Gsl.Sf.gamma f, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_lngamma st =
  check_args 1 "lngamma" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute lngamma of dimensioned value")
      else push1 (RpcFloatUnit (Gsl.Sf.lngamma f, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_erf st =
  check_args 1 "erf" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute erf of dimensioned value")
      else push1 (RpcFloatUnit (Gsl.Sf.erf f, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_erfc st =
  check_args 1 "erfc" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute erfc of dimensioned value")
      else push1 (RpcFloatUnit (Gsl.Sf.erfc f, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_fact st =
  check_args 1 "fact" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcInt i ->
      if sign_big_int i >= 0 then
        let rec fact acc n =
          if eq_big_int n unit_big_int then acc
          else fact (mult_big_int acc n) (pred_big_int n)
        in
        push1 (RpcInt (fact unit_big_int i)) st'
      else
        raise (Invalid_argument "factorial requires nonnegative integer")
  | RpcFloatUnit (f, u) ->
      if has_units u then raise (Invalid_argument "cannot compute factorial of dimensioned value")
      else push1 (RpcFloatUnit (Gsl.Sf.fact (int_of_float f), Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "invalid argument")

let calc_transpose st =
  check_args 1 "transpose" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatMatrixUnit (m, u) ->
      let rows, cols = Gsl.Matrix.dims m in
      let t = Gsl.Matrix.create cols rows in
      Gsl.Matrix.transpose m t;
      push1 (RpcFloatMatrixUnit (t, u)) st'
  | RpcComplexMatrixUnit (m, u) ->
      let rows, cols = Gsl.Matrix_complex.dims m in
      let t = Gsl.Matrix_complex.create cols rows in
      Gsl.Matrix_complex.transpose m t;
      push1 (RpcComplexMatrixUnit (t, u)) st'
  | _ -> raise (Invalid_argument "transpose can only be applied to matrices")

let calc_mod st =
  check_args 2 "mod" st;
  let el1, el2, st0 = pop2_eval st in
  match el1, el2 with
  | RpcInt i1, RpcInt i2 ->
      push1 (RpcInt (mod_big_int i1 i2)) st0
  | RpcFloatUnit (f1, u1), RpcFloatUnit (f2, u2) ->
      if has_units u1 || has_units u2 then
        raise (Invalid_argument "cannot compute mod of dimensioned values")
      else push1 (RpcFloatUnit (mod_float f1 f2, Units.empty_unit)) st0
  | _ -> raise (Invalid_argument "mod requires integer or real arguments")

let calc_floor st =
  check_args 1 "floor" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatUnit (f, u) -> push1 (RpcFloatUnit (floor f, u)) st'
  | _ -> raise (Invalid_argument "floor requires real argument")

let calc_ceiling st =
  check_args 1 "ceiling" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatUnit (f, u) -> push1 (RpcFloatUnit (ceil f, u)) st'
  | _ -> raise (Invalid_argument "ceiling requires real argument")

let calc_to_int st =
  check_args 1 "to_int" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatUnit (f, u) -> push1 (RpcInt (big_int_of_string (string_of_float f))) st'
  | _ -> raise (Invalid_argument "to_int requires real argument")

let calc_to_float st =
  check_args 1 "to_float" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcInt i -> push1 (RpcFloatUnit (float_of_big_int i, Units.empty_unit)) st'
  | _ -> raise (Invalid_argument "to_float requires integer argument")

let calc_rand st =
  { (state_backup st) with stack = stack_push (RpcFloatUnit (Random.float 1.0, Units.empty_unit)) st.stack }

let calc_enter_pi st =
  { (state_backup st) with stack = stack_push (RpcFloatUnit (pi, Units.empty_unit)) st.stack }

let calc_trace st =
  check_args 1 "trace" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatMatrixUnit (m, u) ->
      let n, mm = Gsl.Matrix.dims m in
      if n = mm then
        let result = ref 0.0 in
        for i = 0 to pred n do result := !result +. m.{i, i} done;
        push1 (RpcFloatUnit (!result, u)) st'
      else
        raise (Invalid_argument "argument of trace must be a square matrix")
  | RpcComplexMatrixUnit (m, u) ->
      let n, mm = Gsl.Matrix_complex.dims m in
      if n = mm then
        let result = ref Complex.zero in
        for i = 0 to pred n do result := Complex.add !result m.{i, i} done;
        push1 (RpcComplexUnit (!result, u)) st'
      else
        raise (Invalid_argument "argument of trace must be a square matrix")
  | _ -> raise (Invalid_argument "argument of trace must be a square matrix")

let calc_unit_value st =
  check_args 1 "unit_value" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatUnit (f, _) -> push1 (RpcFloatUnit (f, Units.empty_unit)) st'
  | RpcComplexUnit (c, _) -> push1 (RpcComplexUnit (c, Units.empty_unit)) st'
  | RpcFloatMatrixUnit (m, _) -> push1 (RpcFloatMatrixUnit (m, Units.empty_unit)) st'
  | RpcComplexMatrixUnit (m, _) -> push1 (RpcComplexMatrixUnit (m, Units.empty_unit)) st'
  | _ -> push1 el st'

(**********************************************************************)
(* BINARY OPERATIONS                                                  *)
(**********************************************************************)

let calc_add st =
  check_args 2 "add" st;
  let el1, el2, st0 = pop2_eval st in
  match el1, el2 with
  | RpcInt i1, RpcInt i2 -> push1 (RpcInt (add_big_int i1 i2)) st0
  | RpcInt i1, RpcFloatUnit (f2, u2) ->
      if has_units u2 then raise (Invalid_argument "inconsistent units")
      else push1 (RpcFloatUnit (float_of_big_int i1 +. f2, u2)) st0
  | RpcInt i1, RpcComplexUnit (c2, u2) ->
      if has_units u2 then raise (Invalid_argument "inconsistent units")
      else push1 (RpcComplexUnit (Complex.add (cmpx_of_int i1) c2, u2)) st0
  | RpcFloatUnit (f1, u1), RpcInt i2 ->
      if has_units u1 then raise (Invalid_argument "inconsistent units")
      else push1 (RpcFloatUnit (f1 +. float_of_big_int i2, u1)) st0
  | RpcFloatUnit (f1, u1), RpcFloatUnit (f2, u2) ->
      let conv = Units.conversion_factor u1 u2 !Units.unit_table in
      push1 (RpcFloatUnit (f1 *. conv +. f2, u2)) st0
  | RpcFloatUnit (f1, u1), RpcComplexUnit (c2, u2) ->
      let conv = Units.conversion_factor u1 u2 !Units.unit_table in
      push1 (RpcComplexUnit (Complex.add (c_of_f (f1 *. conv)) c2, u2)) st0
  | RpcComplexUnit (c1, u1), RpcInt i2 ->
      if has_units u1 then raise (Invalid_argument "inconsistent units")
      else push1 (RpcComplexUnit (Complex.add c1 (cmpx_of_int i2), u1)) st0
  | RpcComplexUnit (c1, u1), RpcFloatUnit (f2, u2) ->
      let conv = c_of_f (Units.conversion_factor u1 u2 !Units.unit_table) in
      push1 (RpcComplexUnit (Complex.add (Complex.mul c1 conv) (c_of_f f2), u2)) st0
  | RpcComplexUnit (c1, u1), RpcComplexUnit (c2, u2) ->
      let conv = c_of_f (Units.conversion_factor u1 u2 !Units.unit_table) in
      push1 (RpcComplexUnit (Complex.add (Complex.mul c1 conv) c2, u2)) st0
  | RpcFloatMatrixUnit (m1, u1), RpcFloatMatrixUnit (m2, u2) ->
      let d1 = Gsl.Matrix.dims m1 and d2 = Gsl.Matrix.dims m2 in
      if d1 = d2 then
        let conv = Units.conversion_factor u1 u2 !Units.unit_table in
        let result = Gsl.Matrix.copy m1 in
        Gsl.Matrix.scale result conv;
        Gsl.Matrix.add result m2;
        push1 (RpcFloatMatrixUnit (result, u2)) st0
      else raise (Invalid_argument "incompatible matrix dimensions for addition")
  | RpcFloatMatrixUnit (m1, u1), RpcComplexMatrixUnit (m2, u2) ->
      let d1 = Gsl.Matrix.dims m1 and d2 = Gsl.Matrix_complex.dims m2 in
      if d1 = d2 then
        let conv = c_of_f (Units.conversion_factor u1 u2 !Units.unit_table) in
        let c1 = Gsl_assist.cmat_of_fmat m1 in
        Gsl.Matrix_complex.scale c1 conv;
        Gsl.Matrix_complex.add c1 m2;
        push1 (RpcComplexMatrixUnit (c1, u2)) st0
      else raise (Invalid_argument "incompatible matrix dimensions for addition")
  | RpcComplexMatrixUnit (m1, u1), RpcFloatMatrixUnit (m2, u2) ->
      let d1 = Gsl.Matrix_complex.dims m1 and d2 = Gsl.Matrix.dims m2 in
      if d1 = d2 then
        let conv = c_of_f (Units.conversion_factor u1 u2 !Units.unit_table) in
        let c2 = Gsl_assist.cmat_of_fmat m2 in
        let result = Gsl.Matrix_complex.copy m1 in
        Gsl.Matrix_complex.scale result conv;
        Gsl.Matrix_complex.sub result c2;
        push1 (RpcComplexMatrixUnit (result, u2)) st0
      else raise (Invalid_argument "incompatible matrix dimensions for addition")
  | RpcComplexMatrixUnit (m1, u1), RpcComplexMatrixUnit (m2, u2) ->
      let d1 = Gsl.Matrix_complex.dims m1 and d2 = Gsl.Matrix_complex.dims m2 in
      if d1 = d2 then
        let conv = c_of_f (Units.conversion_factor u1 u2 !Units.unit_table) in
        let result = Gsl.Matrix_complex.copy m1 in
        Gsl.Matrix_complex.scale result conv;
        Gsl.Matrix_complex.add result m2;
        push1 (RpcComplexMatrixUnit (result, u2)) st0
      else raise (Invalid_argument "incompatible matrix dimensions for addition")
  | _ -> raise (Invalid_argument "incompatible types for addition")

let calc_sub st =
  check_args 2 "sub" st;
  let el1, el2, st0 = pop2_eval st in
  match el1, el2 with
  | RpcInt i1, RpcInt i2 -> push1 (RpcInt (sub_big_int i1 i2)) st0
  | RpcInt i1, RpcFloatUnit (f2, u2) ->
      if has_units u2 then raise (Invalid_argument "inconsistent units")
      else push1 (RpcFloatUnit (float_of_big_int i1 -. f2, u2)) st0
  | RpcInt i1, RpcComplexUnit (c2, u2) ->
      if has_units u2 then raise (Invalid_argument "inconsistent units")
      else push1 (RpcComplexUnit (Complex.sub (cmpx_of_int i1) c2, u2)) st0
  | RpcFloatUnit (f1, u1), RpcInt i2 ->
      if has_units u1 then raise (Invalid_argument "inconsistent units")
      else push1 (RpcFloatUnit (f1 -. float_of_big_int i2, u1)) st0
  | RpcFloatUnit (f1, u1), RpcFloatUnit (f2, u2) ->
      let conv = Units.conversion_factor u1 u2 !Units.unit_table in
      push1 (RpcFloatUnit (conv *. f1 -. f2, u2)) st0
  | RpcFloatUnit (f1, u1), RpcComplexUnit (c2, u2) ->
      let c1 = c_of_f (f1 *. Units.conversion_factor u1 u2 !Units.unit_table) in
      push1 (RpcComplexUnit (Complex.sub c1 c2, u2)) st0
  | RpcComplexUnit (c1, u1), RpcInt i2 ->
      if has_units u1 then raise (Invalid_argument "inconsistent units")
      else push1 (RpcComplexUnit (Complex.sub c1 (cmpx_of_int i2), u1)) st0
  | RpcComplexUnit (c1, u1), RpcFloatUnit (f2, u2) ->
      let conv = c_of_f (Units.conversion_factor u1 u2 !Units.unit_table) in
      let c2 = c_of_f f2 in
      push1 (RpcComplexUnit (Complex.sub (Complex.mul c1 conv) c2, u2)) st0
  | RpcComplexUnit (c1, u1), RpcComplexUnit (c2, u2) ->
      let conv = c_of_f (Units.conversion_factor u1 u2 !Units.unit_table) in
      push1 (RpcComplexUnit (Complex.sub (Complex.mul c1 conv) c2, u2)) st0
  | RpcFloatMatrixUnit (m1, u1), RpcFloatMatrixUnit (m2, u2) ->
      let d1 = Gsl.Matrix.dims m1 and d2 = Gsl.Matrix.dims m2 in
      if d1 = d2 then
        let conv = Units.conversion_factor u1 u2 !Units.unit_table in
        let result = Gsl.Matrix.copy m1 in
        Gsl.Matrix.scale result conv;
        Gsl.Matrix.sub result m2;
        push1 (RpcFloatMatrixUnit (result, u2)) st0
      else raise (Invalid_argument "incompatible matrix dimensions for subtraction")
  | RpcFloatMatrixUnit (m1, u1), RpcComplexMatrixUnit (m2, u2) ->
      let d1 = Gsl.Matrix.dims m1 and d2 = Gsl.Matrix_complex.dims m2 in
      if d1 = d2 then
        let conv = c_of_f (Units.conversion_factor u1 u2 !Units.unit_table) in
        let c1 = Gsl_assist.cmat_of_fmat m1 in
        Gsl.Matrix_complex.scale c1 conv;
        Gsl.Matrix_complex.sub c1 m2;
        push1 (RpcComplexMatrixUnit (c1, u2)) st0
      else raise (Invalid_argument "incompatible matrix dimensions for subtraction")
  | RpcComplexMatrixUnit (m1, u1), RpcFloatMatrixUnit (m2, u2) ->
      let d1 = Gsl.Matrix_complex.dims m1 and d2 = Gsl.Matrix.dims m2 in
      if d1 = d2 then
        let conv = c_of_f (Units.conversion_factor u1 u2 !Units.unit_table) in
        let c2 = Gsl_assist.cmat_of_fmat m2 in
        let result = Gsl.Matrix_complex.copy m1 in
        Gsl.Matrix_complex.scale result conv;
        Gsl.Matrix_complex.sub result c2;
        push1 (RpcComplexMatrixUnit (result, u2)) st0
      else raise (Invalid_argument "incompatible matrix dimensions for subtraction")
  | RpcComplexMatrixUnit (m1, u1), RpcComplexMatrixUnit (m2, u2) ->
      let d1 = Gsl.Matrix_complex.dims m1 and d2 = Gsl.Matrix_complex.dims m2 in
      if d1 = d2 then
        let conv = c_of_f (Units.conversion_factor u1 u2 !Units.unit_table) in
        let result = Gsl.Matrix_complex.copy m1 in
        Gsl.Matrix_complex.scale result conv;
        Gsl.Matrix_complex.sub result m2;
        push1 (RpcComplexMatrixUnit (result, u2)) st0
      else raise (Invalid_argument "incompatible matrix dimensions for subtraction")
  | _ -> raise (Invalid_argument "incompatible types for subtraction")

let calc_mult st =
  check_args 2 "mult" st;
  let el1, el2, st0 = pop2_eval st in
  match el1, el2 with
  | RpcInt i1, RpcInt i2 -> push1 (RpcInt (mult_big_int i1 i2)) st0
  | RpcInt i1, RpcFloatUnit (f2, u2) ->
      push1 (RpcFloatUnit (float_of_big_int i1 *. f2, u2)) st0
  | RpcInt i1, RpcComplexUnit (c2, u2) ->
      push1 (RpcComplexUnit (Complex.mul (cmpx_of_int i1) c2, u2)) st0
  | RpcInt i1, RpcFloatMatrixUnit (m2, u2) ->
      let result = Gsl.Matrix.copy m2 in
      Gsl.Matrix.scale result (float_of_big_int i1);
      push1 (RpcFloatMatrixUnit (result, u2)) st0
  | RpcInt i1, RpcComplexMatrixUnit (m2, u2) ->
      let result = Gsl.Matrix_complex.copy m2 in
      Gsl.Matrix_complex.scale result (cmpx_of_int i1);
      push1 (RpcComplexMatrixUnit (result, u2)) st0
  | RpcFloatUnit (f1, u1), RpcInt i2 ->
      push1 (RpcFloatUnit (f1 *. float_of_big_int i2, u1)) st0
  | RpcFloatUnit (f1, u1), RpcFloatUnit (f2, u2) ->
      push1 (RpcFloatUnit (f1 *. f2, Units.mult u1 u2)) st0
  | RpcFloatUnit (f1, u1), RpcComplexUnit (c2, u2) ->
      push1 (RpcComplexUnit (Complex.mul (c_of_f f1) c2, Units.mult u1 u2)) st0
  | RpcFloatUnit (f1, u1), RpcFloatMatrixUnit (m2, u2) ->
      let result = Gsl.Matrix.copy m2 in
      Gsl.Matrix.scale result f1;
      push1 (RpcFloatMatrixUnit (result, Units.mult u1 u2)) st0
  | RpcFloatUnit (f1, u1), RpcComplexMatrixUnit (m2, u2) ->
      let result = Gsl.Matrix_complex.copy m2 in
      Gsl.Matrix_complex.scale result (c_of_f f1);
      push1 (RpcComplexMatrixUnit (result, Units.mult u1 u2)) st0
  | RpcComplexUnit (c1, u1), RpcInt i2 ->
      push1 (RpcComplexUnit (Complex.mul c1 (cmpx_of_int i2), u1)) st0
  | RpcComplexUnit (c1, u1), RpcFloatUnit (f2, u2) ->
      push1 (RpcComplexUnit (Complex.mul c1 (c_of_f f2), Units.mult u1 u2)) st0
  | RpcComplexUnit (c1, u1), RpcComplexUnit (c2, u2) ->
      push1 (RpcComplexUnit (Complex.mul c1 c2, Units.mult u1 u2)) st0
  | RpcComplexUnit (c1, u1), RpcFloatMatrixUnit (m2, u2) ->
      let c2 = Gsl_assist.cmat_of_fmat m2 in
      Gsl.Matrix_complex.scale c2 c1;
      push1 (RpcComplexMatrixUnit (c2, Units.mult u1 u2)) st0
  | RpcComplexUnit (c1, u1), RpcComplexMatrixUnit (m2, u2) ->
      let result = Gsl.Matrix_complex.copy m2 in
      Gsl.Matrix_complex.scale result c1;
      push1 (RpcComplexMatrixUnit (result, Units.mult u1 u2)) st0
  | RpcFloatMatrixUnit (m1, u1), RpcInt i2 ->
      let result = Gsl.Matrix.copy m1 in
      Gsl.Matrix.scale result (float_of_big_int i2);
      push1 (RpcFloatMatrixUnit (result, u1)) st0
  | RpcFloatMatrixUnit (m1, u1), RpcFloatUnit (f2, u2) ->
      let result = Gsl.Matrix.copy m1 in
      Gsl.Matrix.scale result f2;
      push1 (RpcFloatMatrixUnit (result, Units.mult u1 u2)) st0
  | RpcFloatMatrixUnit (m1, u1), RpcComplexUnit (c2, u2) ->
      let c1 = Gsl_assist.cmat_of_fmat m1 in
      Gsl.Matrix_complex.scale c1 c2;
      push1 (RpcComplexMatrixUnit (c1, Units.mult u1 u2)) st0
  | RpcFloatMatrixUnit (m1, u1), RpcFloatMatrixUnit (m2, u2) ->
      let n1, m1d = Gsl.Matrix.dims m1 and n2, m2d = Gsl.Matrix.dims m2 in
      if m1d = n2 then
        let result = Gsl.Matrix.create n1 m2d in
        Gsl.Blas.gemm ~ta:Gsl.Blas.NoTrans ~tb:Gsl.Blas.NoTrans
          ~alpha:1.0 ~a:m1 ~b:m2 ~beta:0.0 ~c:result;
        push1 (RpcFloatMatrixUnit (result, Units.mult u1 u2)) st0
      else raise (Invalid_argument "incompatible matrix dimensions for multiplication")
  | RpcFloatMatrixUnit (m1, u1), RpcComplexMatrixUnit (m2, u2) ->
      let n1, m1d = Gsl.Matrix.dims m1 and n2, m2d = Gsl.Matrix_complex.dims m2 in
      if m1d = n2 then
        let c1 = Gsl_assist.cmat_of_fmat m1
        and result = Gsl.Matrix_complex.create n1 m2d in
        Gsl.Blas.Complex.gemm ~ta:Gsl.Blas.NoTrans ~tb:Gsl.Blas.NoTrans
          ~alpha:Complex.one ~a:c1 ~b:m2 ~beta:Complex.zero ~c:result;
        push1 (RpcComplexMatrixUnit (result, Units.mult u1 u2)) st0
      else raise (Invalid_argument "incompatible matrix dimensions for multiplication")
  | RpcComplexMatrixUnit (m1, u1), RpcInt i2 ->
      let c2 = cmpx_of_int i2 in
      let result = Gsl.Matrix_complex.copy m1 in
      Gsl.Matrix_complex.scale result c2;
      push1 (RpcComplexMatrixUnit (result, u1)) st0
  | RpcComplexMatrixUnit (m1, u1), RpcFloatUnit (f2, u2) ->
      let result = Gsl.Matrix_complex.copy m1 in
      Gsl.Matrix_complex.scale result Complex.one;
      push1 (RpcComplexMatrixUnit (result, Units.mult u1 u2)) st0
  | RpcComplexMatrixUnit (m1, u1), RpcComplexUnit (c2, u2) ->
      let result = Gsl.Matrix_complex.copy m1 in
      Gsl.Matrix_complex.scale result Complex.one;
      push1 (RpcComplexMatrixUnit (result, Units.mult u1 u2)) st0
  | RpcComplexMatrixUnit (m1, u1), RpcFloatMatrixUnit (m2, u2) ->
      let n1, m1d = Gsl.Matrix_complex.dims m1 and n2, m2d = Gsl.Matrix.dims m2 in
      if m1d = n2 then
        let c2 = Gsl_assist.cmat_of_fmat m2
        and result = Gsl.Matrix_complex.create n1 m2d in
        Gsl.Blas.Complex.gemm ~ta:Gsl.Blas.NoTrans ~tb:Gsl.Blas.NoTrans
          ~alpha:Complex.one ~a:m1 ~b:c2 ~beta:Complex.zero ~c:result;
        push1 (RpcComplexMatrixUnit (result, Units.mult u1 u2)) st0
      else raise (Invalid_argument "incompatible matrix dimensions for multiplication")
  | RpcComplexMatrixUnit (m1, u1), RpcComplexMatrixUnit (m2, u2) ->
      let n1, m1d = Gsl.Matrix_complex.dims m1 and n2, m2d = Gsl.Matrix_complex.dims m2 in
      if m1d = n2 then
        let result = Gsl.Matrix_complex.create n1 m2d in
        Gsl.Blas.Complex.gemm ~ta:Gsl.Blas.NoTrans ~tb:Gsl.Blas.NoTrans
          ~alpha:Complex.one ~a:m1 ~b:m2 ~beta:Complex.zero ~c:result;
        push1 (RpcComplexMatrixUnit (result, Units.mult u1 u2)) st0
      else raise (Invalid_argument "incompatible matrix dimensions for multiplication")
  | _ -> raise (Invalid_argument "incompatible types for multiplication")

let calc_div st =
  check_args 2 "div" st;
  let el1, el2, st0 = pop2_eval st in
  match el1, el2 with
  | RpcInt i1, RpcInt i2 -> push1 (RpcInt (div_big_int i1 i2)) st0
  | RpcInt i1, RpcFloatUnit (f2, u2) ->
      push1 (RpcFloatUnit (float_of_big_int i1 /. f2, Units.div Units.empty_unit u2)) st0
  | RpcInt i1, RpcComplexUnit (c2, u2) ->
      push1 (RpcComplexUnit (Complex.div (cmpx_of_int i1) c2, Units.div Units.empty_unit u2)) st0
  | RpcFloatUnit (f1, u1), RpcInt i2 ->
      push1 (RpcFloatUnit (f1 /. float_of_big_int i2, u1)) st0
  | RpcFloatUnit (f1, u1), RpcFloatUnit (f2, u2) ->
      push1 (RpcFloatUnit (f1 /. f2, Units.div u1 u2)) st0
  | RpcFloatUnit (f1, u1), RpcComplexUnit (c2, u2) ->
      push1 (RpcComplexUnit (Complex.div (c_of_f f1) c2, Units.div u1 u2)) st0
  | RpcComplexUnit (c1, u1), RpcInt i2 ->
      push1 (RpcComplexUnit (Complex.div c1 (cmpx_of_int i2), u1)) st0
  | RpcComplexUnit (c1, u1), RpcFloatUnit (f2, u2) ->
      push1 (RpcComplexUnit (Complex.div c1 (c_of_f f2), Units.div u1 u2)) st0
  | RpcComplexUnit (c1, u1), RpcComplexUnit (c2, u2) ->
      push1 (RpcComplexUnit (Complex.div c1 c2, Units.div u1 u2)) st0
  | RpcFloatMatrixUnit (m1, u1), RpcInt i2 ->
      let result = Gsl.Matrix.copy m1 in
      Gsl.Matrix.scale result (1.0 /. float_of_big_int i2);
      push1 (RpcFloatMatrixUnit (result, u1)) st0
  | RpcFloatMatrixUnit (m1, u1), RpcFloatUnit (f2, u2) ->
      let result = Gsl.Matrix.copy m1 in
      Gsl.Matrix.scale result (1.0 /. f2);
      push1 (RpcFloatMatrixUnit (result, Units.div u1 u2)) st0
  | RpcFloatMatrixUnit (m1, u1), RpcComplexUnit (c2, u2) ->
      let c1 = Gsl_assist.cmat_of_fmat m1 in
      Gsl.Matrix_complex.scale c1 (Complex.inv c2);
      push1 (RpcComplexMatrixUnit (c1, Units.div u1 u2)) st0
  | RpcFloatMatrixUnit (m1, u1), RpcFloatMatrixUnit (m2, u2) ->
      let n1, m1d = Gsl.Matrix.dims m1 and n2, m2d = Gsl.Matrix.dims m2 in
      if n2 = m2d && m1d = n2 then
        let copy_el2 = Gsl.Vectmat.mat_convert ~protect:true (`M m2)
        and perm = Gsl.Permut.create m1d
        and inv = Gsl.Matrix.create m1d m1d in
        try
          let _ = Gsl.Linalg._LU_decomp copy_el2 perm in
          Gsl.Linalg._LU_invert copy_el2 perm (`M inv);
          let result = Gsl.Matrix.create n1 m2d in
          Gsl.Blas.gemm ~ta:Gsl.Blas.NoTrans ~tb:Gsl.Blas.NoTrans
            ~alpha:1.0 ~a:m1 ~b:inv ~beta:0.0 ~c:result;
          push1 (RpcFloatMatrixUnit (result, Units.div u1 u2)) st0
        with Gsl.Error.Gsl_exn _ ->
          raise (Invalid_argument "divisor matrix is singular")
      else if n2 <> m2d then
        raise (Invalid_argument "divisor matrix is non-square")
      else
        raise (Invalid_argument "incompatible dimensions for division")
  | RpcFloatMatrixUnit (m1, u1), RpcComplexMatrixUnit (m2, u2) ->
      let n1, m1d = Gsl.Matrix.dims m1 and n2, m2d = Gsl.Matrix_complex.dims m2 in
      if n2 = m2d && m1d = n2 then
        let copy_el2 = Gsl.Matrix_complex.copy m2
        and perm = Gsl.Permut.create m1d
        and inv = Gsl.Matrix_complex.create m1d m1d in
        try
          let _ = Gsl.Linalg.complex_LU_decomp (`CM copy_el2) perm in
          Gsl.Linalg.complex_LU_invert (`CM copy_el2) perm (`CM inv);
          let result = Gsl.Matrix_complex.create n1 m2d in
          Gsl.Blas.Complex.gemm ~ta:Gsl.Blas.NoTrans ~tb:Gsl.Blas.NoTrans
            ~alpha:Complex.one ~a:(Gsl_assist.cmat_of_fmat m1) ~b:inv ~beta:Complex.zero ~c:result;
          push1 (RpcComplexMatrixUnit (result, Units.div u1 u2)) st0
        with Gsl.Error.Gsl_exn _ ->
          raise (Invalid_argument "divisor matrix is singular")
      else if n2 <> m2d then
        raise (Invalid_argument "divisor matrix is non-square")
      else
        raise (Invalid_argument "incompatible matrix dimensions for division")
  | RpcComplexMatrixUnit (m1, u1), RpcInt i2 ->
      let c2 = cmpx_of_int i2 in
      let result = Gsl.Matrix_complex.copy m1 in
      Gsl.Matrix_complex.scale result (Complex.inv c2);
      push1 (RpcComplexMatrixUnit (result, u1)) st0
  | RpcComplexMatrixUnit (m1, u1), RpcFloatUnit (f2, u2) ->
      let result = Gsl.Matrix_complex.copy m1 in
      Gsl.Matrix_complex.scale result (Complex.inv (c_of_f f2));
      push1 (RpcComplexMatrixUnit (result, Units.div u1 u2)) st0
  | RpcComplexMatrixUnit (m1, u1), RpcComplexUnit (c2, u2) ->
      let result = Gsl.Matrix_complex.copy m1 in
      Gsl.Matrix_complex.scale result (Complex.inv c2);
      push1 (RpcComplexMatrixUnit (result, Units.div u1 u2)) st0
  | RpcComplexMatrixUnit (m1, u1), RpcFloatMatrixUnit (m2, u2) ->
      let n1, m1d = Gsl.Matrix_complex.dims m1 and n2, m2d = Gsl.Matrix.dims m2 in
      if n2 = m2d && m1d = n2 then
        let copy_el2 = Gsl.Matrix.copy m2
        and perm = Gsl.Permut.create m1d
        and inv = Gsl.Matrix.create m1d m1d in
        try
          let _ = Gsl.Linalg._LU_decomp (`M copy_el2) perm in
          Gsl.Linalg._LU_invert (`M copy_el2) perm (`M inv);
          let result = Gsl.Matrix_complex.create n1 m2d in
          Gsl.Blas.Complex.gemm ~ta:Gsl.Blas.NoTrans ~tb:Gsl.Blas.NoTrans
            ~alpha:Complex.one ~a:m1 ~b:(Gsl_assist.cmat_of_fmat inv) ~beta:Complex.zero ~c:result;
          push1 (RpcComplexMatrixUnit (result, Units.div u1 u2)) st0
        with Gsl.Error.Gsl_exn _ ->
          raise (Invalid_argument "divisor matrix is singular")
      else if n2 <> m2d then
        raise (Invalid_argument "divisor matrix is non-square")
      else
        raise (Invalid_argument "incompatible matrix dimensions for division")
  | RpcComplexMatrixUnit (m1, u1), RpcComplexMatrixUnit (m2, u2) ->
      let n1, m1d = Gsl.Matrix_complex.dims m1 and n2, m2d = Gsl.Matrix_complex.dims m2 in
      if n2 = m2d && m1d = n2 then
        let copy_el2 = Gsl.Matrix_complex.copy m2
        and perm = Gsl.Permut.create m1d
        and inv = Gsl.Matrix_complex.create m1d m1d in
        try
          let _ = Gsl.Linalg.complex_LU_decomp (`CM copy_el2) perm in
          Gsl.Linalg.complex_LU_invert (`CM copy_el2) perm (`CM inv);
          let result = Gsl.Matrix_complex.create n1 m2d in
          Gsl.Blas.Complex.gemm ~ta:Gsl.Blas.NoTrans ~tb:Gsl.Blas.NoTrans
            ~alpha:Complex.one ~a:m1 ~b:inv ~beta:Complex.zero ~c:result;
          push1 (RpcComplexMatrixUnit (result, Units.div u1 u2)) st0
        with Gsl.Error.Gsl_exn _ ->
          raise (Invalid_argument "divisor matrix is singular")
      else if n2 <> m2d then
        raise (Invalid_argument "divisor matrix is non-square")
      else
        raise (Invalid_argument "incompatible matrix dimensions for division")
  | _ -> raise (Invalid_argument "incompatible types for division")

let calc_pow st =
  check_args 2 "pow" st;
  let el1, el2, st0 = pop2_eval st in
  match el1, el2 with
  | RpcInt i1, RpcInt i2 ->
      if sign_big_int i2 >= 0 then
        push1 (RpcInt (power_big_int_positive_big_int i1 i2)) st0
      else
        raise (Invalid_argument "integer power function requires nonnegative power")
  | RpcInt i1, RpcFloatUnit (f2, u2) ->
      if has_units u2 then raise (Invalid_argument "cannot raise to a dimensioned power")
      else
        let f1 = float_of_big_int i1 in
        if f1 >= 0.0 || f2 = float_of_int (int_of_float f2) then
          push1 (RpcFloatUnit (f1 ** f2, Units.empty_unit)) st0
        else
          push1 (RpcComplexUnit (Complex.pow (cmpx_of_float f1) (cmpx_of_float f2), Units.empty_unit)) st0
  | RpcInt i1, RpcComplexUnit (c2, u2) ->
      if has_units u2 then raise (Invalid_argument "cannot raise to a dimensioned power")
      else
        push1 (RpcComplexUnit (Complex.pow (cmpx_of_int i1) c2, Units.empty_unit)) st0
  | RpcFloatUnit (f1, u1), RpcInt i2 ->
      let f2 = float_of_big_int i2 in
      push1 (RpcFloatUnit (f1 ** f2, Units.pow u1 f2)) st0
  | RpcFloatUnit (f1, u1), RpcFloatUnit (f2, u2) ->
      if has_units u2 then raise (Invalid_argument "cannot raise to a dimensioned power")
      else if f2 > 0.0 then
        push1 (RpcFloatUnit (f1 ** f2, Units.pow u1 f2)) st0
      else
        let c_prod = Complex.pow (c_of_f f1) (c_of_f f2) in
        if c_prod.Complex.im <> 0.0 then
          push1 (RpcComplexUnit (c_prod, Units.pow u1 f2)) st0
        else
          push1 (RpcFloatUnit (c_prod.Complex.re, Units.pow u1 f2)) st0
  | RpcFloatUnit (f1, u1), RpcComplexUnit (c2, u2) ->
      if has_units u2 then raise (Invalid_argument "cannot raise to a dimensioned power")
      else if has_units u1 then raise (Invalid_argument "cannot raise dimensioned value to complex power")
      else push1 (RpcComplexUnit (Complex.pow (c_of_f f1) c2, Units.empty_unit)) st0
  | RpcComplexUnit (c1, u1), RpcInt i2 ->
      let f2 = float_of_big_int i2 in
      push1 (RpcComplexUnit (Complex.pow c1 (cmpx_of_int i2), Units.pow u1 f2)) st0
  | RpcComplexUnit (c1, u1), RpcFloatUnit (f2, u2) ->
      if has_units u2 then raise (Invalid_argument "cannot raise to a dimensioned power")
      else if has_units u1 then raise (Invalid_argument "cannot raise dimensioned value to complex power")
      else push1 (RpcComplexUnit (Complex.pow c1 (c_of_f f2), Units.pow u1 f2)) st0
  | RpcComplexUnit (c1, u1), RpcComplexUnit (c2, u2) ->
      if has_units u2 then raise (Invalid_argument "cannot raise to a dimensioned power")
      else if has_units u1 then raise (Invalid_argument "cannot raise dimensioned value to complex power")
      else push1 (RpcComplexUnit (Complex.pow c1 c2, Units.empty_unit)) st0
  | _ -> raise (Invalid_argument "incompatible types")

let calc_gcd st =
  check_args 2 "gcd" st;
  let el1, el2, st0 = pop2_eval st in
  match el1, el2 with
  | RpcInt i1, RpcInt i2 ->
      if sign_big_int i1 >= 0 && sign_big_int i2 >= 0 then
        push1 (RpcInt (gcd_big_int i1 i2)) st0
      else
        raise (Invalid_argument "integer gcd requires nonnegative arguments")
  | _ -> raise (Invalid_argument "gcd requires integer arguments")

let calc_lcm st =
  check_args 2 "lcm" st;
  let el1, el2, st0 = pop2_eval st in
  match el1, el2 with
  | RpcInt i1, RpcInt i2 ->
      if sign_big_int i1 >= 0 && sign_big_int i2 >= 0 then
        let lcm = abs_big_int (div_big_int (mult_big_int i1 i2) (gcd_big_int i1 i2)) in
        push1 (RpcInt lcm) st0
      else
        raise (Invalid_argument "integer lcm requires nonnegative arguments")
  | _ -> raise (Invalid_argument "lcm requires integer arguments")

let calc_binom st =
  check_args 2 "binom" st;
  let el1, el2, st0 = pop2_eval st in
  match el1, el2 with
  | RpcInt n, RpcInt k ->
      if sign_big_int n >= 0 && sign_big_int k >= 0 && ge_big_int n k then
        let rec loop acc n' k' =
          if eq_big_int k' unit_big_int then acc
          else loop (div_big_int (mult_big_int acc n') k') (pred_big_int n') (pred_big_int k')
        in
        push1 (RpcInt (loop n n k)) st0
      else
        raise (Invalid_argument "integer binom requires nonnegative arguments with n >= k")
  | RpcFloatUnit (n, u1), RpcFloatUnit (k, u2) ->
      if has_units u1 || has_units u2 then
        raise (Invalid_argument "cannot compute binom of dimensioned values")
      else
        try
          let log_coeff = Gsl.Sf.lngamma (n +. 1.0) -.
            Gsl.Sf.lngamma (k +. 1.0) -.
            Gsl.Sf.lngamma (n -. k +. 1.0) in
          push1 (RpcFloatUnit (exp log_coeff, Units.empty_unit)) st0
        with Gsl.Error.Gsl_exn (_, errstr) ->
          raise (Invalid_argument errstr)
  | _ -> raise (Invalid_argument "binom requires either two integer or two real arguments")

let calc_perm st =
  check_args 2 "perm" st;
  let el1, el2, st0 = pop2_eval st in
  match el1, el2 with
  | RpcInt n, RpcInt k ->
      if sign_big_int n >= 0 && sign_big_int k >= 0 && ge_big_int n k then
        let rec loop acc n' =
          if eq_big_int n' k then acc
          else loop (mult_big_int acc n') (pred_big_int n')
        in
        push1 (RpcInt (loop n n)) st0
      else
        raise (Invalid_argument "integer perm requires nonnegative arguments with n >= k")
  | RpcFloatUnit (n, u1), RpcFloatUnit (k, u2) ->
      if has_units u1 || has_units u2 then
        raise (Invalid_argument "cannot compute permutations of dimensioned values")
      else
        try
          let log_perm = Gsl.Sf.lngamma (n +. 1.0) -. Gsl.Sf.lngamma (n -. k +. 1.0) in
          push1 (RpcFloatUnit (exp log_perm, Units.empty_unit)) st0
        with Gsl.Error.Gsl_exn (_, errstr) ->
          raise (Invalid_argument errstr)
  | _ -> raise (Invalid_argument "perm requires either two integer or two real arguments")

let calc_standardize_units st =
  check_args 1 "standardize" st;
  let el, st' = pop1_eval st in
  let get_std u = Units.standardize_units u !Units.unit_table in
  match el with
  | RpcFloatUnit (f, u) ->
      let std = get_std u in
      push1 (RpcFloatUnit (f *. std.Units.coeff, std.Units.comp_units)) st'
  | RpcComplexUnit (c, u) ->
      let std = get_std u in
      push1 (RpcComplexUnit (Complex.mul c (c_of_f std.Units.coeff), std.Units.comp_units)) st'
  | RpcFloatMatrixUnit (m, u) ->
      let std = get_std u in
      let result = Gsl.Matrix.copy m in
      Gsl.Matrix.scale result std.Units.coeff;
      push1 (RpcFloatMatrixUnit (result, std.Units.comp_units)) st'
  | RpcComplexMatrixUnit (m, u) ->
      let std = get_std u in
      let c_coeff = c_of_f std.Units.coeff in
      let result = Gsl.Matrix_complex.copy m in
      Gsl.Matrix_complex.scale result c_coeff;
      push1 (RpcComplexMatrixUnit (result, std.Units.comp_units)) st'
  | _ -> push1 el st'

let calc_convert_units st =
  check_args 2 "convert" st;
  let el1, el2, st0 = pop2_eval st in
  match el2 with
  | RpcFloatUnit (_, u2) ->
      (match el1 with
      | RpcFloatUnit (f1, u1) ->
          let conv = Units.conversion_factor u1 u2 !Units.unit_table in
          push1 (RpcFloatUnit (f1 *. conv, u2)) st0
      | RpcComplexUnit (c1, u1) ->
          let conv = Units.conversion_factor u1 u2 !Units.unit_table in
          let c_conv = { Complex.re = conv; Complex.im = 0.0 } in
          push1 (RpcComplexUnit (Complex.mul c1 c_conv, u2)) st0
      | RpcFloatMatrixUnit (m1, u1) ->
          let conv = Units.conversion_factor u1 u2 !Units.unit_table in
          let result = Gsl.Matrix.copy m1 in
          Gsl.Matrix.scale result conv;
          push1 (RpcFloatMatrixUnit (result, u2)) st0
      | RpcComplexMatrixUnit (m1, u1) ->
          let conv = Units.conversion_factor u1 u2 !Units.unit_table in
          let c_conv = { Complex.re = conv; Complex.im = 0.0 } in
          let result = Gsl.Matrix_complex.copy m1 in
          Gsl.Matrix_complex.scale result c_conv;
          push1 (RpcComplexMatrixUnit (result, u2)) st0
      | _ -> raise (Invalid_argument "cannot convert units for this data type"))
  | _ -> raise (Invalid_argument "unit conversion target must be real-valued")

let calc_solve_linear st =
  check_args 2 "solve_linear" st;
  let el1, el2, st0 = pop2_eval st in
  match el1 with
  | RpcFloatMatrixUnit (a, u1) ->
      (match el2 with
      | RpcFloatMatrixUnit (b, u2) ->
          let n1, m1 = Gsl.Matrix.dims a in
          if n1 <> m1 then raise (Invalid_argument "multiplier matrix must be square");
          let n2, m2 = Gsl.Matrix.dims b in
          if m2 <> 1 then raise (Invalid_argument "resultant matrix must be a column");
          if n2 <> m1 then raise (Invalid_argument "dimensions of multiplier and resultant matrices do not match");
          let b_arr = Gsl.Matrix.to_array b in
          let x = Gsl.Linalg.solve_LU (`M a) (`A b_arr) in
          let x_mat = Gsl.Matrix.of_array x m1 1 in
          push1 (RpcFloatMatrixUnit (x_mat, Units.div u2 u1)) st0
      | RpcComplexMatrixUnit (b, u2) ->
          let n1, m1 = Gsl.Matrix.dims a in
          if n1 <> m1 then raise (Invalid_argument "multiplier matrix must be square");
          let n2, m2 = Gsl.Matrix_complex.dims b in
          if m2 <> 1 then raise (Invalid_argument "resultant matrix must be a column");
          if n2 <> m1 then raise (Invalid_argument "dimensions of multiplier and resultant matrices do not match");
          let a_cpx = Gsl_assist.cmat_of_fmat a in
          let b_arr = Gsl.Matrix_complex.to_array b in
          let b_vec = Gsl.Vector_complex.of_array b_arr in
          let x = Gsl_assist.solve_complex_LU (`CM a_cpx) b_vec in
          let x_mat = Gsl.Matrix_complex.of_complex_array x m1 1 in
          push1 (RpcComplexMatrixUnit (x_mat, Units.div u2 u1)) st0
      | _ -> raise (Invalid_argument "both arguments of solve_linear must be matrices"))
  | RpcComplexMatrixUnit (a, u1) ->
      (match el2 with
      | RpcFloatMatrixUnit (b, u2) ->
          let n1, m1 = Gsl.Matrix_complex.dims a in
          if n1 <> m1 then raise (Invalid_argument "multiplier matrix must be square");
          let n2, m2 = Gsl.Matrix.dims b in
          if m2 <> 1 then raise (Invalid_argument "resultant matrix must be a column");
          if n2 <> m1 then raise (Invalid_argument "dimensions of multiplier and resultant matrices do not match");
          let b_cpx = Gsl_assist.cmat_of_fmat b in
          let b_arr = Gsl.Matrix_complex.to_array b_cpx in
          let b_vec = Gsl.Vector_complex.of_array b_arr in
          let x = Gsl_assist.solve_complex_LU (`CM a) b_vec in
          let x_mat = Gsl.Matrix_complex.of_complex_array x m1 1 in
          push1 (RpcComplexMatrixUnit (x_mat, Units.div u2 u1)) st0
      | RpcComplexMatrixUnit (b, u2) ->
          let n1, m1 = Gsl.Matrix_complex.dims a in
          if n1 <> m1 then raise (Invalid_argument "multiplier matrix must be square");
          let n2, m2 = Gsl.Matrix_complex.dims b in
          if m2 <> 1 then raise (Invalid_argument "resultant matrix must be a column");
          if n2 <> m1 then raise (Invalid_argument "dimensions of multiplier and resultant matrices do not match");
          let b_arr = Gsl.Matrix_complex.to_array b in
          let b_vec = Gsl.Vector_complex.of_array b_arr in
          let x = Gsl_assist.solve_complex_LU (`CM a) b_vec in
          let x_mat = Gsl.Matrix_complex.of_complex_array x m1 1 in
          push1 (RpcComplexMatrixUnit (x_mat, Units.div u2 u1)) st0
      | _ -> raise (Invalid_argument "both arguments of solve_linear must be matrices"))
  | _ -> raise (Invalid_argument "both arguments of solve_linear must be matrices")

(**********************************************************************)
(* STATISTICS                                                         *)
(**********************************************************************)

let calc_total st =
  check_args 1 "total" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatMatrixUnit (m, u) ->
      let n, mm = Gsl.Matrix.dims m in
      let ones_arr = Array.make n 1.0 in
      let ones = Gsl.Matrix.of_array ones_arr 1 n in
      let result = Gsl.Matrix.create 1 mm in
      Gsl.Blas.gemm ~ta:Gsl.Blas.NoTrans ~tb:Gsl.Blas.NoTrans
        ~alpha:1.0 ~a:ones ~b:m ~beta:0.0 ~c:result;
      push1 (RpcFloatMatrixUnit (result, u)) st'
  | _ -> raise (Invalid_argument "total can only be applied to real matrices")

let calc_mean st =
  check_args 1 "mean" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatMatrixUnit (m, u) ->
      let n, mm = Gsl.Matrix.dims m in
      let factor = 1.0 /. float_of_int n in
      let ones_arr = Array.make n factor in
      let ones = Gsl.Matrix.of_array ones_arr 1 n in
      let result = Gsl.Matrix.create 1 mm in
      Gsl.Blas.gemm ~ta:Gsl.Blas.NoTrans ~tb:Gsl.Blas.NoTrans
        ~alpha:1.0 ~a:ones ~b:m ~beta:0.0 ~c:result;
      push1 (RpcFloatMatrixUnit (result, u)) st'
  | _ -> raise (Invalid_argument "mean can only be applied to real matrices")

let calc_sumsq st =
  check_args 1 "sumsq" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatMatrixUnit (m, u) ->
      let n, mm = Gsl.Matrix.dims m in
      let result = Gsl.Matrix.create 1 mm in
      for col = 0 to pred mm do
        result.{0, col} <- 0.0;
        for row = 0 to pred n do
          result.{0, col} <- result.{0, col} +. m.{row, col} *. m.{row, col}
        done
      done;
      push1 (RpcFloatMatrixUnit (result, Units.mult u u)) st'
  | _ -> raise (Invalid_argument "sumsq can only be applied to real matrices")

let calc_dup st = { st with stack = stack_dup st.stack }

let calc_varbias st =
  check_args 1 "varbias" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatMatrixUnit (m, u) ->
      let n, mm = Gsl.Matrix.dims m in
      let float_n = float_of_int n in
      let st1 = calc_dup { st' with stack = stack_push (RpcFloatMatrixUnit (m, u)) st'.stack } in
      let st2 = calc_sumsq st1 in
      let st3 = { st2 with stack = stack_push (RpcFloatUnit (float_n, Units.empty_unit)) st2.stack } in
      let st4 = calc_div st3 in
      let st5 = calc_dup st4 in
      let st6 = calc_mean st5 in
      let st7 = calc_sumsq st6 in
      calc_sub st7
  | _ -> raise (Invalid_argument "varbias can only be applied to real matrices")

let calc_var st =
  check_args 1 "var" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatMatrixUnit (m, u) ->
      let n, mm = Gsl.Matrix.dims m in
      if n >= 2 then
        let st1 = calc_varbias { st' with stack = stack_push (RpcFloatMatrixUnit (m, u)) st'.stack } in
        let n_over_nm1 = float_of_int n /. float_of_int (pred n) in
        let st2 = { st1 with stack = stack_push (RpcFloatUnit (n_over_nm1, Units.empty_unit)) st1.stack } in
        calc_mult st2
      else
        raise (Invalid_argument "insufficient matrix rows for unbiased sample variance")
  | _ -> raise (Invalid_argument "var can only be applied to real matrices")

let calc_stdevbias st =
  check_args 1 "stdevbias" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatMatrixUnit (m, u) ->
      let st1 = calc_varbias { st' with stack = stack_push (RpcFloatMatrixUnit (m, u)) st'.stack } in
      let el2, st2 = pop1 st1 in
      (match el2 with
      | RpcFloatMatrixUnit (m2, u2) ->
          let n, mm = Gsl.Matrix.dims m2 in
          let result = Gsl.Matrix.create 1 mm in
          for col = 0 to pred mm do result.{0, col} <- sqrt m2.{0, col} done;
          push1 (RpcFloatMatrixUnit (result, Units.pow u2 0.5)) st2
      | _ -> raise (Invalid_argument "internal error in stdevbias"))
  | _ -> raise (Invalid_argument "stdevbias can only be applied to real matrices")

let calc_stdev st =
  check_args 1 "stdev" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatMatrixUnit (m, u) ->
      let st1 = calc_var { st' with stack = stack_push (RpcFloatMatrixUnit (m, u)) st'.stack } in
      let el2, st2 = pop1 st1 in
      (match el2 with
      | RpcFloatMatrixUnit (m2, u2) ->
          let n, mm = Gsl.Matrix.dims m2 in
          let result = Gsl.Matrix.create 1 mm in
          for col = 0 to pred mm do result.{0, col} <- sqrt m2.{0, col} done;
          push1 (RpcFloatMatrixUnit (result, Units.pow u2 0.5)) st2
      | _ -> raise (Invalid_argument "internal error in stdev"))
  | _ -> raise (Invalid_argument "stdev can only be applied to real matrices")

let calc_min st =
  check_args 1 "min" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatMatrixUnit (m, u) ->
      let n, mm = Gsl.Matrix.dims m in
      let result = Gsl.Matrix.create 1 mm in
      for col = 0 to pred mm do
        result.{0, col} <- m.{0, col};
        for row = 1 to pred n do
          if m.{row, col} < result.{0, col} then result.{0, col} <- m.{row, col}
        done
      done;
      push1 (RpcFloatMatrixUnit (result, u)) st'
  | _ -> raise (Invalid_argument "min can only be applied to real matrices")

let calc_max st =
  check_args 1 "max" st;
  let el, st' = pop1_eval st in
  match el with
  | RpcFloatMatrixUnit (m, u) ->
      let n, mm = Gsl.Matrix.dims m in
      let result = Gsl.Matrix.create 1 mm in
      for col = 0 to pred mm do
        result.{0, col} <- m.{0, col};
        for row = 1 to pred n do
          if m.{row, col} > result.{0, col} then result.{0, col} <- m.{row, col}
        done
      done;
      push1 (RpcFloatMatrixUnit (result, u)) st'
  | _ -> raise (Invalid_argument "max can only be applied to real matrices")

(**********************************************************************)
(* UTPN                                                               *)
(**********************************************************************)

let calc_utpn st =
  check_args 3 "utpn" st;
  let el1, el2, el3, st0 = pop3_eval st in
  let get_float_args el =
    match el with
    | RpcInt i -> (float_of_big_int i, Units.empty_unit)
    | RpcFloatUnit (f, u) -> (f, u)
    | _ -> raise (Invalid_argument "utpn requires real scalar arguments")
  in
  let mean_orig, mean_units = get_float_args el1
  and var_orig, var_units = get_float_args el2
  and cutoff, cutoff_units = get_float_args el3 in
  try
    let mean = mean_orig *. Units.conversion_factor mean_units cutoff_units !Units.unit_table in
    let var = var_orig *. Units.conversion_factor var_units cutoff_units !Units.unit_table in
    if var <= 0.0 then raise (Invalid_argument "variance argument to utpn must be positive");
    let arg = (cutoff -. mean) /. (sqrt (2.0 *. var)) in
    let st1 = push1 (RpcFloatUnit (arg, Units.empty_unit)) st0 in
    let st2 = calc_erfc st1 in
    let st3 = push1 (RpcFloatUnit (0.5, cutoff_units)) st2 in
    calc_mult st3
  with Units.Units_error s -> raise (Invalid_argument s)

(**********************************************************************)
(* COMMANDS                                                           *)
(**********************************************************************)

let cmd_drop st =
  if st.stack.len > 0 then
    let _, s = stack_pop st.stack in
    { st with stack = s }
  else st

let cmd_clear st = { st with stack = empty_stack }

let cmd_swap st = { st with stack = stack_swap st.stack }
let cmd_dup st = { st with stack = stack_dup st.stack }
let cmd_undo st = state_restore st

(**********************************************************************)
(* VARIABLES                                                          *)
(**********************************************************************)

let get_variables st = st.variables

let cmd_store st =
  check_args 2 "store" st;
  let name_el, st1 = pop1 st in
  let value, st0 = pop1 st1 in
  match name_el with
  | RpcVariable vname ->
      Hashtbl.replace st0.variables vname value;
      st0
  | _ -> raise (Invalid_argument "store requires variable name as first argument")

let cmd_purge st =
  check_args 1 "purge" st;
  let name_el, st0 = pop1 st in
  match name_el with
  | RpcVariable vname ->
      Hashtbl.remove st0.variables vname;
      st0
  | _ -> raise (Invalid_argument "purge requires variable name")

let cmd_eval st = eval1 st

(**********************************************************************)
(* RENDERING                                                          *)
(**********************************************************************)

let string_of_big_int_base_gen n base =
  let rec aux n acc =
    if eq_big_int n zero_big_int then acc
    else
      let q, r = quomod_big_int n (big_int_of_int base) in
      let digit =
        if ge_big_int r (big_int_of_int 10) then
          String.make 1 (Char.chr (int_of_big_int r + 87))
        else string_of_int (int_of_big_int r)
      in
      aux q (digit ^ acc)
  in
  if eq_big_int n zero_big_int then "0"
  else if sign_big_int n < 0 then "-" ^ aux (minus_big_int n) ""
  else aux n ""

let render_int calc_modes i =
  match calc_modes.base with
  | Bin -> "# " ^ string_of_big_int_base_gen i 2 ^ "`b"
  | Oct -> "# " ^ string_of_big_int_base_gen i 8 ^ "`o"
  | Hex -> "# " ^ string_of_big_int_base_gen i 16 ^ "`h"
  | Dec -> "# " ^ string_of_big_int_base_gen i 10 ^ "`d"

let render_float_unit ff uu =
  if uu <> Units.empty_unit then
    sprintf "%.15g_%s" ff (Units.string_of_units uu)
  else
    sprintf "%.15g" ff

let render_complex_unit calc_modes cc uu =
  let append_units ss =
    if uu <> Units.empty_unit then ss ^ "_" ^ Units.string_of_units uu else ss
  in
  match calc_modes.complex with
  | Rect ->
      append_units (sprintf "(%.15g, %.15g)" cc.Complex.re cc.Complex.im)
  | Polar ->
      let r = sqrt (cc.Complex.re *. cc.Complex.re +. cc.Complex.im *. cc.Complex.im)
      and theta = atan2 cc.Complex.im cc.Complex.re in
      match calc_modes.angle with
      | Rad -> append_units (sprintf "(%.15g <%.15g)" r theta)
      | Deg -> append_units (sprintf "(%.15g <%.15g)" r (180.0 /. pi *. theta))

let render_float_matrix_line fm uu =
  let append_units ss =
    if uu <> Units.empty_unit then ss ^ "_" ^ Units.string_of_units uu else ss
  in
  let rows, cols = Gsl.Matrix.dims fm in
  let line = ref "[" in
  for n = 0 to rows - 1 do
    line := !line ^ "[ ";
    for m = 0 to cols - 2 do
      line := !line ^ sprintf "%.15g, " fm.{n, m}
    done;
    line := !line ^ sprintf "%.15g ]" fm.{n, cols-1}
  done;
  line := !line ^ "]";
  append_units !line

let render_complex_matrix_line calc_modes cm uu =
  let append_units ss =
    if uu <> Units.empty_unit then ss ^ "_" ^ Units.string_of_units uu else ss
  in
  let rows, cols = Gsl.Matrix_complex.dims cm in
  let line = ref "[" in
  for n = 0 to rows - 1 do
    line := !line ^ "[ ";
    for m = 0 to cols - 2 do
      (match calc_modes.complex with
      | Rect ->
          line := !line ^ sprintf "(%.15g, %.15g), " cm.{n, m}.Complex.re cm.{n, m}.Complex.im
      | Polar ->
          let rr = cm.{n, m}.Complex.re and ii = cm.{n, m}.Complex.im in
          let r = sqrt (rr *. rr +. ii *. ii) and theta = atan2 ii rr in
          (match calc_modes.angle with
          | Rad -> line := !line ^ sprintf "(%.15g <%.15g), " r theta
          | Deg -> line := !line ^ sprintf "(%.15g <%.15g), " r (180.0 /. pi *. theta)));
    done;
    (match calc_modes.complex with
    | Rect ->
        line := !line ^ sprintf "(%.15g, %.15g) ]" cm.{n, cols-1}.Complex.re cm.{n, cols-1}.Complex.im
    | Polar ->
        let rr = cm.{n, cols-1}.Complex.re and ii = cm.{n, cols-1}.Complex.im in
        let r = sqrt (rr *. rr +. ii *. ii) and theta = atan2 ii rr in
        (match calc_modes.angle with
        | Rad -> line := !line ^ sprintf "(%.15g <%.15g) ]" r theta
        | Deg -> line := !line ^ sprintf "(%.15g <%.15g) ]" r (180.0 /. pi *. theta)));
  done;
  line := !line ^ "]";
  append_units !line

let render_variable_line vv = "@ " ^ vv

let render_data calc_modes d =
  match d with
  | RpcInt i -> render_int calc_modes i
  | RpcFloatUnit (f, u) -> render_float_unit f u
  | RpcComplexUnit (c, u) -> render_complex_unit calc_modes c u
  | RpcFloatMatrixUnit (m, u) -> render_float_matrix_line m u
  | RpcComplexMatrixUnit (m, u) -> render_complex_matrix_line calc_modes m u
  | RpcVariable v -> render_variable_line v

let get_display_line line_num st =
  if line_num > 0 && line_num <= st.stack.len then
    render_data st.modes st.stack.data.(st.stack.len - line_num)
  else if line_num > st.stack.len then
    ""
  else
    let s = sprintf "cannot display nonexistent stack element %d" line_num in
    stack_failwith s

let get_fullscreen_display line_num st =
  if line_num > 0 && line_num <= st.stack.len then
    render_data st.modes st.stack.data.(st.stack.len - line_num)
  else if line_num > st.stack.len then
    ""
  else
    let s = sprintf "cannot display nonexistent stack element %d" line_num in
    stack_failwith s

(**********************************************************************)
(* KEY BINDINGS                                                       *)
(**********************************************************************)

type key_t =
  | Key_char of Uchar.t
  | Key_enter
  | Key_tab
  | Key_backspace
  | Key_delete
  | Key_escape
  | Key_up | Key_down | Key_left | Key_right
  | Key_home | Key_end | Key_page_up | Key_page_down | Key_insert
  | Key_f of int
  | Key_space
  | Key_unknown of int

type key_binding = {
  key  : key_t;
  ctrl : bool;
  meta : bool;
}

let decode_alias str =
  match str with
  | "<esc>"     -> Key_escape
  | "<tab>"     -> Key_tab
  | "<enter>"   -> Key_enter
  | "<return>"  -> Key_enter
  | "<insert>"  -> Key_insert
  | "<delete>"  -> Key_delete
  | "<home>"    -> Key_home
  | "<end>"     -> Key_end
  | "<pageup>"  -> Key_page_up
  | "<pagedown>"-> Key_page_down
  | "<space>"   -> Key_space
  | "<backspace>" -> Key_backspace
  | "<left>"    -> Key_left
  | "<right>"   -> Key_right
  | "<up>"      -> Key_up
  | "<down>"    -> Key_down
  | "<f1>"      -> Key_f 1
  | "<f2>"      -> Key_f 2
  | "<f3>"      -> Key_f 3
  | "<f4>"      -> Key_f 4
  | "<f5>"      -> Key_f 5
  | "<f6>"      -> Key_f 6
  | "<f7>"      -> Key_f 7
  | "<f8>"      -> Key_f 8
  | "<f9>"      -> Key_f 9
  | "<f10>"     -> Key_f 10
  | "<f11>"     -> Key_f 11
  | "<f12>"     -> Key_f 12
  | _ ->
      if String.length str = 1 then
        Key_char (Uchar.of_char str.[0])
      else
        raise (Invalid_argument ("Unrecognized key \"" ^ str ^ "\""))

let decode_single_key_string key_string =
  let len = String.length key_string in
  let i, ctrl, meta =
    if len >= 4 &&
       ((key_string.[0] = '\\' && key_string.[1] = 'M' && key_string.[2] = '\\' && key_string.[3] = 'C')
     || (key_string.[0] = '\\' && key_string.[1] = 'C' && key_string.[2] = '\\' && key_string.[3] = 'M'))
    then (4, true, true)
    else if len >= 2 && key_string.[0] = '\\' && key_string.[1] = 'M' then (2, false, true)
    else if len >= 2 && key_string.[0] = '\\' && key_string.[1] = 'C' then (2, true, false)
    else (0, false, false)
  in
  if i >= len then
    raise (Invalid_argument ("empty key binding in \"" ^ key_string ^ "\""));
  let main_key = String.sub key_string i (len - i) in
  let key =
    if ctrl && meta then
      if String.length main_key = 1 then
        let uc = String.uppercase_ascii main_key in
        let ch = Char.code uc.[0] + 64 in
        Key_char (Uchar.of_int ch)
      else
        raise (Invalid_argument ("Cannot apply \\M\\C to key \"" ^ main_key ^ "\""))
    else if meta then
      if String.length main_key = 1 then
        let ch = Char.code main_key.[0] + 128 in
        Key_char (Uchar.of_int ch)
      else
        raise (Invalid_argument ("Cannot apply \\M to key \"" ^ main_key ^ "\""))
    else if ctrl then
      if String.length main_key = 1 then
        let uc = String.uppercase_ascii main_key in
        let ch = Char.code uc.[0] - 64 in
        if ch = 10 then Key_enter
        else if ch = 9 then Key_tab
        else Key_char (Uchar.of_int ch)
      else
        raise (Invalid_argument ("Cannot apply \\C to key \"" ^ main_key ^ "\""))
    else
      if String.length main_key >= 2 && main_key.[0] = '0' &&
         (main_key.[1] = 'o' || main_key.[1] = 'O')
      then
        try Key_char (Uchar.of_int (int_of_string main_key))
        with _ -> decode_alias main_key
      else
        decode_alias main_key
  in
  { key; ctrl; meta }

let string_of_key_binding kb =
  let prefix =
    if kb.ctrl && kb.meta then "\\M\\C"
    else if kb.meta then "\\M"
    else if kb.ctrl then "\\C"
    else ""
  in
  let key_str =
    match kb.key with
    | Key_char c when Uchar.to_int c < 256 ->
        let ch = Char.chr (Uchar.to_int c) in
        if ch = ' ' then "<space>" else String.make 1 ch
    | Key_enter -> "<enter>"
    | Key_tab -> "<tab>"
    | Key_backspace -> "<backspace>"
    | Key_delete -> "<delete>"
    | Key_escape -> "<esc>"
    | Key_insert -> "<insert>"
    | Key_home -> "<home>"
    | Key_end -> "<end>"
    | Key_page_up -> "<pageup>"
    | Key_page_down -> "<pagedown>"
    | Key_up -> "<up>"
    | Key_down -> "<down>"
    | Key_left -> "<left>"
    | Key_right -> "<right>"
    | Key_f n -> "<f" ^ string_of_int n ^ ">"
    | Key_space -> "<space>"
    | Key_char c -> "\\" ^ Printf.sprintf "%03o" (Uchar.to_int c)
    | Key_unknown n -> "\\" ^ Printf.sprintf "%03o" n
  in
  prefix ^ key_str
