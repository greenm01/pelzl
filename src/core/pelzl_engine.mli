(*  Pelzl -- a modern RPN calculator for the console
 *  Copyright (C) 2003-2004, 2005, 2006-2007, 2010, 2018 Paul Pelzl
 *  Copyright (C) 2026 Mason Austin Green
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License, Version 3,
 *  as published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *)

(* pelzl_engine.ml
 * Functional, data-oriented calculator engine.
 * Replaces the mutable object-oriented rpc_stack and rpc_calc modules. *)

open Big_int

exception Stack_error of string

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

val empty_stack : stack_state
val stack_length : stack_state -> int
val stack_push : pelzl_data_t -> stack_state -> stack_state
val stack_pop : stack_state -> (pelzl_data_t * stack_state)
val stack_peek : int -> stack_state -> pelzl_data_t
val stack_dup : stack_state -> stack_state
val stack_swap : stack_state -> stack_state
val stack_rolldown : int -> stack_state -> stack_state
val stack_rollup : int -> stack_state -> stack_state
val stack_delete : int -> stack_state -> stack_state
val stack_deleteN : int -> stack_state -> stack_state
val stack_keep : int -> stack_state -> stack_state
val stack_keepN : int -> stack_state -> stack_state
val stack_echo : int -> stack_state -> stack_state

(**********************************************************************)
(* CALC STATE                                                         *)
(**********************************************************************)

val empty_state : calc_state
val state_backup : calc_state -> calc_state
val state_restore : calc_state -> calc_state

val mode_rad : calc_state -> calc_state
val mode_deg : calc_state -> calc_state
val mode_rect : calc_state -> calc_state
val mode_polar : calc_state -> calc_state
val mode_bin : calc_state -> calc_state
val mode_oct : calc_state -> calc_state
val mode_dec : calc_state -> calc_state
val mode_hex : calc_state -> calc_state
val toggle_angle_mode : calc_state -> calc_state
val toggle_complex_mode : calc_state -> calc_state
val cycle_base : calc_state -> calc_state

val get_modes : calc_state -> calculator_modes
val set_modes : calculator_modes -> calc_state -> calc_state

(**********************************************************************)
(* RENDERING                                                          *)
(**********************************************************************)

val get_display_line : int -> calc_state -> string
val get_fullscreen_display : int -> calc_state -> string

(**********************************************************************)
(* UNARY OPERATIONS                                                   *)
(**********************************************************************)

val calc_neg : calc_state -> calc_state
val calc_inv : calc_state -> calc_state
val calc_sqrt : calc_state -> calc_state
val calc_sq : calc_state -> calc_state
val calc_abs : calc_state -> calc_state
val calc_arg : calc_state -> calc_state
val calc_exp : calc_state -> calc_state
val calc_ln : calc_state -> calc_state
val calc_ten_x : calc_state -> calc_state
val calc_log10 : calc_state -> calc_state
val calc_conj : calc_state -> calc_state
val calc_sin : calc_state -> calc_state
val calc_cos : calc_state -> calc_state
val calc_tan : calc_state -> calc_state
val calc_asin : calc_state -> calc_state
val calc_acos : calc_state -> calc_state
val calc_atan : calc_state -> calc_state
val calc_sinh : calc_state -> calc_state
val calc_cosh : calc_state -> calc_state
val calc_tanh : calc_state -> calc_state
val calc_asinh : calc_state -> calc_state
val calc_acosh : calc_state -> calc_state
val calc_atanh : calc_state -> calc_state
val calc_re : calc_state -> calc_state
val calc_im : calc_state -> calc_state
val calc_gamma : calc_state -> calc_state
val calc_lngamma : calc_state -> calc_state
val calc_erf : calc_state -> calc_state
val calc_erfc : calc_state -> calc_state
val calc_fact : calc_state -> calc_state
val calc_transpose : calc_state -> calc_state
val calc_mod : calc_state -> calc_state
val calc_floor : calc_state -> calc_state
val calc_ceiling : calc_state -> calc_state
val calc_to_int : calc_state -> calc_state
val calc_to_float : calc_state -> calc_state
val calc_rand : calc_state -> calc_state
val calc_enter_pi : calc_state -> calc_state
val calc_utpn : calc_state -> calc_state
val calc_trace : calc_state -> calc_state
val calc_unit_value : calc_state -> calc_state

(**********************************************************************)
(* BINARY OPERATIONS                                                  *)
(**********************************************************************)

val calc_add : calc_state -> calc_state
val calc_sub : calc_state -> calc_state
val calc_mult : calc_state -> calc_state
val calc_div : calc_state -> calc_state
val calc_pow : calc_state -> calc_state
val calc_gcd : calc_state -> calc_state
val calc_lcm : calc_state -> calc_state
val calc_binom : calc_state -> calc_state
val calc_perm : calc_state -> calc_state
val calc_convert_units : calc_state -> calc_state
val calc_standardize_units : calc_state -> calc_state
val calc_solve_linear : calc_state -> calc_state

(**********************************************************************)
(* N-ARY / MATRIX STATISTICS                                          *)
(**********************************************************************)

val calc_total : calc_state -> calc_state
val calc_mean : calc_state -> calc_state
val calc_sumsq : calc_state -> calc_state
val calc_var : calc_state -> calc_state
val calc_varbias : calc_state -> calc_state
val calc_stdev : calc_state -> calc_state
val calc_stdevbias : calc_state -> calc_state
val calc_min : calc_state -> calc_state
val calc_max : calc_state -> calc_state

(**********************************************************************)
(* STACK COMMANDS                                                     *)
(**********************************************************************)

val cmd_drop : calc_state -> calc_state
val cmd_clear : calc_state -> calc_state
val cmd_swap : calc_state -> calc_state
val cmd_dup : calc_state -> calc_state
val cmd_undo : calc_state -> calc_state

(**********************************************************************)
(* VARIABLES                                                          *)
(**********************************************************************)

val cmd_store : calc_state -> calc_state
val cmd_purge : calc_state -> calc_state
val cmd_eval : calc_state -> calc_state
val eval1 : calc_state -> calc_state
val evaln : int -> calc_state -> calc_state
val get_variables : calc_state -> (string, pelzl_data_t) Hashtbl.t

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

val decode_single_key_string : string -> key_binding
val string_of_key_binding : key_binding -> string

