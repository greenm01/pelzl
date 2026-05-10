(*  Pelzl -- a modern RPN calculator for the console
 *  Copyright (C) 2003-2004, 2005, 2006-2007, 2010, 2018 Paul Pelzl
 *  Copyright (C) 2026 Mason Austin Green
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License, Version 3,
 *  as published by the Free Software Foundation.
 *)

(* rcfile.ml
 * Configuration file processing for Pelzl. *)

open Genlex;;
open Operations;;
open Pelzl_engine;;

exception Config_failure of string;;
let config_failwith s = raise (Config_failure s);;

(* These hashtables store conversions between key bindings and operations. *)
let table_key_function = Hashtbl.create 20;;
let table_function_key = Hashtbl.create 20;;
let table_key_command  = Hashtbl.create 20;;
let table_command_key  = Hashtbl.create 20;;
let table_key_edit     = Hashtbl.create 20;;
let table_edit_key     = Hashtbl.create 20;;
let table_key_browse   = Hashtbl.create 20;;
let table_browse_key   = Hashtbl.create 20;;
let table_abbrev_key   = Hashtbl.create 20;;
let table_key_abbrev   = Hashtbl.create 20;;
let table_intedit_key  = Hashtbl.create 20;;
let table_key_intedit  = Hashtbl.create 20;;
let table_key_macro    = Hashtbl.create 20;;
let table_key_varedit  = Hashtbl.create 20;;
let table_varedit_key  = Hashtbl.create 20;;

(* Default directory for pelzl data *)
let datadir = ref "~/.pelzl"
(* Default editor for fullscreen viewing *)
let editor = ref "vi";;
(* Whether or not to hide the help panel *)
let hide_help = ref false;;
(* Whether or not to conserve memory in favor of faster display *)
let conserve_memory = ref false;;
(* Autobinding keys *)
let autobind_keys_list : (key_binding * string * operation_t option * int) list ref = ref [];;
let autobind_keys = ref (Array.make 1 ({ key = Key_char (Uchar.of_char ' '); ctrl = false; meta = false }, "", None, 0));;
(* List of included rc files *)
let included_rcfiles : (string list) ref = ref [];;
(* Unit definition table *)



let function_of_key key =
   Hashtbl.find table_key_function key;;
let key_of_function f =
   Hashtbl.find table_function_key f;;
let command_of_key key =
   Hashtbl.find table_key_command key;;
let key_of_command command =
   Hashtbl.find table_command_key command;;
let edit_of_key key =
   Hashtbl.find table_key_edit key;;
let key_of_edit edit_op =
   Hashtbl.find table_edit_key edit_op;;
let browse_of_key key =
   Hashtbl.find table_key_browse key;;
let key_of_browse browse_op =
   Hashtbl.find table_browse_key browse_op;;
let abbrev_of_key key =
   Hashtbl.find table_key_abbrev key;;
let key_of_abbrev ex_op =
   Hashtbl.find table_abbrev_key ex_op;;
let intedit_of_key key =
   Hashtbl.find table_key_intedit key;;
let key_of_intedit edit_op =
   Hashtbl.find table_intedit_key edit_op;;
let macro_of_key key =
   Hashtbl.find table_key_macro key;;
let varedit_of_key key =
   Hashtbl.find table_key_varedit key;;
let key_of_varedit edit_op =
   Hashtbl.find table_varedit_key edit_op;;


let key_of_operation (op : operation_t) =
   match op with
   |Function x -> Hashtbl.find table_function_key x
   |Command x  -> Hashtbl.find table_command_key x
   |Edit x     -> Hashtbl.find table_edit_key x
   |Browse x   -> Hashtbl.find table_browse_key x
   |Abbrev x   -> Hashtbl.find table_abbrev_key x
   |IntEdit x  -> Hashtbl.find table_intedit_key x
   |VarEdit x  -> Hashtbl.find table_varedit_key x


(* abbreviations used in abbreviation entry mode *)
let abbrev_commands = ref [];;
let abbrev_command_table = Hashtbl.create 50;;
let command_abbrev_table = Hashtbl.create 50;;

(* Register an abbreviation for an operation *)
let register_abbrev abbr op =
   let regex = Str.regexp_string abbr in
   let check_match (prev_result : bool) el =
      if prev_result then
         true
      else
         Str.string_match regex el 0
   in
   if List.fold_left check_match false !abbrev_commands then
      abbrev_commands := !abbrev_commands @ [abbr]
   else
      abbrev_commands := abbr :: !abbrev_commands;
   Hashtbl.add abbrev_command_table abbr op;
   Hashtbl.add command_abbrev_table op abbr;;


(* tables used in for constant entry *)
let constant_symbols = ref [];;
let constants_table = Hashtbl.create 50;;

(* Register a constant string. *)
let register_constant (const : string) (unit_def : Units.unit_def_t) =
   let regex = Str.regexp_string const in
   let check_match (prev_result : bool) el =
      if prev_result then
         true
      else
         Str.string_match regex el 0
   in
   if List.fold_left check_match false !constant_symbols then
      constant_symbols := !constant_symbols @ [const]
   else
      constant_symbols := const :: !constant_symbols;
   Hashtbl.add constants_table const unit_def;;


(* remove an abbreviation for a command. *)
let unregister_abbrev abbr =
   let remove_matching out_list el =
      if el = abbr then out_list
      else el :: out_list
   in
   let sublist =
      List.fold_left remove_matching [] !abbrev_commands
   in
   abbrev_commands := List.rev sublist;
   try
      let op = Hashtbl.find abbrev_command_table abbr in
      Hashtbl.remove abbrev_command_table abbr;
      Hashtbl.remove command_abbrev_table op
   with Not_found -> ();;


let translate_abbrev abb =
   Hashtbl.find abbrev_command_table abb;;
let abbrev_of_operation op =
   Hashtbl.find command_abbrev_table op;;

let translate_constant const =
   Hashtbl.find constants_table const;;


let decode_single_key_string key_string =
   try
      let kb = Pelzl_engine.decode_single_key_string key_string in
      let str = Pelzl_engine.string_of_key_binding kb in
      (kb, str)
   with Invalid_argument s ->
      config_failwith s


(* Register a key binding. *)
let register_binding_internal k k_string op =
   match op with
   |Function x ->
      Hashtbl.add table_key_function k x;
      Hashtbl.add table_function_key x k_string
   |Command x ->
      Hashtbl.add table_key_command k x;
      Hashtbl.add table_command_key x k_string
   |Edit x ->
      Hashtbl.add table_key_edit k x;
      Hashtbl.add table_edit_key x k_string
   |Browse x ->
      Hashtbl.add table_key_browse k x;
      Hashtbl.add table_browse_key x k_string
   |Abbrev x ->
      Hashtbl.add table_key_abbrev k x;
      Hashtbl.add table_abbrev_key x k_string
   |IntEdit x ->
      Hashtbl.add table_key_intedit k x;
      Hashtbl.add table_intedit_key x k_string
   |VarEdit x ->
      Hashtbl.add table_key_varedit k x;
      Hashtbl.add table_varedit_key x k_string


let register_binding key_string op =
   let k, string_rep = decode_single_key_string key_string in
   register_binding_internal k string_rep op


(* Unregister key bindings. *)
let unregister_function_binding key_string =
   let k, _ = decode_single_key_string key_string in
   try
      let op = Hashtbl.find table_key_function k in
      Hashtbl.remove table_key_function k;
      Hashtbl.remove table_function_key op
   with Not_found -> ()

let unregister_command_binding key_string =
   let k, _ = decode_single_key_string key_string in
   try
      let op = Hashtbl.find table_key_command k in
      Hashtbl.remove table_key_command k;
      Hashtbl.remove table_command_key op
   with Not_found -> ()

let unregister_edit_binding key_string =
   let k, _ = decode_single_key_string key_string in
   try
      let op = Hashtbl.find table_key_edit k in
      Hashtbl.remove table_key_edit k;
      Hashtbl.remove table_edit_key op
   with Not_found -> ()

let unregister_browse_binding key_string =
   let k, _ = decode_single_key_string key_string in
   try
      let op = Hashtbl.find table_key_browse k in
      Hashtbl.remove table_key_browse k;
      Hashtbl.remove table_browse_key op
   with Not_found -> ()

let unregister_abbrev_binding key_string =
   let k, _ = decode_single_key_string key_string in
   try
      let op = Hashtbl.find table_key_abbrev k in
      Hashtbl.remove table_key_abbrev k;
      Hashtbl.remove table_abbrev_key op
   with Not_found -> ()

let unregister_intedit_binding key_string =
   let k, _ = decode_single_key_string key_string in
   try
      let op = Hashtbl.find table_key_intedit k in
      Hashtbl.remove table_key_intedit k;
      Hashtbl.remove table_intedit_key op
   with Not_found -> ()

let unregister_varedit_binding key_string =
   let k, _ = decode_single_key_string key_string in
   try
      let op = Hashtbl.find table_key_varedit k in
      Hashtbl.remove table_key_varedit k;
      Hashtbl.remove table_varedit_key op
   with Not_found -> ()


(* Remove a key binding. *)
let remove_binding k op =
   match op with
   |Function x ->
      Hashtbl.remove table_key_function k;
      Hashtbl.remove table_function_key x
   |Command x ->
      Hashtbl.remove table_key_command k;
      Hashtbl.remove table_command_key x
   |Edit x ->
      Hashtbl.remove table_key_edit k;
      Hashtbl.remove table_edit_key x
   |Browse x ->
      Hashtbl.remove table_key_browse k;
      Hashtbl.remove table_browse_key x
   |Abbrev x ->
      Hashtbl.remove table_key_abbrev k;
      Hashtbl.remove table_abbrev_key x
   |IntEdit x ->
      Hashtbl.remove table_key_intedit k;
      Hashtbl.remove table_intedit_key x
   |VarEdit x ->
      Hashtbl.remove table_key_varedit k;
      Hashtbl.remove table_varedit_key x


(* Register a macro. *)
let register_macro key keys_string =
   let macro_kb, _ = decode_single_key_string key in
   let split_regex = Str.regexp "[ \t]+" in
   let keys_list = Str.split split_regex keys_string in
   let kb_of_key_string k_string =
      fst (decode_single_key_string k_string)
   in
   let kb_list = List.rev_map kb_of_key_string keys_list in
   Hashtbl.add table_key_macro macro_kb kb_list


(* translate a command string to the command type it represents *)
let operation_of_string command_str =
   begin match command_str with
   |"function_add"                  -> (Function Add)
   |"function_sub"                  -> (Function Sub)
   |"function_mult"                 -> (Function Mult)
   |"function_div"                  -> (Function Div)
   |"function_neg"                  -> (Function Neg)
   |"function_inv"                  -> (Function Inv)
   |"function_pow"                  -> (Function Pow)
   |"function_sq"                   -> (Function Sq)
   |"function_sqrt"                 -> (Function Sqrt)
   |"function_abs"                  -> (Function Abs)
   |"function_arg"                  -> (Function Arg)
   |"function_exp"                  -> (Function Exp)
   |"function_ln"                   -> (Function Ln)
   |"function_10_x"                 -> (Function Ten_x)
   |"function_log10"                -> (Function Log10)
   |"function_conj"                 -> (Function Conj)
   |"function_sin"                  -> (Function Sin)
   |"function_cos"                  -> (Function Cos)
   |"function_tan"                  -> (Function Tan)
   |"function_asin"                 -> (Function Asin)
   |"function_acos"                 -> (Function Acos)
   |"function_atan"                 -> (Function Atan)
   |"function_sinh"                 -> (Function Sinh)
   |"function_cosh"                 -> (Function Cosh)
   |"function_tanh"                 -> (Function Tanh)
   |"function_asinh"                -> (Function Asinh)
   |"function_acosh"                -> (Function Acosh)
   |"function_atanh"                -> (Function Atanh)
   |"function_re"                   -> (Function Re)
   |"function_im"                   -> (Function Im)
   |"function_gamma"                -> (Function Gamma)
   |"function_lngamma"              -> (Function LnGamma)
   |"function_erf"                  -> (Function Erf)
   |"function_erfc"                 -> (Function Erfc)
   |"function_factorial"            -> (Function Fact)
   |"function_transpose"            -> (Function Transpose)
   |"function_mod"                  -> (Function Mod)
   |"function_floor"                -> (Function Floor)
   |"function_ceiling"              -> (Function Ceiling)
   |"function_to_int"               -> (Function ToInt)
   |"function_to_real"              -> (Function ToFloat)
   |"function_solve_linear"         -> (Function SolveLin)
   |"function_eval"                 -> (Function Eval)
   |"function_store"                -> (Function Store)
   |"function_purge"                -> (Function Purge)
   |"function_gcd"                  -> (Function Gcd)
   |"function_lcm"                  -> (Function Lcm)
   |"function_binomial_coeff"       -> (Function Binom)
   |"function_permutation"          -> (Function Perm)
   |"function_total"                -> (Function Total)
   |"function_mean"                 -> (Function Mean)
   |"function_sumsq"                -> (Function Sumsq)
   |"function_var_unbiased"         -> (Function Var)
   |"function_var_biased"           -> (Function VarBias)
   |"function_stdev_unbiased"       -> (Function Stdev)
   |"function_stdev_biased"         -> (Function StdevBias)
   |"function_minimum"              -> (Function Min)
   |"function_maximum"              -> (Function Max)
   |"function_utpn"                 -> (Function Utpn)
   |"function_standardize_units"    -> (Function StandardizeUnits)
   |"function_convert_units"        -> (Function ConvertUnits)
   |"function_unit_value"           -> (Function UnitValue)
   |"function_trace"                -> (Function Trace)
   |"edit_begin_integer"            -> (Edit BeginInteger)
   |"edit_complex"                  -> (Edit BeginComplex)
   |"edit_matrix"                   -> (Edit BeginMatrix)
   |"edit_separator"                -> (Edit Separator)
   |"edit_angle"                    -> (Edit Angle)
   |"edit_minus"                    -> (Edit Minus)
   |"edit_backspace"                -> (Edit Backspace)
   |"edit_enter"                    -> (Edit Enter)
   |"edit_scientific_notation_base" -> (Edit SciNotBase)
   |"edit_begin_units"              -> (Edit BeginUnits)
   |"command_drop"                  -> (Command Drop)
   |"command_clear"                 -> (Command Clear)
   |"command_swap"                  -> (Command Swap)
   |"command_dup"                   -> (Command Dup)
   |"command_undo"                  -> (Command Undo)
   |"command_begin_browsing"        -> (Command BeginBrowse)
   |"command_begin_abbrev"          -> (Command BeginAbbrev)
   |"command_begin_constant"        -> (Command BeginConst)
   |"command_begin_variable"        -> (Command BeginVar)
   |"command_quit"                  -> (Command Quit)
   |"command_rad"                   -> (Command SetRadians)
   |"command_deg"                   -> (Command SetDegrees)
   |"command_rect"                  -> (Command SetRect)
   |"command_polar"                 -> (Command SetPolar)
   |"command_bin"                   -> (Command SetBin)
   |"command_oct"                   -> (Command SetOct)
   |"command_dec"                   -> (Command SetDec)
   |"command_hex"                   -> (Command SetHex)
   |"command_toggle_angle_mode"     -> (Command ToggleAngleMode)
   |"command_toggle_complex_mode"   -> (Command ToggleComplexMode)
   |"command_cycle_base"            -> (Command CycleBase)
   |"command_view"                  -> (Command View)
   |"command_refresh"               -> (Command Refresh)
   |"command_about"                 -> (Command About)
   |"command_enter_pi"              -> (Command EnterPi)
   |"command_rand"                  -> (Command Rand)
   |"command_edit_input"            -> (Command EditInput)
   |"command_cycle_help"            -> (Command CycleHelp)
   |"command_repl"                  -> (Command SwitchRepl)
   |"browse_end"                    -> (Browse EndBrowse)
   |"browse_scroll_left"            -> (Browse ScrollLeft)
   |"browse_scroll_right"           -> (Browse ScrollRight)
   |"browse_prev_line"              -> (Browse PrevLine)
   |"browse_next_line"              -> (Browse NextLine)
   |"browse_rolldown"               -> (Browse RollDown)
   |"browse_rollup"                 -> (Browse RollUp)
   |"browse_echo"                   -> (Browse Echo)
   |"browse_view"                   -> (Browse ViewEntry)
   |"browse_drop"                   -> (Browse Drop1)
   |"browse_dropn"                  -> (Browse DropN)
   |"browse_keep"                   -> (Browse Keep)
   |"browse_keepn"                  -> (Browse KeepN)
   |"browse_edit"                   -> (Browse EditEntry)
   |"abbrev_exit"                   -> (Abbrev AbbrevExit)
   |"abbrev_enter"                  -> (Abbrev AbbrevEnter)
   |"abbrev_backspace"              -> (Abbrev AbbrevBackspace)
   |"integer_cancel"                -> (IntEdit IntEditExit)
   |"variable_cancel"               -> (VarEdit VarEditExit)
   |"variable_enter"                -> (VarEdit VarEditEnter)
   |"variable_backspace"            -> (VarEdit VarEditBackspace)
   |"variable_complete"             -> (VarEdit VarEditComplete)
   |"function_rand"                 -> config_failwith
                                       "operation \"function_rand\" is deprecated; please replace with \"command_rand\"."
   |"command_begin_extended"        -> config_failwith
                                       "operation \"command_begin_extended\" is deprecated; please replace with \"command_begin_abbrev\"."
   |"extended_exit"                 -> config_failwith
                                       "operation \"extended_exit\" is deprecated; please replace with \"abbrev_exit\"."
   |"extended_enter"                -> config_failwith
                                       "operation \"extended_enter\" is deprecated; please replace with \"abbrev_enter\"."
   |"extended_backspace"            -> config_failwith
                                       "operation \"extended_backspace\" is deprecated; please replace with \"abbrev_backspace\"."
   |_                               -> config_failwith ("Unknown command name \"" ^ command_str ^ "\"")
   end


(* Parse a line from a Pelzl configuration file. *)
let parse_line line_stream =
   match Stream.peek line_stream with
   | Some (Kwd "include") ->
      Stream.junk line_stream;
      begin match Stream.peek line_stream with
      | Some (String include_file) ->
         Stream.junk line_stream;
         included_rcfiles := include_file :: !included_rcfiles
      | _ ->
         config_failwith ("Expected a filename string after \"include\"")
      end
   | Some (Kwd "bind") ->
      Stream.junk line_stream;
      let bind_key key =
         begin match Stream.peek line_stream with
         | Some (Ident command_str) ->
            Stream.junk line_stream;
            let command = operation_of_string command_str in
            register_binding key command
         | _ ->
            config_failwith ("Expected a command name after \"bind \"" ^ key ^ "\"")
         end
      in
      begin match Stream.peek line_stream with
      | Some (String k) ->
         Stream.junk line_stream;
         bind_key k
      | Some (Ident "\\") ->
         Stream.junk line_stream;
         begin match Stream.peek line_stream with
         | Some (Int octal_int) ->
            Stream.junk line_stream;
            begin
               try
                  let octal_digits = "0o" ^ (string_of_int octal_int) in
                  bind_key octal_digits
               with
                  (Failure "int_of_string") -> config_failwith "Expected octal digits after \"\\\""
            end
         | _  ->
            config_failwith "Expected octal digits after \"\\\""
         end
      | _ ->
         config_failwith "Expected a key string after keyword \"bind\""
      end
   | Some (Kwd "unbind_function") ->
      Stream.junk line_stream;
      begin match Stream.peek line_stream with
      | Some (String k) ->
         Stream.junk line_stream;
         unregister_function_binding k
      | _ ->
         config_failwith ("Expected a key string after keyword \"unbind_function\"")
      end
   | Some (Kwd "unbind_command") ->
      Stream.junk line_stream;
      begin match Stream.peek line_stream with
      | Some (String k) ->
         Stream.junk line_stream;
         unregister_command_binding k
      | _ ->
         config_failwith ("Expected a key string after keyword \"unbind_command\"")
      end
   | Some (Kwd "unbind_edit") ->
      Stream.junk line_stream;
      begin match Stream.peek line_stream with
      | Some (String k) ->
         Stream.junk line_stream;
         unregister_edit_binding k
      | _ ->
         config_failwith ("Expected a key string after keyword \"unbind_edit\"")
      end
   | Some (Kwd "unbind_browse") ->
      Stream.junk line_stream;
      begin match Stream.peek line_stream with
      | Some (String k) ->
         Stream.junk line_stream;
         unregister_browse_binding k
      | _ ->
         config_failwith ("Expected a key string after keyword \"unbind_browse\"")
      end
   | Some (Kwd "unbind_abbrev") ->
      Stream.junk line_stream;
      begin match Stream.peek line_stream with
      | Some (String k) ->
         Stream.junk line_stream;
         unregister_abbrev_binding k
      | _ ->
         config_failwith ("Expected a key string after keyword \"unbind_abbrev\"")
      end
   | Some (Kwd "unbind_integer") ->
      Stream.junk line_stream;
      begin match Stream.peek line_stream with
      | Some (String k) ->
         Stream.junk line_stream;
         unregister_intedit_binding k
      | _ ->
         config_failwith ("Expected a key string after keyword \"unbind_integer\"")
      end
   | Some (Kwd "unbind_variable") ->
      Stream.junk line_stream;
      begin match Stream.peek line_stream with
      | Some (String k) ->
         Stream.junk line_stream;
         unregister_varedit_binding k
      | _ ->
         config_failwith ("Expected a key string after keyword \"unbind_variable\"")
      end
   | Some (Kwd "autobind") ->
      Stream.junk line_stream;
      begin match Stream.peek line_stream with
      | Some (String k) ->
         Stream.junk line_stream;
         let key, key_string = decode_single_key_string k in
         autobind_keys_list := (key, key_string, None, 1) :: !autobind_keys_list
      | Some (Ident "\\") ->
         Stream.junk line_stream;
         begin match Stream.peek line_stream with
         | Some (Int octal_int) ->
            Stream.junk line_stream;
            begin
               try
                  let octal_digits = "0o" ^ (string_of_int octal_int) in
                  let key, key_string = decode_single_key_string octal_digits in
                  autobind_keys_list := (key, key_string, None, 1) :: !autobind_keys_list
               with
                  (Failure "int_of_string") -> config_failwith "Expected octal digits after \"\\\""
            end
         | _  ->
            config_failwith "Expected octal digits after \"\\\""
         end
      | _ ->
         config_failwith "Expected a key string after keyword \"bind\""
      end
   | Some (Kwd "macro") ->
      Stream.junk line_stream;
      begin match Stream.peek line_stream with
      | Some (String key) ->
         Stream.junk line_stream;
         begin match Stream.peek line_stream with
         | Some (String generated_keys) ->
            Stream.junk line_stream;
            register_macro key generated_keys
         | _ ->
            config_failwith ("Expected a key string after \"macro \"" ^ key ^ "\"")
         end
      | _ ->
         config_failwith "Expected a key string after keyword \"macro\""
      end
   | Some (Kwd "abbrev") ->
      Stream.junk line_stream;
      begin match Stream.peek line_stream with
      | Some (String abbr) ->
         Stream.junk line_stream;
         begin match Stream.peek line_stream with
         | Some (Ident command_str) ->
            Stream.junk line_stream;
            let command = operation_of_string command_str in
            register_abbrev abbr command
         | _ ->
            config_failwith ("Expected a command name after \"abbrev \"" ^ abbr ^ "\"")
         end
      | _ ->
         config_failwith ("Expected an abbreviation string after \"abbrev\"")
      end
   | Some (Kwd "unabbrev") ->
      Stream.junk line_stream;
      begin match Stream.peek line_stream with
      | Some (String abbr) ->
         Stream.junk line_stream;
         unregister_abbrev abbr
      | _ ->
         config_failwith ("Expected an abbreviation string after \"unabbrev\"")
      end
   | Some (Kwd "set") ->
      Stream.junk line_stream;
      begin match Stream.peek line_stream with
      | Some (Ident "datadir") ->
         Stream.junk line_stream;
         begin match Stream.peek line_stream with
         | Some (Ident "=") ->
            Stream.junk line_stream;
            begin match Stream.peek line_stream with
            | Some (String dir) ->
               Stream.junk line_stream;
               datadir := dir
            | _ ->
               config_failwith ("Expected a directory string after " ^
               "\"set datadir = \"")
            end
         | _ ->
            config_failwith ("Expected \"=\" after \"set datadir\"")
         end
      | Some (Ident "editor") ->
         Stream.junk line_stream;
         begin match Stream.peek line_stream with
         | Some (Ident "=") ->
            Stream.junk line_stream;
            begin match Stream.peek line_stream with
            | Some (String executable) ->
               Stream.junk line_stream;
               editor := executable
            | _ ->
               config_failwith ("Expected an executable filename string after " ^
               "\"set editor = \"")
            end
         | _ ->
            config_failwith ("Expected \"=\" after \"set editor\"")
         end
      | Some (Ident "hide_help") ->
         Stream.junk line_stream;
         begin match Stream.peek line_stream with
         | Some (Ident "=") ->
            Stream.junk line_stream;
            begin match Stream.peek line_stream with
            | Some (String setting) ->
               Stream.junk line_stream;
               if setting = "true" then
                  hide_help := true
               else if setting = "false" then
                  hide_help := false
               else
                  config_failwith ("Expected a boolean argument after " ^
                  "\"set hide_help = \"")
            | _ ->
               config_failwith ("Expected a boolean argument after " ^
               "\"set hide_help = \"")
            end
         | _ ->
            config_failwith ("Expected \"=\" after \"set hide_help\"")
         end
      | Some (Ident "conserve_memory") ->
         Stream.junk line_stream;
         begin match Stream.peek line_stream with
         | Some (Ident "=") ->
            Stream.junk line_stream;
            begin match Stream.peek line_stream with
            | Some (String setting) ->
               Stream.junk line_stream;
               if setting = "true" then
                  conserve_memory := true
               else if setting = "false" then
                  conserve_memory := false
               else
                  config_failwith ("Expected a boolean argument after " ^
                  "\"set conserve_memory = \"")
            | _ ->
               config_failwith ("Expected a boolean argument after " ^
               "\"set conserve_memory = \"")
            end
         | _ ->
            config_failwith ("Expected \"=\" after \"set conserve_memory\"")
         end
      | _ ->
         config_failwith ("Unmatched variable name after \"set\"")
      end
   | Some (Kwd "base_unit") ->
      Stream.junk line_stream;
      begin match Stream.peek line_stream with
      | Some (String base_u) ->
         Stream.junk line_stream;
         begin match Stream.peek line_stream with
         | Some (String prefix_s) ->
            Stream.junk line_stream;
            begin try
               let prefix = Units.prefix_of_string prefix_s in
               Units.unit_table := Units.add_base_unit base_u prefix !Units.unit_table
            with Not_found ->
               config_failwith
               ("Expected an SI prefix string (or null string) after: base_unit \"" ^
               base_u ^ "\"")
            end
         | _ ->
            config_failwith ("Expected a unit string and prefix string after \"base_unit\"")
         end
      | _ ->
         config_failwith ("Expected a unit string and prefix string after \"base_unit\"")
      end
   | Some (Kwd "unit") ->
      Stream.junk line_stream;
      begin match Stream.peek line_stream with
      | Some (String unit_str) ->
         Stream.junk line_stream;
         begin match Stream.peek line_stream with
         | Some (String unit_def_str) ->
            Stream.junk line_stream;
            begin try
               let unit_def = Units.unit_def_of_string unit_def_str !Units.unit_table in
               Units.unit_table := Units.add_unit unit_str unit_def !Units.unit_table
            with Units.Units_error s ->
               config_failwith ("Illegal unit definition: unit \"" ^
               unit_str ^ "\" \"" ^ unit_def_str ^ "\"; " ^ s)
            end
         | _ ->
            config_failwith ("Expected a unit string and definition after \"unit\"")
         end
      | _ ->
         config_failwith ("Expected a unit string and definition after \"unit\"")
      end
   | Some (Kwd "constant") ->
      Stream.junk line_stream;
      begin match Stream.peek line_stream with
      | Some (String const_str) ->
         Stream.junk line_stream;
         begin match Stream.peek line_stream with
         | Some (String unit_def_str) ->
            Stream.junk line_stream;
            begin try
               let unit_def = Units.unit_def_of_string unit_def_str !Units.unit_table in
               register_constant const_str unit_def
            with Units.Units_error s ->
               config_failwith ("Illegal constant definition: constant \"" ^
               const_str ^ "\" \"" ^ unit_def_str ^ "\"; " ^ s)
            end
         | _ ->
            config_failwith ("Expected a constant name and definition after \"constant\"")
         end
      | _ ->
         config_failwith ("Expected a constant name and definition after \"constant\"")
      end
   | Some (Kwd "#") ->
      Stream.junk line_stream;
      ()
   | None -> ()
   | _ ->
      config_failwith "Expected a keyword at start of line";;


(* obtain a valid autobinding array, eliminating duplicate keys *)
let generate_autobind_array () =
   let candidates = Array.of_list (List.rev !autobind_keys_list) in
   let temp_arr = Array.make (Array.length candidates) ({ key = Key_char (Uchar.of_char ' '); ctrl = false; meta = false }, "", None, 0) in
   let pointer = ref 0 in
   for i = 0 to pred (Array.length candidates) do
      let (c_k, c_ss, c_bound_f, c_age) = candidates.(i) in
      let matched = ref false in
      for j = 0 to !pointer do
         let (t_k, t_ss, t_bound_f, t_age) = temp_arr.(j) in
         if c_k = t_k then matched := true else ()
      done;
      if not !matched then begin
         temp_arr.(!pointer) <- candidates.(i);
         pointer := succ !pointer
      end else
         ()
   done;
   autobind_keys := Array.sub temp_arr 0 !pointer



(* compare a set of autobindings saved to disk to the set loaded from the
 * pelzlrc file.  If the autobindings match and the hashtbl abbreviations
 * are the same, then use the saved version. *)
let validate_saved_autobindings saved_autobind =
   if Array.length !autobind_keys = Array.length saved_autobind then
      let is_valid = ref true in
      for i = 0 to pred (Array.length saved_autobind) do
         let (s_key, s_key_str, s_bound_f, s_age) = saved_autobind.(i)
         and (n_key, n_key_str, n_bound_f, n_age) = !autobind_keys.(i) in
         if s_key = n_key then begin
            try
               begin match s_bound_f with
               |None -> ()
               |Some op ->
                  let _ = abbrev_of_operation op in ()
               end
            with Not_found ->
               is_valid := false
         end else
            is_valid := false
      done;
      if !is_valid then begin
         autobind_keys := saved_autobind;
         for i = 0 to pred (Array.length !autobind_keys) do
            let (n_key, n_key_str, n_bound_f, n_age) = !autobind_keys.(i) in
            match n_bound_f with
            |None    -> ()
            |Some op -> register_binding_internal n_key n_key_str op
         done
      end else
         ()
   else
      ()


(* try opening the rc file, first looking at $HOME/.pelzlrc,
 * then looking at $PREFIX/etc/pelzlrc *)
let open_rcfile rcfile_op =
   match rcfile_op with
   |None ->
      let home_rcfile =
         let homedir = Sys.getenv "HOME" in
         homedir ^ "/.pelzlrc"
      in
      let rcfile_fullpath =
         let prefix_regex = Str.regexp "\\${prefix}" in
         let expanded_sysconfdir = Str.global_replace prefix_regex
         Install.prefix Install.sysconfdir in
         Utility.join_path expanded_sysconfdir "pelzlrc"
      in
      begin try (open_in home_rcfile, home_rcfile)
      with Sys_error error_str ->
         begin try (open_in rcfile_fullpath, rcfile_fullpath)
         with Sys_error error_str -> failwith
            ("Could not open configuration file \"" ^ home_rcfile ^ "\" or \"" ^
            rcfile_fullpath ^ "\" .")
         end
      end
   |Some file ->
      try (Utility.expand_open_in_ascii file, file)
      with Sys_error error_str -> config_failwith
      ("Could not open configuration file \"" ^ file ^ "\".")


let rec process_rcfile rcfile_op =
   let line_lexer line =
      make_lexer
         ["include"; "bind"; "unbind_function"; "unbind_command";
         "unbind_edit"; "unbind_browse"; "unbind_abbrev"; "unbind_integer";
         "unbind_variable"; "autobind"; "abbrev"; "unabbrev"; "macro"; "set";
         "base_unit"; "unit"; "constant"; "#"]
      (Stream.of_string line)
   in
   let empty_regexp = Str.regexp "^[\t ]*$" in
   let config_stream, rcfile_filename = open_rcfile rcfile_op in
   let line_num = ref 0 in
   try
      while true do
         line_num := succ !line_num;
         let line_string = input_line config_stream in
         if Str.string_match empty_regexp line_string 0 then
            ()
         else
            try
               let line_stream = line_lexer line_string in
               parse_line line_stream;
               begin match !included_rcfiles with
               |[] -> ()
               |head :: tail ->
                  included_rcfiles := tail;
                  process_rcfile (Some head)
               end
            with
               |Config_failure s ->
                  (let error_str = Printf.sprintf "Syntax error on line %d of \"%s\": %s"
                  !line_num rcfile_filename s in
                  failwith error_str)
               |Stream.Failure ->
                  failwith (Printf.sprintf "Syntax error on line %d of \"%s\""
                  !line_num rcfile_filename)

      done
   with End_of_file ->
      begin
         close_in config_stream;
         generate_autobind_array ()
      end
