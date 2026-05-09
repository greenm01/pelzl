open Pelzl_model

module Algebraic = struct
  type token =
    | Num of float
    | Op of char
    | LParen
    | RParen
    | Var of string
    | Func of string
    | Comma
    | Assign

  let is_digit c = match c with '0'..'9' | '.' -> true | _ -> false
  let is_alpha c = match c with 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false
  let is_alnum c = is_alpha c || (match c with '0'..'9' -> true | _ -> false)

  let tokenize s =
    let rec aux i acc =
      if i >= String.length s then List.rev acc
      else
        match s.[i] with
        | ' ' | '\t' -> aux (i + 1) acc
        | '+' | '-' | '*' | '/' | '%' | '^' as c -> 
            aux (i + 1) (Op c :: acc)
        | '(' -> 
            let acc = match acc with
              | Num _ :: _ | Var _ :: _ | RParen :: _ -> Op '*' :: acc
              | _ -> acc
            in
            aux (i + 1) (LParen :: acc)
        | ')' -> aux (i + 1) (RParen :: acc)
        | ',' -> aux (i + 1) (Comma :: acc)
        | '=' -> aux (i + 1) (Assign :: acc)
        | c when is_digit c ->
            let start = i in
            let rec parse_num j =
              if j < String.length s && is_digit s.[j] then parse_num (j + 1) else j
            in
            let end_idx = parse_num i in
            let n = try float_of_string (String.sub s start (end_idx - start)) with _ -> 0.0 in
            let acc = match acc with
              | RParen :: _ -> Op '*' :: acc
              | _ -> acc
            in
            aux end_idx (Num n :: acc)
        | c when is_alpha c ->
            let start = i in
            let rec parse_ident j =
              if j < String.length s && is_alnum s.[j] then parse_ident (j + 1) else j
            in
            let end_idx = parse_ident i in
            let ident = String.sub s start (end_idx - start) in
            let acc = match acc with
              | Num _ :: _ | RParen :: _ -> Op '*' :: acc
              | _ -> acc
            in
            let rec skip_ws j = 
              if j < String.length s && (s.[j] = ' ' || s.[j] = '\t') then skip_ws (j + 1) else j
            in
            let next_non_ws = skip_ws end_idx in
            if next_non_ws < String.length s && s.[next_non_ws] = '(' then
              aux end_idx (Func ident :: acc)
            else
              aux end_idx (Var ident :: acc)
        | _ -> aux (i + 1) acc
    in
    aux 0 []

  let precedence = function
    | '^' -> 4
    | '*' | '/' | '%' -> 3
    | '+' | '-' -> 2
    | _ -> 0

  let is_left_assoc = function
    | '^' -> false
    | _ -> true

  let to_postfix tokens =
    let rec aux tokens stack output =
      match tokens with
      | [] -> 
          let rec pop_all st acc = match st with
            | [] -> List.rev acc
            | (Op _ | Func _) as t :: rest -> pop_all rest (t :: acc)
            | _ :: rest -> pop_all rest acc
          in pop_all stack output
      | Num n :: rest -> aux rest stack (Num n :: output)
      | Var v :: rest -> aux rest stack (Var v :: output)
      | Func f :: rest -> aux rest (Func f :: stack) output
      | LParen :: rest -> aux rest (LParen :: stack) output
      | Comma :: rest ->
          let rec pop_until_lparen st acc = match st with
            | LParen :: _ -> (st, acc)
            | t :: rest_st -> pop_until_lparen rest_st (t :: acc)
            | [] -> ([], acc)
          in
          let new_stack, new_output = pop_until_lparen stack output in
          aux rest new_stack new_output
      | RParen :: rest ->
          let rec pop_until_lparen st acc = match st with
            | LParen :: rest_st -> 
                (match rest_st with
                 | Func f :: rest_st2 -> (rest_st2, Func f :: acc)
                 | _ -> (rest_st, acc))
            | t :: rest_st -> pop_until_lparen rest_st (t :: acc)
            | [] -> ([], acc)
          in
          let new_stack, new_output = pop_until_lparen stack output in
          aux rest new_stack new_output
      | Op o1 :: rest ->
          let rec pop_higher st acc = match st with
            | Op o2 :: rest_st ->
                let p1, p2 = precedence o1, precedence o2 in
                if (is_left_assoc o1 && p1 <= p2) || (not (is_left_assoc o1) && p1 < p2) then
                  pop_higher rest_st (Op o2 :: acc)
                else (st, acc)
            | _ -> (st, acc)
          in
          let new_stack, new_output = pop_higher stack output in
          aux rest (Op o1 :: new_stack) new_output
      | Assign :: rest -> aux rest stack (Assign :: output)
    in
    aux tokens [] []
end

let parse_entry s =
  let len = String.length s in
  if len = 0 then Pelzl_engine.RpcVariable ""
  else
    let last = s.[len - 1] in
    if last = 'b' || last = 'o' || last = 'd' || last = 'h' then
      let base = match last with 'b' -> 2 | 'o' -> 8 | 'd' -> 10 | 'h' -> 16 | _ -> 10 in
      try
        let digits = String.sub s 0 (len - 1) in
        Pelzl_engine.RpcInt (Big_int_str.big_int_of_string_base digits base)
      with _ -> Pelzl_engine.RpcVariable s
    else
      try Pelzl_engine.RpcFloatUnit (float_of_string s, Units.empty_unit)
      with Failure _ -> Pelzl_engine.RpcVariable s

let add_history line model =
  if model.ui_mode = Repl then
    let history = model.history @ [line] in
    let len = List.length history in
    let history = if len > 200 then 
      let _, h = List.fold_left (fun (i, acc) x -> if i >= len - 200 then (i+1, acc @ [x]) else (i+1, acc)) (0, []) history in h
      else history 
    in
    { model with history }
  else
    model

let string_of_function = function
  | Operations.Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
  | Neg -> "neg" | Inv -> "inv" | Pow -> "^" | Sqrt -> "sqrt"
  | Sq -> "sq" | Abs -> "abs" | Arg -> "arg" | Exp -> "exp"
  | Ln -> "ln" | Ten_x -> "10^x" | Log10 -> "log10" | Conj -> "conj"
  | Sin -> "sin" | Cos -> "cos" | Tan -> "tan" | Asin -> "asin"
  | Acos -> "acos" | Atan -> "atan" | Sinh -> "sinh" | Cosh -> "cosh"
  | Tanh -> "tanh" | Asinh -> "asinh" | Acosh -> "acosh" | Atanh -> "atanh"
  | Re -> "re" | Im -> "im" | Gamma -> "gamma" | LnGamma -> "lngamma"
  | Erf -> "erf" | Erfc -> "erfc" | Fact -> "!" | Transpose -> "tr"
  | Mod -> "%" | Floor -> "floor" | Ceiling -> "ceil" | ToInt -> "to_int"
  | ToFloat -> "to_float" | SolveLin -> "solve" | Eval -> "eval"
  | Store -> "store" | Purge -> "purge" | Gcd -> "gcd" | Lcm -> "lcm"
  | Binom -> "binom" | Perm -> "perm" | Total -> "total" | Mean -> "mean"
  | Sumsq -> "sumsq" | Var -> "var" | VarBias -> "var_bias"
  | Stdev -> "stdev" | StdevBias -> "stdev_bias" | Min -> "min"
  | Max -> "max" | Utpn -> "utpn" | StandardizeUnits -> "std_units"
  | ConvertUnits -> "conv_units" | UnitValue -> "unit_val" | Trace -> "trace"

let push_entry model =
  if model.entry = "" then model
  else
    let entry = model.entry in
    let value = parse_entry entry in
    let new_calc = { model.calc with stack = Pelzl_engine.stack_push value model.calc.stack } in
    let model = { model with entry = ""; calc = new_calc } in
    add_history (Printf.sprintf "Push %s" entry) model

let exec_function model op =
  let model = push_entry model in
  try
    let calc = model.calc in
    let new_calc =
      match op with
      | Operations.Add -> Pelzl_engine.calc_add calc
      | Operations.Sub -> Pelzl_engine.calc_sub calc
      | Operations.Mult -> Pelzl_engine.calc_mult calc
      | Operations.Div -> Pelzl_engine.calc_div calc
      | Operations.Neg -> Pelzl_engine.calc_neg calc
      | Operations.Inv -> Pelzl_engine.calc_inv calc
      | Operations.Pow -> Pelzl_engine.calc_pow calc
      | Operations.Sqrt -> Pelzl_engine.calc_sqrt calc
      | Operations.Sq -> Pelzl_engine.calc_sq calc
      | Operations.Abs -> Pelzl_engine.calc_abs calc
      | Operations.Arg -> Pelzl_engine.calc_arg calc
      | Operations.Exp -> Pelzl_engine.calc_exp calc
      | Operations.Ln -> Pelzl_engine.calc_ln calc
      | Operations.Ten_x -> Pelzl_engine.calc_ten_x calc
      | Operations.Log10 -> Pelzl_engine.calc_log10 calc
      | Operations.Conj -> Pelzl_engine.calc_conj calc
      | Operations.Sin -> Pelzl_engine.calc_sin calc
      | Operations.Cos -> Pelzl_engine.calc_cos calc
      | Operations.Tan -> Pelzl_engine.calc_tan calc
      | Operations.Asin -> Pelzl_engine.calc_asin calc
      | Operations.Acos -> Pelzl_engine.calc_acos calc
      | Operations.Atan -> Pelzl_engine.calc_atan calc
      | Operations.Sinh -> Pelzl_engine.calc_sinh calc
      | Operations.Cosh -> Pelzl_engine.calc_cosh calc
      | Operations.Tanh -> Pelzl_engine.calc_tanh calc
      | Operations.Asinh -> Pelzl_engine.calc_asinh calc
      | Operations.Acosh -> Pelzl_engine.calc_acosh calc
      | Operations.Atanh -> Pelzl_engine.calc_atanh calc
      | Operations.Re -> Pelzl_engine.calc_re calc
      | Operations.Im -> Pelzl_engine.calc_im calc
      | Operations.Gamma -> Pelzl_engine.calc_gamma calc
      | Operations.LnGamma -> Pelzl_engine.calc_lngamma calc
      | Operations.Erf -> Pelzl_engine.calc_erf calc
      | Operations.Erfc -> Pelzl_engine.calc_erfc calc
      | Operations.Fact -> Pelzl_engine.calc_fact calc
      | Operations.Transpose -> Pelzl_engine.calc_transpose calc
      | Operations.Mod -> Pelzl_engine.calc_mod calc
      | Operations.Floor -> Pelzl_engine.calc_floor calc
      | Operations.Ceiling -> Pelzl_engine.calc_ceiling calc
      | Operations.ToInt -> Pelzl_engine.calc_to_int calc
      | Operations.ToFloat -> Pelzl_engine.calc_to_float calc
      | Operations.SolveLin -> Pelzl_engine.calc_solve_linear calc
      | Operations.Eval -> Pelzl_engine.cmd_eval calc
      | Operations.Store -> Pelzl_engine.cmd_store calc
      | Operations.Purge -> Pelzl_engine.cmd_purge calc
      | Operations.Gcd -> Pelzl_engine.calc_gcd calc
      | Operations.Lcm -> Pelzl_engine.calc_lcm calc
      | Operations.Binom -> Pelzl_engine.calc_binom calc
      | Operations.Perm -> Pelzl_engine.calc_perm calc
      | Operations.Total -> Pelzl_engine.calc_total calc
      | Operations.Mean -> Pelzl_engine.calc_mean calc
      | Operations.Sumsq -> Pelzl_engine.calc_sumsq calc
      | Operations.Var -> Pelzl_engine.calc_var calc
      | Operations.VarBias -> Pelzl_engine.calc_varbias calc
      | Operations.Stdev -> Pelzl_engine.calc_stdev calc
      | Operations.StdevBias -> Pelzl_engine.calc_stdevbias calc
      | Operations.Min -> Pelzl_engine.calc_min calc
      | Operations.Max -> Pelzl_engine.calc_max calc
      | Operations.Utpn -> Pelzl_engine.calc_utpn calc
      | Operations.StandardizeUnits -> Pelzl_engine.calc_standardize_units calc
      | Operations.ConvertUnits -> Pelzl_engine.calc_convert_units calc
      | Operations.UnitValue -> Pelzl_engine.calc_unit_value calc
      | Operations.Trace -> Pelzl_engine.calc_trace calc
    in
    let res_str = Pelzl_engine.get_display_line 1 new_calc in
    let model = { model with calc = new_calc; error_msg = None } in
    add_history (Printf.sprintf "Apply '%s' -> Result: %s" (string_of_function op) res_str) model
  with
  | Invalid_argument s -> { model with error_msg = Some s }
  | Pelzl_engine.Stack_error s -> { model with error_msg = Some s }

let exec_command model op =
  try
    let calc = model.calc in
    let new_calc =
      match op with
      | Operations.Drop -> Pelzl_engine.cmd_drop calc
      | Operations.Clear -> Pelzl_engine.cmd_clear calc
      | Operations.Swap -> Pelzl_engine.cmd_swap calc
      | Operations.Dup -> Pelzl_engine.cmd_dup calc
      | Operations.Undo -> Pelzl_engine.cmd_undo calc
      | Operations.ToggleAngleMode -> Pelzl_engine.toggle_angle_mode calc
      | Operations.ToggleComplexMode -> Pelzl_engine.toggle_complex_mode calc
      | Operations.CycleBase -> Pelzl_engine.cycle_base calc
      | Operations.SetRadians -> Pelzl_engine.mode_rad calc
      | Operations.SetDegrees -> Pelzl_engine.mode_deg calc
      | Operations.SetRect -> Pelzl_engine.mode_rect calc
      | Operations.SetPolar -> Pelzl_engine.mode_polar calc
      | Operations.SetBin -> Pelzl_engine.mode_bin calc
      | Operations.SetOct -> Pelzl_engine.mode_oct calc
      | Operations.SetDec -> Pelzl_engine.mode_dec calc
      | Operations.SetHex -> Pelzl_engine.mode_hex calc
      | Operations.EnterPi -> Pelzl_engine.calc_enter_pi calc
      | Operations.Rand -> Pelzl_engine.calc_rand calc
      | Operations.EditInput -> calc
      | Operations.CycleHelp -> calc
      | Operations.View -> calc
      | Operations.About -> calc
      | Operations.Refresh -> calc
      | Operations.BeginBrowse -> calc
      | Operations.BeginAbbrev -> calc
      | Operations.BeginConst -> calc
      | Operations.BeginVar -> calc
      | _ -> calc
    in
    let op_str = match op with
      | Operations.Drop -> "Drop" | Clear -> "Clear" | Swap -> "Swap"
      | Dup -> "Dup" | Undo -> "Undo" | EnterPi -> "PI" | Rand -> "Rand"
      | _ -> ""
    in
    let model = { model with calc = new_calc; error_msg = None } in
    if op_str <> "" then add_history op_str model else model
  with
  | Invalid_argument s -> { model with error_msg = Some s }
  | Pelzl_engine.Stack_error s -> { model with error_msg = Some s }

let exec_edit model op =
  match op with
  | Operations.Backspace ->
      let entry = if String.length model.entry > 0 then String.sub model.entry 0 (String.length model.entry - 1) else "" in
      { model with entry }
  | Operations.Enter -> push_entry model
  | Operations.Minus ->
      let entry =
        if String.length model.entry > 0 && model.entry.[0] = '-' then
          String.sub model.entry 1 (String.length model.entry - 1)
        else
          "-" ^ model.entry
      in
      { model with entry }
  | Operations.SciNotBase -> { model with entry = model.entry ^ "e" }
  | Operations.BeginInteger -> { model with entry_mode = Integer }
  | Operations.BeginComplex -> { model with entry_mode = Complex }
  | Operations.BeginMatrix -> { model with entry_mode = Matrix }
  | Operations.Separator -> { model with entry = model.entry ^ "," }
  | Operations.Angle -> { model with entry = model.entry ^ "<" }
  | Operations.BeginUnits -> { model with entry = model.entry ^ "_" }
  | Operations.Digit -> model

let eval_algebraic model =
  if model.entry = "" then { model with error_msg = None }
  else
    let original_entry = model.entry in
    try
      let tokens = Algebraic.tokenize original_entry in
      let var_name, expr_tokens = match tokens with
        | Algebraic.Var v :: Assign :: rest -> (Some v, rest)
        | _ -> (None, tokens)
      in
      let rpn = Algebraic.to_postfix expr_tokens in
      let rec eval tokens calc =
        match tokens with
        | [] -> calc
        | t :: rest ->
            let new_calc = match t with
              | Algebraic.Num n -> 
                  { calc with stack = Pelzl_engine.stack_push (Pelzl_engine.RpcFloatUnit (n, Units.empty_unit)) calc.stack }
              | Algebraic.Var v ->
                  let value = try Hashtbl.find calc.variables v with Not_found -> Pelzl_engine.RpcFloatUnit (0., Units.empty_unit) in
                  { calc with stack = Pelzl_engine.stack_push value calc.stack }
              | Algebraic.Op c ->
                  (match c with
                   | '+' -> Pelzl_engine.calc_add calc
                   | '-' -> Pelzl_engine.calc_sub calc
                   | '*' -> Pelzl_engine.calc_mult calc
                   | '/' -> Pelzl_engine.calc_div calc
                   | '%' -> Pelzl_engine.calc_mod calc
                   | '^' -> Pelzl_engine.calc_pow calc
                   | _ -> calc)
              | Algebraic.Func f ->
                  (match String.lowercase_ascii f with
                   | "sin" -> Pelzl_engine.calc_sin calc
                   | "cos" -> Pelzl_engine.calc_cos calc
                   | "tan" -> Pelzl_engine.calc_tan calc
                   | "asin" -> Pelzl_engine.calc_asin calc
                   | "acos" -> Pelzl_engine.calc_acos calc
                   | "atan" -> Pelzl_engine.calc_atan calc
                   | "sqrt" -> Pelzl_engine.calc_sqrt calc
                   | "ln" -> Pelzl_engine.calc_ln calc
                   | "log" -> Pelzl_engine.calc_log10 calc
                   | "abs" -> Pelzl_engine.calc_abs calc
                   | "exp" -> Pelzl_engine.calc_exp calc
                   | "ceil" -> Pelzl_engine.calc_ceiling calc
                   | "floor" -> Pelzl_engine.calc_floor calc
                   | _ -> calc)
              | _ -> calc
            in
            eval rest new_calc
      in
      let final_calc = eval rpn model.calc in
      let final_calc = match var_name with
        | Some v ->
            if Pelzl_engine.stack_length final_calc.stack > 0 then
              let top, _ = Pelzl_engine.stack_pop final_calc.stack in
              Hashtbl.replace final_calc.variables v top;
              final_calc
            else final_calc
        | None -> final_calc
      in
      let res_str = Pelzl_engine.get_display_line 1 final_calc in
      let model' = { model with calc = final_calc; entry = ""; error_msg = None } in
      let model'' = add_history (Printf.sprintf "> %s" original_entry) model' in
      add_history (Printf.sprintf "Result: %s" res_str) model''
    with _ -> { model with error_msg = Some "Invalid expression" }

let update msg model =
  match msg with
  | Key_input ev ->
      let data = Mosaic.Event.Key.data ev in
      let key_binding =
        let open Pelzl_engine in
        let k = match data.key with
        | Char c -> Key_char c
        | Enter -> Key_enter
        | Tab -> Key_tab
        | Backspace -> Key_backspace
        | Delete -> Key_delete
        | Escape -> Key_escape
        | Up -> Key_up
        | Down -> Key_down
        | Left -> Key_left
        | Right -> Key_right
        | Home -> Key_home
        | End -> Key_end
        | Page_up -> Key_page_up
        | Page_down -> Key_page_down
        | Insert -> Key_insert
        | F n -> Key_f n
        | _ -> Key_unknown 0
        in
        { key = k; ctrl = data.modifier.ctrl; meta = data.modifier.alt }
      in
      let is_enter = (data.key = Enter) in
      if model.ui_mode = Repl then begin
        match data.key with
        | Enter -> update Enter model
        | Backspace -> update Backspace model
        | Char c when not data.modifier.ctrl && not data.modifier.alt ->
            let s = model.entry ^ String.make 1 (Uchar.to_char c) in
            { model with entry = s; error_msg = None }, Mosaic.Cmd.none
        | _ ->
            let op_opt =
              (try Some (Operations.Command (Rcfile.command_of_key key_binding)) with Not_found -> None)
            in
            (match op_opt with
             | Some (Operations.Command c) when c = Operations.Quit -> model, Mosaic.Cmd.quit
             | Some (Operations.Command c) when c = Operations.CycleHelp -> { model with show_help = not model.show_help }, Mosaic.Cmd.none
             | _ -> { model with error_msg = None }, Mosaic.Cmd.none)
      end else if model.entry <> "" && is_enter then
        push_entry model, Mosaic.Cmd.none
      else
        let op_opt =
          (try Some (Operations.Function (Rcfile.function_of_key key_binding)) with Not_found ->
           try Some (Operations.Command (Rcfile.command_of_key key_binding)) with Not_found ->
           try Some (Operations.Edit (Rcfile.edit_of_key key_binding)) with Not_found ->
           None)
        in
        (match op_opt with
        | Some (Operations.Function f) -> exec_function model f, Mosaic.Cmd.none
        | Some (Operations.Command c) ->
            if c = Operations.Quit then model, Mosaic.Cmd.quit
            else if c = Operations.CycleHelp then { model with show_help = not model.show_help }, Mosaic.Cmd.none
            else exec_command model c, Mosaic.Cmd.none
        | Some (Operations.Edit e) -> exec_edit model e, Mosaic.Cmd.none
        | Some (Operations.Browse _) | Some (Operations.Abbrev _) | Some (Operations.IntEdit _) | Some (Operations.VarEdit _) ->
            model, Mosaic.Cmd.none
        | None ->
            (match data.key with
            | Char c when not data.modifier.ctrl && not data.modifier.alt ->
                let s = model.entry ^ String.make 1 (Uchar.to_char c) in
                { model with entry = s; error_msg = None }, Mosaic.Cmd.none
            | _ -> { model with error_msg = None }, Mosaic.Cmd.none))
  | Backspace ->
      let entry = if String.length model.entry > 0 then String.sub model.entry 0 (String.length model.entry - 1) else "" in
      { model with entry; error_msg = None }, Mosaic.Cmd.none
  | Enter ->
      if model.ui_mode = Repl then
        eval_algebraic model, Mosaic.Cmd.none
      else
        { (push_entry model) with error_msg = None }, Mosaic.Cmd.none
  | Clear_error ->
      { model with error_msg = None }, Mosaic.Cmd.none
  | Toggle_help ->
      { model with show_help = not model.show_help }, Mosaic.Cmd.none
  | Toggle_angle ->
      { model with calc = Pelzl_engine.toggle_angle_mode model.calc }, Mosaic.Cmd.none
  | Toggle_complex ->
      { model with calc = Pelzl_engine.toggle_complex_mode model.calc }, Mosaic.Cmd.none
  | Cycle_base ->
      { model with calc = Pelzl_engine.cycle_base model.calc }, Mosaic.Cmd.none
  | Resize (w, h) ->
      { model with width = w; height = h }, Mosaic.Cmd.none
  | Quit ->
      model, Mosaic.Cmd.quit
