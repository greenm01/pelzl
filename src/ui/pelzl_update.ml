open Pelzl_model

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

let push_entry model =
  if model.entry = "" then model
  else
    let value = parse_entry model.entry in
    { model with entry = ""; calc = { model.calc with stack = Pelzl_engine.stack_push value model.calc.stack } }

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
    { model with calc = new_calc; error_msg = None }
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
    { model with calc = new_calc; error_msg = None }
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
      if model.entry <> "" && is_enter then
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
