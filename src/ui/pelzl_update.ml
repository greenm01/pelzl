open Pelzl_model

(* -------------------------------------------------------------------- *)
(* Helpers carried over from the legacy RPN/Classic implementation.     *)
(* The algebraic parser previously embedded here has moved to           *)
(* Pelzl_core.Pelzl_algebraic.                                          *)
(* -------------------------------------------------------------------- *)

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

(* Classic-mode trace log. Repl mode never uses this. *)
let add_trace line model =
  if model.ui_mode = Classic then
    let history = model.history @ [line] in
    let len = List.length history in
    let history =
      if len > 200 then
        let _, h =
          List.fold_left
            (fun (i, acc) x ->
              if i >= len - 200 then (i + 1, acc @ [x]) else (i + 1, acc))
            (0, []) history
        in
        h
      else history
    in
    { model with history }
  else model

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
    add_trace (Printf.sprintf "Push %s" entry) model

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
    add_trace (Printf.sprintf "Apply '%s' -> Result: %s" (string_of_function op) res_str) model
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
    if op_str <> "" then add_trace op_str model else model
  with
  | Invalid_argument s -> { model with error_msg = Some s }
  | Pelzl_engine.Stack_error s -> { model with error_msg = Some s }

let exec_edit model op =
  match op with
  | Operations.Backspace ->
      let entry =
        if String.length model.entry > 0 then
          String.sub model.entry 0 (String.length model.entry - 1)
        else ""
      in
      { model with entry }
  | Operations.Enter -> push_entry model
  | Operations.Minus ->
      let entry =
        if String.length model.entry > 0 && model.entry.[0] = '-' then
          String.sub model.entry 1 (String.length model.entry - 1)
        else "-" ^ model.entry
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

(* -------------------------------------------------------------------- *)
(* Repl-mode evaluation: parse + eval through Pelzl_algebraic, then emit *)
(* a static_commit so the input/result line lands in scrollback above   *)
(* the live prompt. Errors are shown transiently in the live region.    *)
(* -------------------------------------------------------------------- *)

let trim_ws s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && (s.[!i] = ' ' || s.[!i] = '\t') do incr i done;
  let j = ref (n - 1) in
  while !j >= !i && (s.[!j] = ' ' || s.[!j] = '\t') do decr j done;
  if !j < !i then "" else String.sub s !i (!j - !i + 1)

let meta_help_text =
  String.concat "\n" [
    "  arithmetic : + - * / % ^                     ";
    "  functions  : sin cos tan asin acos atan      ";
    "             : sinh cosh tanh asinh acosh atanh";
    "             : sqrt sq ln log exp abs ceil floor";
    "             : gamma lngamma erf erfc fact     ";
    "  numbers    : 1.5e3, ffh, 101b, 17o, 42d      ";
    "  variables  : name = expr   (also 'ans')      ";
    "  history    : up/down arrows recall lines     ";
    "  commands   : :vars :purge NAME :help :quit   ";
    "  exit       : :quit, Ctrl-D (empty), or Ctrl-Q";
  ]

let format_vars calc =
  let h = Pelzl_engine.get_variables calc in
  if Hashtbl.length h = 0 then "  (no variables defined)"
  else
    let names = Hashtbl.fold (fun k _ acc -> k :: acc) h [] in
    let names = List.sort compare names in
    String.concat "\n"
      (List.map
         (fun n ->
           let v = Hashtbl.find h n in
           let tmp =
             { calc with Pelzl_engine.stack =
                 Pelzl_engine.stack_push v calc.Pelzl_engine.stack }
           in
           Printf.sprintf "  %s = %s" n
             (Pelzl_engine.get_display_line 1 tmp))
         names)

(* Bind 'ans' to top-of-stack after a successful eval. *)
let bind_ans calc =
  if Pelzl_engine.stack_length calc.Pelzl_engine.stack > 0 then begin
    let v = Pelzl_engine.stack_peek 1 calc.Pelzl_engine.stack in
    Hashtbl.replace (Pelzl_engine.get_variables calc) "ans" v
  end;
  calc

(* Returns one of:
   `Quit                  -- user asked to exit
   `Commit (model, record) -- meta-command produced a record to commit
   `Unknown               -- not a recognised meta-command *)
let handle_meta model raw =
  let s = trim_ws raw in
  let parts =
    String.split_on_char ' ' s
    |> List.filter (fun x -> x <> "")
  in
  match parts with
  | [":quit"] | [":q"] | [":exit"] -> `Quit
  | [":help"] | [":h"] | ["?"] ->
      `Commit (model, Repl_msg meta_help_text)
  | [":vars"] | [":v"] ->
      `Commit (model, Repl_msg (format_vars model.calc))
  | [":purge"; name] ->
      let vars = Pelzl_engine.get_variables model.calc in
      if Hashtbl.mem vars name then begin
        Hashtbl.remove vars name;
        `Commit (model, Repl_msg (Printf.sprintf "  purged %s" name))
      end else
        `Commit (model, Repl_msg (Printf.sprintf "  no such variable: %s" name))
  | _ ->
      `Commit (model, Repl_msg (Printf.sprintf "  unknown command: %s" s))

(* Prepend [s] to history (skip if same as head). Does not persist. *)
let push_history model s =
  match model.history with
  | h :: _ when h = s -> model
  | _ ->
      let history = s :: model.history in
      let history =
        let rec take n = function
          | [] -> []
          | _ when n <= 0 -> []
          | x :: rest -> x :: take (n - 1) rest
        in
        take 1000 history
      in
      { model with history }

(* Submit a Repl-mode entry. Returns (model, should_quit). *)
let submit_repl model raw =
  let s = trim_ws raw in
  if s = "" then
    ({ model with entry = ""; history_idx = None;
                  history_save = ""; error_msg = None;
                  pending_commit = None }, false)
  else
    (* Persist and remember. *)
    let model = push_history model s in
    Pelzl_history.append s;
    if String.length s > 0 && s.[0] = ':' then
      match handle_meta model s with
      | `Quit ->
          ({ model with entry = ""; history_idx = None;
                        history_save = ""; pending_commit = None }, true)
      | `Commit (m, rec_) ->
          ({ m with entry = ""; history_idx = None;
                    history_save = ""; error_msg = None;
                    pending_commit = Some rec_ }, false)
    else
      match Pelzl_algebraic.run model.calc s with
      | Ok (new_calc, display) ->
          let new_calc = bind_ans new_calc in
          let rec_ = Repl_ok { input = s; result = display } in
          ({ model with calc = new_calc; entry = ""; history_idx = None;
                        history_save = ""; error_msg = None;
                        pending_commit = Some rec_ }, false)
      | Error e ->
          let rec_ = Repl_err { input = s; error = Pelzl_algebraic.pp_error e } in
          ({ model with entry = ""; history_idx = None;
                        history_save = ""; error_msg = None;
                        pending_commit = Some rec_ }, false)

(* History navigation. *)
let history_prev model =
  let len = List.length model.history in
  if len = 0 then model
  else
    match model.history_idx with
    | None ->
        { model with
          history_save = model.entry;
          history_idx = Some 0;
          entry = List.nth model.history 0;
          error_msg = None }
    | Some k when k + 1 < len ->
        { model with history_idx = Some (k + 1);
                     entry = List.nth model.history (k + 1);
                     error_msg = None }
    | Some _ -> model

let history_next model =
  match model.history_idx with
  | None -> model
  | Some 0 ->
      { model with entry = model.history_save;
                   history_idx = None; history_save = "";
                   error_msg = None }
  | Some k ->
      { model with history_idx = Some (k - 1);
                   entry = List.nth model.history (k - 1);
                   error_msg = None }

(* -------------------------------------------------------------------- *)
(* Main update                                                          *)
(* -------------------------------------------------------------------- *)

(* Style the committed scrollback record. *)
let style_prompt =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Cyan ~bold:true ()
let style_input =
  Mosaic.Ansi.Style.make ()
let style_result =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Green ()
let style_arrow =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Bright_black ()
let style_err =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Red ~bold:true ()
let style_msg =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Bright_black ()

let render_record (r : repl_record) =
  let row children =
    Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row children
  in
  match r with
  | Repl_ok { input; result } ->
      Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Column [
        row [ Mosaic.text ~style:style_prompt "> ";
              Mosaic.text ~style:style_input input ];
        row [ Mosaic.text ~style:style_arrow "  = ";
              Mosaic.text ~style:style_result result ];
      ]
  | Repl_err { input; error } ->
      Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Column [
        row [ Mosaic.text ~style:style_prompt "> ";
              Mosaic.text ~style:style_input input ];
        row [ Mosaic.text ~style:style_arrow "  ! ";
              Mosaic.text ~style:style_err error ];
      ]
  | Repl_msg s ->
      Mosaic.text ~style:style_msg s

let take_pending model =
  match model.pending_commit with
  | None -> (model, Mosaic.Cmd.none)
  | Some r ->
      let cmd = Mosaic.Cmd.static_commit (render_record r) in
      ({ model with pending_commit = None }, cmd)

let update msg model =
  match msg with
  | Set_entry s ->
      { model with entry = s; history_idx = None;
                   history_save = ""; error_msg = None },
      Mosaic.Cmd.none
  | Submit s ->
      let model', should_quit = submit_repl model s in
      let model', cmd = take_pending model' in
      if should_quit then model', Mosaic.Cmd.quit else model', cmd
  | History_prev ->
      history_prev model, Mosaic.Cmd.none
  | History_next ->
      history_next model, Mosaic.Cmd.none
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
      if model.ui_mode = Repl then
        match data.key with
        | Up   -> history_prev model, Mosaic.Cmd.none
        | Down -> history_next model, Mosaic.Cmd.none
        | Enter ->
            let model', should_quit = submit_repl model model.entry in
            let model', cmd = take_pending model' in
            if should_quit then model', Mosaic.Cmd.quit else model', cmd
        | Backspace ->
            let entry =
              if String.length model.entry > 0 then
                String.sub model.entry 0 (String.length model.entry - 1)
              else ""
            in
            { model with entry; history_idx = None;
                         history_save = ""; error_msg = None },
            Mosaic.Cmd.none
        | Char c when data.modifier.ctrl
                      && Uchar.equal c (Uchar.of_char 'd')
                      && model.entry = "" ->
            model, Mosaic.Cmd.quit
        | Char c when data.modifier.ctrl
                      && Uchar.equal c (Uchar.of_char 'u') ->
            { model with entry = ""; history_idx = None;
                         history_save = ""; error_msg = None },
            Mosaic.Cmd.none
        | Char c when not data.modifier.ctrl && not data.modifier.alt
                      && Uchar.is_char c ->
            let s = model.entry ^ String.make 1 (Uchar.to_char c) in
            { model with entry = s; history_idx = None;
                         history_save = ""; error_msg = None },
            Mosaic.Cmd.none
        | _ -> { model with error_msg = None }, Mosaic.Cmd.none
      else if model.entry <> "" && is_enter then
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
      let entry =
        if String.length model.entry > 0 then
          String.sub model.entry 0 (String.length model.entry - 1)
        else ""
      in
      { model with entry; error_msg = None }, Mosaic.Cmd.none
  | Enter ->
      if model.ui_mode = Repl then
        let model', should_quit = submit_repl model model.entry in
        let model', cmd = take_pending model' in
        if should_quit then model', Mosaic.Cmd.quit else model', cmd
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
