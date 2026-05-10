open Pelzl_model

type editor_runner = string -> unit

type repl_submit_action =
  | Repl_continue
  | Repl_quit
  | Repl_switch of ui_mode

(* -------------------------------------------------------------------- *)
(* Helpers carried over from the legacy RPN/Classic implementation.     *)
(* The algebraic parser previously embedded here has moved to           *)
(* Pelzl_core.Pelzl_algebraic.                                          *)
(* -------------------------------------------------------------------- *)

let parse_with_txtin modes s =
  let normalized =
    let len = String.length s in
    if len > 0 && s.[0] = '(' && not (String.contains s ')') then
      s ^ ")"
    else if len > 0 && s.[0] = '[' && (len = 1 || s.[1] <> '[') then
      let unit_start =
        try Some (String.rindex s '_') with Not_found -> None
      in
      let matrix_part, units =
        match unit_start with
        | Some idx ->
            String.sub s 0 idx, String.sub s idx (len - idx)
        | None -> s, ""
      in
      let rows =
        String.split_on_char '[' (String.sub matrix_part 1 (String.length matrix_part - 1))
        |> List.filter (fun row -> row <> "")
      in
      "["
      ^ String.concat "" (List.map (fun row -> "[" ^ row ^ "]") rows)
      ^ "]" ^ units
    else s
  in
  let lexbuf = Lexing.from_string normalized in
  let values =
    match modes.Pelzl_engine.angle with
    | Pelzl_engine.Rad -> Txtin_parser.decode_data_rad Txtin_lexer.token lexbuf
    | Pelzl_engine.Deg -> Txtin_parser.decode_data_deg Txtin_lexer.token lexbuf
  in
  match values with
  | [value] -> Some value
  | _ -> None

let parse_entry modes s =
  let len = String.length s in
  if len = 0 then Pelzl_engine.RpcVariable ""
  else match (try parse_with_txtin modes s with _ -> None) with
    | Some value -> value
    | None ->
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

let clamp_cursor entry cursor =
  max 0 (min cursor (String.length entry))

let with_entry entry model =
  { model with entry; entry_cursor = String.length entry }

let reset_entry model =
  { model with entry = ""; entry_cursor = 0; entry_mode = Normal }

let exit_classic_mode model =
  { (reset_entry model) with classic_mode = ClassicMain; error_msg = None }

let insert_text s model =
  let cursor = clamp_cursor model.entry model.entry_cursor in
  let before = String.sub model.entry 0 cursor in
  let after = String.sub model.entry cursor (String.length model.entry - cursor) in
  { model with
    entry = before ^ s ^ after;
    entry_cursor = cursor + String.length s;
    history_idx = None;
    history_save = "";
    error_msg = None }

let delete_before_cursor model =
  let cursor = clamp_cursor model.entry model.entry_cursor in
  if cursor = 0 then { model with error_msg = None }
  else
    let before = String.sub model.entry 0 (cursor - 1) in
    let after = String.sub model.entry cursor (String.length model.entry - cursor) in
    { model with
      entry = before ^ after;
      entry_cursor = cursor - 1;
      history_idx = None;
      history_save = "";
      error_msg = None }

let delete_at_cursor model =
  let cursor = clamp_cursor model.entry model.entry_cursor in
  if cursor >= String.length model.entry then { model with error_msg = None }
  else
    let before = String.sub model.entry 0 cursor in
    let after =
      String.sub model.entry (cursor + 1) (String.length model.entry - cursor - 1)
    in
    { model with
      entry = before ^ after;
      entry_cursor = cursor;
      history_idx = None;
      history_save = "";
      error_msg = None }

let move_cursor delta model =
  { model with
    entry_cursor = clamp_cursor model.entry (model.entry_cursor + delta);
    error_msg = None }

let cursor_home model = { model with entry_cursor = 0; error_msg = None }

let cursor_end model =
  { model with entry_cursor = String.length model.entry; error_msg = None }

let starts_with ~prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let resolve_prefixed symbols translate prefix =
  let exact = List.find_opt (fun s -> s = prefix) symbols in
  let symbol =
    match exact with
    | Some s -> Some s
    | None -> List.find_opt (starts_with ~prefix) symbols
  in
  match symbol with
  | Some s -> Some (s, translate s)
  | None -> None

let default_editor_runner path =
  ignore (Sys.command (!(Rcfile.editor) ^ " " ^ Filename.quote path))

let data_path basename =
  Utility.expand_file (Utility.join_path !(Rcfile.datadir) basename)

let rec ensure_dir dir =
  if dir = "" || dir = "." || Sys.file_exists dir then ()
  else begin
    let parent = Filename.dirname dir in
    if parent <> dir then ensure_dir parent;
    try Unix.mkdir dir 0o755 with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let write_text_file path text =
  ensure_dir (Filename.dirname path);
  let ch = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr ch)
    (fun () -> output_string ch text)

let ensure_text_file path =
  ensure_dir (Filename.dirname path);
  if not (Sys.file_exists path) then write_text_file path ""

let read_text_file path =
  let ch = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ch)
    (fun () ->
      let len = in_channel_length ch in
      really_input_string ch len)

let parse_input_text calc text =
  let lexbuf = Lexing.from_string text in
  match calc.Pelzl_engine.modes.Pelzl_engine.angle with
  | Pelzl_engine.Rad -> Txtin_parser.decode_data_rad Txtin_lexer.token lexbuf
  | Pelzl_engine.Deg -> Txtin_parser.decode_data_deg Txtin_lexer.token lexbuf

let editor_error_message = function
  | Parsing.Parse_error | Failure _ -> "syntax error in input"
  | Utility.Txtin_error s
  | Big_int_str.Big_int_string_failure s
  | Units.Units_error s
  | Sys_error s
  | Invalid_argument s
  | Pelzl_engine.Stack_error s -> s
  | exn -> Printexc.to_string exn

let with_editor_error f model =
  try f model with exn -> { model with error_msg = Some (editor_error_message exn) }

let push_editor_values values calc =
  match values with
  | [] -> calc
  | _ ->
      let calc = Pelzl_engine.state_backup calc in
      List.fold_left
        (fun calc value ->
          { calc with
            Pelzl_engine.stack =
              Pelzl_engine.stack_push value calc.Pelzl_engine.stack })
        calc values

let replace_browse_values selected values calc =
  match values with
  | [] -> calc
  | _ ->
      let calc = Pelzl_engine.state_backup calc in
      List.fold_left
        (fun calc value ->
          let stack =
            calc.Pelzl_engine.stack
            |> Pelzl_engine.stack_delete selected
            |> Pelzl_engine.stack_push value
            |> Pelzl_engine.stack_rolldown selected
          in
          { calc with Pelzl_engine.stack })
        calc values

let toggle_minus model =
  let cursor = clamp_cursor model.entry model.entry_cursor in
  let anchor = ref 0 in
  for i = 0 to cursor - 1 do
    match model.entry.[i] with
    | '(' | '[' | ',' | '<' -> anchor := i + 1
    | 'e' | 'E' when i + 1 <= cursor -> anchor := i + 1
    | _ -> ()
  done;
  if !anchor < String.length model.entry && model.entry.[!anchor] = '-' then
    { model with
      entry =
        String.sub model.entry 0 !anchor
        ^ String.sub model.entry (!anchor + 1)
            (String.length model.entry - !anchor - 1);
      entry_cursor = max !anchor (cursor - 1);
      error_msg = None }
  else
    let before = String.sub model.entry 0 !anchor in
    let after = String.sub model.entry !anchor (String.length model.entry - !anchor) in
    { model with
      entry = before ^ "-" ^ after;
      entry_cursor = cursor + 1;
      error_msg = None }

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
    let value = parse_entry model.calc.Pelzl_engine.modes entry in
    let new_calc = { model.calc with stack = Pelzl_engine.stack_push value model.calc.stack } in
    let model = { (reset_entry model) with calc = new_calc } in
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

let push_constant symbol unit_def model =
  let calc = Pelzl_engine.state_backup model.calc in
  let value =
    Pelzl_engine.RpcFloatUnit
      (unit_def.Units.coeff, unit_def.Units.comp_units)
  in
  let calc =
    { calc with stack = Pelzl_engine.stack_push value calc.Pelzl_engine.stack }
  in
  add_trace (Printf.sprintf "Const %s" symbol)
    { (exit_classic_mode model) with calc }

let begin_modal modal model =
  if model.entry <> "" then
    { model with error_msg = Some "finish entry before changing modes" }
  else
    { (reset_entry model) with classic_mode = modal; error_msg = None }

let clamp_browse_level calc selected =
  let len = Pelzl_engine.stack_length calc.Pelzl_engine.stack in
  if len = 0 then 0 else max 1 (min selected len)

let set_browse_mode model selected hscroll =
  let selected = clamp_browse_level model.calc selected in
  if selected = 0 then exit_classic_mode model
  else
    { model with
      classic_mode =
        ClassicBrowse { selected_level = selected; hscroll = max 0 hscroll };
      error_msg = None }

let run_view_editor editor_runner level model =
  with_editor_error
    (fun model ->
      let path = data_path "fullscreen" in
      write_text_file path (Pelzl_engine.get_fullscreen_display level model.calc);
      editor_runner path;
      { model with error_msg = None })
    model

let run_edit_input_editor editor_runner model =
  with_editor_error
    (fun model ->
      let path = data_path "input" in
      ensure_text_file path;
      editor_runner path;
      let values = parse_input_text model.calc (read_text_file path) in
      let calc = push_editor_values values model.calc in
      add_trace "Edit input" { model with calc; error_msg = None })
    model

let run_browse_edit_editor editor_runner selected hscroll model =
  with_editor_error
    (fun model ->
      let path = data_path "input" in
      write_text_file path (Pelzl_engine.get_fullscreen_display selected model.calc);
      editor_runner path;
      let values = parse_input_text model.calc (read_text_file path) in
      let calc = replace_browse_values selected values model.calc in
      add_trace "Edit stack entry"
        (set_browse_mode { model with calc; error_msg = None } selected hscroll))
    model

let mutate_browse_stack model selected hscroll f next_selected =
  try
    let calc = Pelzl_engine.state_backup model.calc in
    let calc = { calc with stack = f calc.Pelzl_engine.stack } in
    set_browse_mode { model with calc } next_selected hscroll
  with
  | Invalid_argument s -> { model with error_msg = Some s }
  | Pelzl_engine.Stack_error s -> { model with error_msg = Some s }

let exec_browse editor_runner model selected hscroll op =
  match op with
  | Operations.EndBrowse ->
      exit_classic_mode model
  | Operations.ScrollLeft ->
      set_browse_mode model selected (hscroll - 1)
  | Operations.ScrollRight ->
      set_browse_mode model selected (hscroll + 1)
  | Operations.PrevLine ->
      set_browse_mode model (selected + 1) hscroll
  | Operations.NextLine ->
      set_browse_mode model (selected - 1) hscroll
  | Operations.Echo ->
      mutate_browse_stack model selected hscroll
        (Pelzl_engine.stack_echo selected) 1
  | Operations.Drop1 ->
      mutate_browse_stack model selected hscroll
        (Pelzl_engine.stack_delete selected) selected
  | Operations.DropN ->
      mutate_browse_stack model selected hscroll
        (Pelzl_engine.stack_deleteN selected) 1
  | Operations.Keep ->
      mutate_browse_stack model selected hscroll
        (Pelzl_engine.stack_keep selected) 1
  | Operations.KeepN ->
      mutate_browse_stack model selected hscroll
        (Pelzl_engine.stack_keepN selected) selected
  | Operations.RollDown ->
      mutate_browse_stack model selected hscroll
        (Pelzl_engine.stack_rolldown selected) selected
  | Operations.RollUp ->
      mutate_browse_stack model selected hscroll
        (Pelzl_engine.stack_rollup selected) selected
  | Operations.ViewEntry ->
      run_view_editor editor_runner selected model
  | Operations.EditEntry ->
      run_browse_edit_editor editor_runner selected hscroll model

let exec_edit model op =
  match op with
  | Operations.Backspace ->
      delete_before_cursor model
  | Operations.Enter -> push_entry model
  | Operations.Minus ->
      toggle_minus model
  | Operations.SciNotBase ->
      insert_text (if model.entry_mode = Integer then "`" else "e") model
  | Operations.BeginInteger ->
      { (insert_text "#" model) with entry_mode = Integer }
  | Operations.BeginComplex ->
      { (insert_text "(" model) with entry_mode = Complex }
  | Operations.BeginMatrix ->
      { (insert_text "[" model) with entry_mode = Matrix }
  | Operations.Separator -> insert_text "," model
  | Operations.Angle -> insert_text "<" model
  | Operations.BeginUnits -> insert_text "_" model
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
    "  constants  : pi tau e i                     ";
    "  variables  : name = expr   (also 'ans')      ";
    "  history    : up/down arrows recall lines     ";
    "  commands   : :vars :purge NAME :help         ";
    "  modes      : [Alt-R] RPN                     ";
    "  quit       : [Ctrl-Q/D] Quit                 ";
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
   `Switch mode           -- user asked to switch UI runtimes *)
let handle_meta model raw =
  let s = trim_ws raw in
  let parts =
    String.split_on_char ' ' s
    |> List.filter (fun x -> x <> "")
  in
  match parts with
  | [":quit"] | [":q"] | [":exit"] -> `Quit
  | [":rpn"] -> `Switch Classic
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

(* Submit a Repl-mode entry. Returns (model, action). *)
let submit_repl_action model raw =
  let s = trim_ws raw in
  if s = "" then
    ({ model with entry = ""; entry_cursor = 0; history_idx = None;
                  history_save = ""; error_msg = None;
                  pending_commit = None }, Repl_continue)
  else
    (* Persist and remember. *)
    let model = push_history model s in
    Pelzl_history.append s;
    if String.length s > 0 && s.[0] = ':' then
      match handle_meta model s with
      | `Quit ->
          ({ model with entry = ""; entry_cursor = 0; history_idx = None;
                        history_save = ""; pending_commit = None }, Repl_quit)
      | `Switch mode ->
          normalize_for_mode mode model, Repl_switch mode
      | `Commit (m, rec_) ->
          ({ m with entry = ""; entry_cursor = 0; history_idx = None;
                    history_save = ""; error_msg = None;
                    pending_commit = Some rec_ }, Repl_continue)
    else
      match Pelzl_algebraic.run model.calc s with
      | Ok (new_calc, display) ->
          let new_calc = bind_ans new_calc in
          let rec_ = Repl_ok { input = s; result = display } in
          ({ model with calc = new_calc; entry = ""; entry_cursor = 0; history_idx = None;
                        history_save = ""; error_msg = None;
                        pending_commit = Some rec_ }, Repl_continue)
      | Error e ->
          let rec_ = Repl_err { input = s; error = Pelzl_algebraic.pp_error e } in
          ({ model with entry = ""; entry_cursor = 0; history_idx = None;
                        history_save = ""; error_msg = None;
                        pending_commit = Some rec_ }, Repl_continue)

let submit_repl model raw =
  let model, action = submit_repl_action model raw in
  model, action <> Repl_continue

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
          entry_cursor = String.length (List.nth model.history 0);
          error_msg = None }
    | Some k when k + 1 < len ->
        let entry = List.nth model.history (k + 1) in
        { model with history_idx = Some (k + 1);
                     entry;
                     entry_cursor = String.length entry;
                     error_msg = None }
    | Some _ -> model

let history_next model =
  match model.history_idx with
  | None -> model
  | Some 0 ->
      { model with entry = model.history_save;
                   entry_cursor = String.length model.history_save;
                   history_idx = None; history_save = "";
                   error_msg = None }
  | Some k ->
      let entry = List.nth model.history (k - 1) in
      { model with history_idx = Some (k - 1);
                   entry;
                   entry_cursor = String.length entry;
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

let repl_msg_lines s =
  match String.split_on_char '\n' s with
  | [] -> [""]
  | lines -> lines

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
      Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Column
        (List.map
           (fun line ->
             if line = "" then Mosaic.text " "
             else Mosaic.text ~style:style_msg line)
           (repl_msg_lines s))

let take_pending model =
  match model.pending_commit with
  | None -> (model, Mosaic.Cmd.none)
  | Some r ->
      let cmd = Mosaic.Cmd.static_commit (render_record r) in
      ({ model with pending_commit = None }, cmd)

let quit_cmd = Mosaic.Cmd.quit

let default_mode_switch _mode _model = ()

let request_mode_switch on_mode_switch mode model =
  let model = normalize_for_mode mode model in
  on_mode_switch mode model;
  model, quit_cmd

let request_mode_toggle on_mode_switch model =
  match model.ui_mode with
  | Repl -> request_mode_switch on_mode_switch Classic model
  | Classic -> request_mode_switch on_mode_switch Repl model

let execute_classic_operation on_mode_switch editor_runner model op =
  match op with
  | Operations.Function f ->
      exec_function model f, Mosaic.Cmd.none
  | Operations.Command c ->
      (match c with
      | Operations.Quit ->
          model, quit_cmd
      | Operations.CycleHelp ->
          { model with show_help = not model.show_help }, Mosaic.Cmd.none
      | Operations.BeginAbbrev ->
          begin_modal (ClassicAbbrev OperationAbbrev) model, Mosaic.Cmd.none
      | Operations.BeginConst ->
          begin_modal (ClassicAbbrev ConstantAbbrev) model, Mosaic.Cmd.none
      | Operations.BeginVar ->
          begin_modal (ClassicVariable { completion_prefix = None }) model,
          Mosaic.Cmd.none
      | Operations.BeginBrowse ->
          if Pelzl_engine.stack_length model.calc.Pelzl_engine.stack = 0 then
            { model with error_msg = Some "empty stack" }, Mosaic.Cmd.none
          else
            set_browse_mode (reset_entry model) 1 0, Mosaic.Cmd.none
      | Operations.View ->
          run_view_editor editor_runner 1 model, Mosaic.Cmd.none
      | Operations.EditInput ->
          run_edit_input_editor editor_runner model, Mosaic.Cmd.none
      | _ ->
          exec_command model c, Mosaic.Cmd.none)
  | Operations.Edit e ->
      exec_edit model e, Mosaic.Cmd.none
  | Operations.Browse _
  | Operations.Abbrev _
  | Operations.IntEdit _
  | Operations.VarEdit _ ->
      model, Mosaic.Cmd.none

let is_ctrl_char (data : Input.Key.event) ch =
  match data.key with
  | Char c ->
      (data.modifier.ctrl
       && Uchar.is_char c
       && Char.lowercase_ascii (Uchar.to_char c) = ch)
      || Uchar.to_int c = Char.code ch - Char.code 'a' + 1
  | _ -> false

let is_text_char c =
  Uchar.is_char c
  && let code = Uchar.to_int c in
     code >= 0x20 && code <> 0x7f

let is_backspace_key (data : Input.Key.event) =
  match data.key with
  | Backspace -> true
  | Char c -> Uchar.to_int c = 0x7f
  | _ -> false

let is_alt_r_key (data : Input.Key.event) =
  data.modifier.alt
  &&
  match data.key with
  | Char c when Uchar.is_char c ->
      Char.lowercase_ascii (Uchar.to_char c) = 'r'
  | _ -> false

let execute_abbrev on_mode_switch editor_runner model =
  if model.entry = "" then
    { model with error_msg = Some "empty abbreviation" }, Mosaic.Cmd.none
  else
  match resolve_prefixed !Rcfile.abbrev_commands Rcfile.translate_abbrev model.entry with
  | None ->
      { model with error_msg = Some ("unknown abbreviation: " ^ model.entry) },
      Mosaic.Cmd.none
  | Some (_symbol, op) ->
      execute_classic_operation on_mode_switch editor_runner
        (exit_classic_mode model) op

let execute_constant model =
  if model.entry = "" then
    { model with error_msg = Some "empty constant" }, Mosaic.Cmd.none
  else
  match resolve_prefixed !Rcfile.constant_symbols Rcfile.translate_constant model.entry with
  | None ->
      { model with error_msg = Some ("unknown constant: " ^ model.entry) },
      Mosaic.Cmd.none
  | Some (symbol, unit_def) ->
      push_constant symbol unit_def model, Mosaic.Cmd.none

let enter_variable model =
  if model.entry = "" then
    { model with error_msg = Some "empty variable name" }, Mosaic.Cmd.none
  else
    let calc = Pelzl_engine.state_backup model.calc in
    let value = Pelzl_engine.RpcVariable model.entry in
    let calc =
      { calc with stack = Pelzl_engine.stack_push value calc.Pelzl_engine.stack }
    in
    add_trace (Printf.sprintf "Variable %s" model.entry)
      { (exit_classic_mode model) with calc },
    Mosaic.Cmd.none

let reset_variable_completion model =
  { model with classic_mode = ClassicVariable { completion_prefix = None } }

let complete_variable completion_prefix model =
  let prefix = Option.value completion_prefix ~default:model.entry in
  let vars = Pelzl_engine.get_variables model.calc in
  let matches =
    Hashtbl.fold
      (fun name _ acc ->
        if starts_with ~prefix name then name :: acc else acc)
      vars []
    |> List.sort String.compare
  in
  match matches with
  | [] -> { model with error_msg = Some "no matching variable" }
  | _ ->
      let next =
        match List.find_index (fun name -> name = model.entry) matches with
        | Some i -> List.nth matches ((i + 1) mod List.length matches)
        | None -> List.hd matches
      in
      { (with_entry next { model with error_msg = None })
        with classic_mode = ClassicVariable { completion_prefix = Some prefix } }

let handle_classic_abbrev on_mode_switch editor_runner kind model
    (data : Input.Key.event) =
  if data.key = Enter then
    match kind with
    | OperationAbbrev -> execute_abbrev on_mode_switch editor_runner model
    | ConstantAbbrev -> execute_constant model
  else if is_backspace_key data then
    delete_before_cursor model, Mosaic.Cmd.none
  else
    match data.key with
    | Char c when Uchar.equal c (Uchar.of_char '\'') ->
        exit_classic_mode model, Mosaic.Cmd.none
    | Char c when not data.modifier.ctrl && not data.modifier.alt && is_text_char c ->
        insert_text (String.make 1 (Uchar.to_char c)) model, Mosaic.Cmd.none
    | _ -> { model with error_msg = None }, Mosaic.Cmd.none

let handle_classic_variable completion_prefix model (data : Input.Key.event) =
  if data.key = Enter then enter_variable model
  else if data.key = Tab then complete_variable completion_prefix model, Mosaic.Cmd.none
  else if is_backspace_key data then
    reset_variable_completion (delete_before_cursor model), Mosaic.Cmd.none
  else
    match data.key with
    | Char c when Uchar.equal c (Uchar.of_char '@') ->
        exit_classic_mode model, Mosaic.Cmd.none
    | Char c when not data.modifier.ctrl && not data.modifier.alt && is_text_char c ->
        reset_variable_completion (insert_text (String.make 1 (Uchar.to_char c)) model),
        Mosaic.Cmd.none
    | _ -> { model with error_msg = None }, Mosaic.Cmd.none

let handle_classic_browse editor_runner model key_binding selected_level hscroll =
  try
    let op = Rcfile.browse_of_key key_binding in
    exec_browse editor_runner model selected_level hscroll op, Mosaic.Cmd.none
  with Not_found ->
    { model with error_msg = None }, Mosaic.Cmd.none

let handle_repl_submit on_mode_switch model raw =
  let model, action = submit_repl_action model raw in
  let model, cmd = take_pending model in
  match action with
  | Repl_continue -> model, cmd
  | Repl_quit -> model, quit_cmd
  | Repl_switch mode ->
      on_mode_switch mode model;
      model, quit_cmd

let update ?(editor_runner = default_editor_runner)
    ?(on_mode_switch = default_mode_switch) msg model =
  match msg with
  | Set_entry s ->
      { (with_entry s model) with history_idx = None;
                             history_save = ""; error_msg = None },
      Mosaic.Cmd.none
  | Submit s ->
      handle_repl_submit on_mode_switch model s
  | History_prev ->
      history_prev model, Mosaic.Cmd.none
  | History_next ->
      history_next model, Mosaic.Cmd.none
  | Key_input ev ->
      let data = Mosaic.Event.Key.data ev in
      let key_binding =
        let open Pelzl_engine in
        let k, ctrl = match data.key with
        | Char c when data.modifier.ctrl && Uchar.is_char c ->
            let ch = Char.uppercase_ascii (Uchar.to_char c) in
            let code = Char.code ch - Char.code '@' in
            if code >= 0 && code < 0x20 then Key_char (Uchar.of_int code), true
            else Key_char c, true
        | Char c when Uchar.to_int c > 0 && Uchar.to_int c < 0x20 ->
            Key_char c, true
        | Char c when Uchar.equal c (Uchar.of_char ' ') ->
            Key_space, data.modifier.ctrl
        | Char c -> Key_char c, data.modifier.ctrl
        | Enter -> Key_enter, data.modifier.ctrl
        | Tab -> Key_tab, data.modifier.ctrl
        | Backspace -> Key_backspace, data.modifier.ctrl
        | Delete -> Key_delete, data.modifier.ctrl
        | Escape -> Key_escape, data.modifier.ctrl
        | Up -> Key_up, data.modifier.ctrl
        | Down -> Key_down, data.modifier.ctrl
        | Left -> Key_left, data.modifier.ctrl
        | Right -> Key_right, data.modifier.ctrl
        | Home -> Key_home, data.modifier.ctrl
        | End -> Key_end, data.modifier.ctrl
        | Page_up -> Key_page_up, data.modifier.ctrl
        | Page_down -> Key_page_down, data.modifier.ctrl
        | Insert -> Key_insert, data.modifier.ctrl
        | F n -> Key_f n, data.modifier.ctrl
        | _ -> Key_unknown 0, data.modifier.ctrl
        in
        { key = k; ctrl; meta = data.modifier.alt }
      in
      let is_enter = (data.key = Enter) in
      if is_alt_r_key data then
        request_mode_toggle on_mode_switch model
      else if is_ctrl_char data 'q'
         || (is_ctrl_char data 'd'
             && (model.ui_mode = Classic || model.entry = ""))
      then model, quit_cmd
      else if model.ui_mode = Repl then
        match data.key with
        | Up   -> history_prev model, Mosaic.Cmd.none
        | Down -> history_next model, Mosaic.Cmd.none
        | Left -> move_cursor (-1) model, Mosaic.Cmd.none
        | Right -> move_cursor 1 model, Mosaic.Cmd.none
        | Home -> cursor_home model, Mosaic.Cmd.none
        | End -> cursor_end model, Mosaic.Cmd.none
        | Delete -> delete_at_cursor model, Mosaic.Cmd.none
        | Enter ->
            handle_repl_submit on_mode_switch model model.entry
        | Backspace ->
            delete_before_cursor model, Mosaic.Cmd.none
        | Char c when data.modifier.ctrl
                      && Uchar.equal c (Uchar.of_char 'u') ->
            { (reset_entry model) with history_idx = None;
                                       history_save = ""; error_msg = None },
            Mosaic.Cmd.none
        | Char c when not data.modifier.ctrl && not data.modifier.alt
                      && is_text_char c ->
            insert_text (String.make 1 (Uchar.to_char c)) model, Mosaic.Cmd.none
        | _ -> { model with error_msg = None }, Mosaic.Cmd.none
      else
      if data.key = Escape then
        match model.classic_mode with
        | ClassicMain ->
            if model.entry = "" then model, Mosaic.Cmd.none
            else reset_entry model, Mosaic.Cmd.none
        | ClassicAbbrev _
        | ClassicVariable _
        | ClassicBrowse _ ->
            exit_classic_mode model, Mosaic.Cmd.none
      else
        (match model.classic_mode with
        | ClassicAbbrev kind ->
            handle_classic_abbrev on_mode_switch editor_runner kind model data
        | ClassicVariable { completion_prefix } ->
            handle_classic_variable completion_prefix model data
        | ClassicBrowse { selected_level; hscroll } ->
            handle_classic_browse editor_runner model key_binding selected_level hscroll
        | ClassicMain ->
      if model.entry <> "" && is_enter then
        push_entry model, Mosaic.Cmd.none
      else
        let op_opt =
          let edit_opt =
            try Some (Rcfile.edit_of_key key_binding) with Not_found -> None
          in
          match model.entry, edit_opt with
          | "", _ | _, None ->
              (try Some (Operations.Function (Rcfile.function_of_key key_binding)) with Not_found ->
               try Some (Operations.Command (Rcfile.command_of_key key_binding)) with Not_found ->
               match edit_opt with
               | Some e -> Some (Operations.Edit e)
               | None -> None)
          | _, Some Operations.Minus ->
              Some (Operations.Edit Operations.Minus)
          | _, Some e ->
              (try Some (Operations.Function (Rcfile.function_of_key key_binding)) with Not_found ->
               try Some (Operations.Command (Rcfile.command_of_key key_binding)) with Not_found ->
               Some (Operations.Edit e))
        in
        (match op_opt with
        | Some op -> execute_classic_operation on_mode_switch editor_runner model op
        | None ->
            (match data.key with
            | Char c when not data.modifier.ctrl && not data.modifier.alt
                          && is_text_char c ->
                insert_text (String.make 1 (Uchar.to_char c)) model, Mosaic.Cmd.none
            | _ -> { model with error_msg = None }, Mosaic.Cmd.none)))
  | Backspace ->
      delete_before_cursor model, Mosaic.Cmd.none
  | Enter ->
      if model.ui_mode = Repl then
        handle_repl_submit on_mode_switch model model.entry
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
      model, quit_cmd
