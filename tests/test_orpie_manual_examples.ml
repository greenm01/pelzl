open Alcotest
open Pelzl_engine
open Pelzl_model

let eps = 1e-9

let rc_loaded = ref false

let ensure_orpie_defaults () =
  if not !rc_loaded then begin
    ignore (Pelzl_model.init Pelzl_model.Classic ());
    let candidates =
      [
        "etc/pelzlrc";
        "../etc/pelzlrc";
        "../../etc/pelzlrc";
        "../../../etc/pelzlrc";
      ]
    in
    let rcfile =
      match List.find_opt Sys.file_exists candidates with
      | Some path -> path
      | None -> fail "could not locate etc/pelzlrc for manual example tests"
    in
    Units.unit_table := Units.empty_unit_table;
    Rcfile.process_rcfile (Some rcfile);
    rc_loaded := true
  end

let parse_deg s =
  ensure_orpie_defaults ();
  Txtin_parser.decode_data_deg Txtin_lexer.token (Lexing.from_string s)

let parse_rad s =
  ensure_orpie_defaults ();
  Txtin_parser.decode_data_rad Txtin_lexer.token (Lexing.from_string s)

let one_deg s =
  match parse_deg s with
  | [x] -> x
  | xs -> failf "expected one parsed value, got %d" (List.length xs)

let render_data ?(modes = empty_state.modes) data =
  let st =
    { empty_state with
      modes;
      stack = stack_push data empty_state.stack }
  in
  String.trim (get_display_line 1 st)

let check_render label expected data =
  check string label expected (render_data data)

let check_float label expected = function
  | RpcFloatUnit (f, _) -> check (float eps) label expected f
  | _ -> fail (label ^ ": expected real")

let check_int label expected = function
  | RpcInt i -> check int label expected (Big_int.int_of_big_int i)
  | _ -> fail (label ^ ": expected integer")

let check_complex label expected_re expected_im = function
  | RpcComplexUnit (c, _) ->
      check (float eps) (label ^ " real") expected_re c.Complex.re;
      check (float eps) (label ^ " imag") expected_im c.Complex.im
  | _ -> fail (label ^ ": expected complex")

let contains_substring haystack needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with Not_found -> false

let check_fmat label expected = function
  | RpcFloatMatrixUnit (m, _) ->
      let rows, cols = Gsl.Matrix.dims m in
      check int (label ^ " rows") (Array.length expected) rows;
      check int (label ^ " cols") (Array.length expected.(0)) cols;
      Array.iteri
        (fun r row ->
          Array.iteri
            (fun c expected_value ->
              check (float eps)
                (Printf.sprintf "%s[%d,%d]" label r c)
                expected_value (Gsl.Matrix.get m r c))
            row)
        expected
  | _ -> fail (label ^ ": expected real matrix")

let check_cmat label expected = function
  | RpcComplexMatrixUnit (m, _) ->
      let rows, cols = Gsl.Matrix_complex.dims m in
      check int (label ^ " rows") (Array.length expected) rows;
      check int (label ^ " cols") (Array.length expected.(0)) cols;
      Array.iteri
        (fun r row ->
          Array.iteri
            (fun c expected_value ->
              let got = Gsl.Matrix_complex.get m r c in
              check (float eps)
                (Printf.sprintf "%s[%d,%d].re" label r c)
                expected_value.Complex.re got.Complex.re;
              check (float eps)
                (Printf.sprintf "%s[%d,%d].im" label r c)
                expected_value.Complex.im got.Complex.im)
            row)
        expected
  | _ -> fail (label ^ ": expected complex matrix")

let c re im = { Complex.re; im }

let key ?(modifier = Input.Key.no_modifier) k =
  Mosaic.Event.Key.of_input (Input.Key.make ~modifier k)

let plain_char ch =
  key (Input.Key.Char (Uchar.of_char ch))

let classic_key ch model =
  fst (Pelzl_update.update (Pelzl_model.Key_input (plain_char ch)) model)

let classic_enter model =
  fst (Pelzl_update.update (Pelzl_model.Key_input (key Input.Key.Enter)) model)

let classic_type s model =
  String.fold_left (fun model ch -> classic_key ch model) model s

let top_int model =
  match stack_peek 1 model.Pelzl_model.calc.stack with
  | RpcInt i -> Big_int.int_of_big_int i
  | RpcFloatUnit (f, _) -> int_of_float f
  | _ -> fail "expected numeric top of stack"

let top_float model =
  match stack_peek 1 model.Pelzl_model.calc.stack with
  | RpcFloatUnit (f, _) -> f
  | RpcInt i -> Big_int.float_of_big_int i
  | _ -> fail "expected numeric top of stack"

let stack_int level model =
  match stack_peek level model.Pelzl_model.calc.stack with
  | RpcInt i -> Big_int.int_of_big_int i
  | RpcFloatUnit (f, _) -> int_of_float f
  | _ -> failf "expected numeric stack level %d" level

let model_with_ints ints =
  let model, _ = Pelzl_model.init Pelzl_model.Classic () in
  let stack =
    List.fold_left
      (fun stack n -> stack_push (RpcInt (Big_int.big_int_of_int n)) stack)
      model.calc.stack ints
  in
  { model with calc = { model.calc with stack } }

let read_file path =
  let ch = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ch)
    (fun () -> really_input_string ch (in_channel_length ch))

let write_file path text =
  let ch = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr ch)
    (fun () -> output_string ch text)

let with_temp_datadir f =
  let old_datadir = !(Rcfile.datadir) in
  let dir = Filename.temp_file "pelzl-editor-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Rcfile.datadir := dir;
  Fun.protect
    ~finally:(fun () ->
      Rcfile.datadir := old_datadir;
      List.iter
        (fun basename ->
          let path = Filename.concat dir basename in
          if Sys.file_exists path then Sys.remove path)
        [ "input"; "fullscreen" ];
      Unix.rmdir dir)
    (fun () -> f dir)

let update_with_editor editor_runner input_key model =
  fst (Pelzl_update.update ~editor_runner (Key_input input_key) model)

let classic_key_with_editor editor_runner ch model =
  update_with_editor editor_runner (plain_char ch) model

let test_overview_addition_example () =
  let model, _ = Pelzl_model.init Pelzl_model.Classic () in
  let model = { model with entry = "1"; entry_cursor = 1 } |> classic_enter in
  let model = { model with entry = "2"; entry_cursor = 1 } |> classic_enter in
  let model = classic_key '+' model in
  check int "1<enter>2<enter>+" 3 (top_int model)

let test_real_number_entry_examples () =
  check_float "1.23<enter>" 1.23 (one_deg "1.23");
  check_float "1.23<space>23n<enter>" 1.23e-23 (one_deg "1.23e-23");
  check_float "1.23n<space>23<enter>" (-1.23e23) (one_deg "-1.23e23")

let test_complex_entry_examples () =
  check_complex "(1.23, 4.56<enter>" 1.23 4.56 (one_deg "(1.23, 4.56)");
  check_complex "(0.7072<45<enter>" 0.500065915655126 0.500065915655126
    (one_deg "(0.7072 <45)");
  check_complex "(1.23n,4.56<space>10<enter>" (-1.23) 45_600_000_000.
    (one_deg "(-1.23,4.56e10)")

let test_matrix_entry_examples () =
  check_fmat "[1,2[3,4<enter>"
    [| [| 1.; 2. |]; [| 3.; 4. |] |]
    (one_deg "[[1,2][3,4]]");
  check_fmat "[1.2<space>10,0[3n,5n<enter>"
    [| [| 12_000_000_000.; 0. |]; [| -3.; -5. |] |]
    (one_deg "[[1.2e10,0][-3,-5]]");
  check_cmat "[(1,2,3,4[5,6,7,8<enter>"
    [| [| c 1. 2.; c 3. 4. |]; [| c 5. 6.; c 7. 8. |] |]
    (one_deg "[[(1,2),(3,4)][(5,6),(7,8)]]")

let test_classic_bracket_key_matrix_entry_example () =
  let model, _ = Pelzl_model.init Pelzl_model.Classic () in
  let model = model |> classic_type "[1,2[3,4" |> classic_enter in
  match stack_peek 1 model.Pelzl_model.calc.stack with
  | RpcFloatMatrixUnit (m, _) ->
      let rows, cols = Gsl.Matrix.dims m in
      check int "rows" 2 rows;
      check int "cols" 2 cols;
      check (float eps) "matrix[0,0]" 1. (Gsl.Matrix.get m 0 0);
      check (float eps) "matrix[1,1]" 4. (Gsl.Matrix.get m 1 1)
  | _ -> fail "[ key entry should push a real matrix"

let test_unit_entry_examples () =
  check_render "1.234_N*mm^2/s<enter>" "1.234_N*mm^2*s^-1"
    (one_deg "1.234_N*mm^2/s");
  check_render "(2.3,5_s^-4<enter>" "(2.3, 5)_s^-4"
    (one_deg "(2.3,5)_s^-4");
  let matrix_units = render_data (one_deg "[[1,2][3,4]]_lbf*in") in
  check bool "[1,2[3,4_lbf*in<enter> matrix"
    true (String.starts_with ~prefix:"[[ 1, 2 ][ 3, 4 ]]_" matrix_units);
  check bool "[1,2[3,4_lbf*in<enter> lbf"
    true (contains_substring matrix_units "lbf");
  check bool "[1,2[3,4_lbf*in<enter> in"
    true (contains_substring matrix_units "in");
  check_render "_nm<enter>" "1_nm" (one_deg "1_nm")

let test_exact_integer_entry_examples () =
  check_int "#123456<enter>" 123456 (one_deg "#123456`d");
  check_int "#ffff<space>h<enter>" 65535 (one_deg "#ffff`h");
  check_int "#10101n<space>b<enter>" (-21) (one_deg "#-10101`b")

let test_variable_entry_example () =
  match one_deg "@myvar" with
  | RpcVariable "myvar" -> ()
  | RpcVariable s -> failf "unexpected variable %S" s
  | _ -> fail "expected variable"

let test_external_editor_sample_inputs () =
  check_int "exact integer sample" 12345678 (one_deg "#12345678`d");
  check_float "real number sample" (-123.45e67) (one_deg "-123.45e67");
  check_complex "complex rect sample" 1e10 2. (one_deg "(1e10, 2)");
  check_complex "complex polar sample" 0. 1. (one_deg "(1 <90)");
  check_fmat "real matrix sample"
    [| [| 1.; 2. |]; [| 3.1; 4.5e10 |] |]
    (one_deg "[[1, 2][3.1, 4.5e10]]");
  check_cmat "complex matrix sample"
    [| [| c 1. 0.; c 5. 0. |]; [| c 1e10 0.; c 0. 2. |] |]
    (one_deg "[[(1, 0), 5][1e10, (2 <90)]]");
  match one_deg "@myvar" with
  | RpcVariable "myvar" -> ()
  | _ -> fail "expected variable sample"

let test_external_editor_multiple_entries_example () =
  match parse_deg "(1, 2) 1.5" with
  | [RpcComplexUnit (z, _); RpcFloatUnit (f, _)] ->
      check (float eps) "complex real" 1. z.Complex.re;
      check (float eps) "complex imag" 2. z.Complex.im;
      check (float eps) "real" 1.5 f
  | _ -> fail "expected complex value followed by real value"

let test_function_shortcut_examples () =
  let run entries_and_keys =
    let model, _ = Pelzl_model.init Pelzl_model.Classic () in
    List.fold_left
      (fun model -> function
        | `Entry s -> { model with entry = s; entry_cursor = String.length s }
        | `Enter -> classic_enter model
        | `Key ch -> classic_key ch model)
      model entries_and_keys
  in
  let long_form = run [`Entry "2"; `Enter; `Entry "2"; `Enter; `Key '+'] in
  let shortcut = run [`Entry "2"; `Enter; `Entry "2"; `Key '+'] in
  check int "2<enter>2<enter>+" 4 (top_int long_form);
  check int "2<enter>2+" 4 (top_int shortcut)

let test_classic_operation_abbreviation_executes_functions () =
  let model, _ = Pelzl_model.init Pelzl_model.Classic () in
  let angle = "1.5707963267948966" in
  let model =
    { model with entry = angle; entry_cursor = String.length angle }
    |> classic_enter
    |> classic_key '\''
    |> classic_type "sin"
    |> classic_enter
  in
  check (float 1e-9) "'sin<enter>" 1. (top_float model);
  check bool "returns to main" true (model.classic_mode = ClassicMain)

let test_classic_command_abbreviation_executes_commands () =
  let model = model_with_ints [42] in
  let model = model |> classic_key '\'' |> classic_type "deg" |> classic_enter in
  check bool "'deg<enter>" true ((get_modes model.calc).angle = Deg);
  let model = model |> classic_key '\'' |> classic_type "drop" |> classic_enter in
  check int "'drop<enter>" 0 (stack_length model.calc.stack)

let test_classic_unknown_abbreviation_stays_in_mode () =
  let model, _ = Pelzl_model.init Pelzl_model.Classic () in
  let model = model |> classic_key '\'' |> classic_type "nosuch" |> classic_enter in
  check bool "stays in abbrev mode" true
    (match model.classic_mode with ClassicAbbrev OperationAbbrev -> true | _ -> false);
  check (option string) "error" (Some "unknown abbreviation: nosuch") model.error_msg

let test_classic_constant_mode_pushes_constant () =
  let model, _ = Pelzl_model.init Pelzl_model.Classic () in
  let model = model |> classic_key 'C' |> classic_type "g" |> classic_enter in
  match stack_peek 1 model.calc.stack with
  | RpcFloatUnit (f, units) ->
      check (float 1e-9) "g" 9.80665 f;
      check bool "has units" true (units <> Units.empty_unit)
  | _ -> fail "expected constant value"

let test_classic_variable_mode_and_eval () =
  let model, _ = Pelzl_model.init Pelzl_model.Classic () in
  let model =
    { model with entry = "42"; entry_cursor = 2 }
    |> classic_enter
    |> classic_key '@'
    |> classic_type "foo"
    |> classic_enter
    |> classic_key 'S'
    |> classic_key '@'
    |> classic_type "foo"
    |> classic_enter
    |> classic_key ';'
  in
  check int "stored variable eval" 42 (top_int model)

let test_classic_variable_completion_cycles () =
  let model, _ = Pelzl_model.init Pelzl_model.Classic () in
  Hashtbl.replace (get_variables model.calc) "foo" (RpcInt (Big_int.big_int_of_int 1));
  Hashtbl.replace (get_variables model.calc) "fop" (RpcInt (Big_int.big_int_of_int 2));
  let model = model |> classic_key '@' |> classic_type "fo" in
  let model, _ = Pelzl_update.update (Key_input (key Input.Key.Tab)) model in
  check string "first completion" "foo" model.entry;
  let model, _ = Pelzl_update.update (Key_input (key Input.Key.Tab)) model in
  check string "second completion" "fop" model.entry

let test_classic_browse_echo_and_drop_selected () =
  let model = model_with_ints [1; 2; 3] in
  let model =
    model
    |> fun m -> fst (Pelzl_update.update (Key_input (key Input.Key.Up)) m)
    |> fun m -> fst (Pelzl_update.update (Key_input (key Input.Key.Up)) m)
    |> classic_enter
  in
  check int "echo selected level 2" 2 (top_int model);
  let model =
    model
    |> fun m -> fst (Pelzl_update.update (Key_input (key Input.Key.Up)) m)
    |> fun m -> fst (Pelzl_update.update (Key_input (key Input.Key.Up)) m)
    |> classic_key 'd'
  in
  check int "drop selected length" 3 (stack_length model.calc.stack);
  check int "top after drop selected" 2 (top_int model)

let test_classic_browse_keep_and_roll () =
  let model = model_with_ints [1; 2; 3] in
  let model =
    model
    |> fun m -> fst (Pelzl_update.update (Key_input (key Input.Key.Up)) m)
    |> fun m -> fst (Pelzl_update.update (Key_input (key Input.Key.Up)) m)
    |> classic_key 'k'
  in
  check int "keep selected length" 1 (stack_length model.calc.stack);
  check int "kept selected" 2 (top_int model);
  let model = model_with_ints [1; 2; 3] in
  let model =
    model
    |> fun m -> fst (Pelzl_update.update (Key_input (key Input.Key.Up)) m)
    |> fun m -> fst (Pelzl_update.update (Key_input (key Input.Key.Up)) m)
    |> classic_key 'r'
  in
  check int "roll selected range" 2 (top_int model)

let test_fullscreen_render_round_trips_through_txtin () =
  let check_round_trip data check_parsed =
    let calc =
      { empty_state with stack = stack_push data empty_state.stack }
    in
    let text = get_fullscreen_display 1 calc in
    check_parsed text (one_deg text)
  in
  check_round_trip
    (RpcInt (Big_int.big_int_of_int 123))
    (fun text parsed ->
      check string "int text" "#123`d" text;
      check_int "int parsed" 123 parsed);
  check_round_trip
    (RpcVariable "myvar")
    (fun text parsed ->
      check string "variable text" "@myvar" text;
      match parsed with
      | RpcVariable "myvar" -> ()
      | _ -> fail "variable should round-trip");
  check_round_trip
    (one_deg "[[1, 2][3.1, 4.5e10]]")
    (fun text parsed ->
      check bool "matrix multiline" true (String.contains text '\n');
      check_fmat "matrix parsed"
        [| [| 1.; 2. |]; [| 3.1; 4.5e10 |] |]
        parsed);
  check_round_trip
    (one_deg "[[(1, 0), 5][1e10, (2 <90)]]")
    (fun _ parsed ->
      check_cmat "complex matrix parsed"
        [| [| c 1. 0.; c 5. 0. |]; [| c 1e10 0.; c 0. 2. |] |]
        parsed)

let test_classic_view_uses_external_editor_without_mutating_stack () =
  let model = model_with_ints [1; 2] in
  with_temp_datadir (fun _dir ->
    let saw_runner = ref false in
    let editor_runner path =
      saw_runner := true;
      check string "view file" "fullscreen" (Filename.basename path);
      check string "view content" "#2`d" (read_file path);
      write_file path "#999`d"
    in
    let model = classic_key_with_editor editor_runner 'v' model in
    check bool "runner called" true !saw_runner;
    check int "stack unchanged length" 2 (stack_length model.calc.stack);
    check int "stack unchanged top" 2 (top_int model))

let test_classic_edit_input_reuses_buffer_and_pushes_values () =
  let model, _ = Pelzl_model.init Pelzl_model.Classic () in
  with_temp_datadir (fun dir ->
    let input = Filename.concat dir "input" in
    write_file input "3";
    let saw_runner = ref false in
    let editor_runner path =
      saw_runner := true;
      check string "edit file" "input" (Filename.basename path);
      check string "preserved buffer" "3" (read_file path);
      write_file path "4 5"
    in
    let model = classic_key_with_editor editor_runner 'E' model in
    check bool "runner called" true !saw_runner;
    check int "two values pushed" 2 (stack_length model.calc.stack);
    check int "top value" 5 (stack_int 1 model);
    check int "second value" 4 (stack_int 2 model))

let test_classic_browse_view_and_edit_selected_entry () =
  let select_second model =
    model
    |> fun m -> fst (Pelzl_update.update (Key_input (key Input.Key.Up)) m)
    |> fun m -> fst (Pelzl_update.update (Key_input (key Input.Key.Up)) m)
  in
  let model = model_with_ints [1; 2; 3] |> select_second in
  with_temp_datadir (fun _dir ->
    let editor_runner path =
      check string "browse view file" "fullscreen" (Filename.basename path);
      check string "browse view selected" "#2`d" (read_file path)
    in
    let model = classic_key_with_editor editor_runner 'v' model in
    check int "browse view unchanged selected" 2 (stack_int 2 model));
  let model = model_with_ints [1; 2; 3] |> select_second in
  with_temp_datadir (fun _dir ->
    let editor_runner path =
      check string "browse edit file" "input" (Filename.basename path);
      check string "browse edit prefilled" "#2`d" (read_file path);
      write_file path "#9`d #8`d"
    in
    let model = classic_key_with_editor editor_runner 'E' model in
    check int "browse edit length" 3 (stack_length model.calc.stack);
    check int "browse edit leaves top alone" 3 (stack_int 1 model);
    check int "browse edit last parsed value wins" 8 (stack_int 2 model);
    check int "browse edit bottom unchanged" 1 (stack_int 3 model))

let test_classic_external_editor_parse_error_preserves_stack () =
  let model = model_with_ints [7] in
  with_temp_datadir (fun _dir ->
    let editor_runner path = write_file path "[" in
    let model = classic_key_with_editor editor_runner 'E' model in
    check int "stack length unchanged" 1 (stack_length model.calc.stack);
    check int "top unchanged" 7 (top_int model);
    check (option string) "parse error"
      (Some "syntax error in input") model.error_msg)

let test_unit_formatting_example_parses () =
  let data = one_deg "1_N*nm^2*kg/s/in^-3*GHz^2.34" in
  check bool "unit formatting example has units"
    true
    (String.contains (render_data data) '_')

let test_rad_parser_variant_for_polar_examples () =
  check_complex "rad parser keeps polar angle in radians"
    (cos 1.) (sin 1.) (one_deg "(1 <57.29577951308232)");
  check_complex "explicit rad parser"
    (cos 1.) (sin 1.) (match parse_rad "(1 <1)" with [x] -> x | _ -> fail "one")

let test_function_abbreviation_examples_are_registered () =
  ensure_orpie_defaults ();
  let abbreviations =
    [
      "inv"; "pow"; "sq"; "sqrt"; "abs"; "exp"; "ln"; "10^"; "log10";
      "conj"; "sin"; "cos"; "tan"; "sinh"; "cosh"; "tanh"; "asin";
      "acos"; "atan"; "asinh"; "acosh"; "atanh"; "re"; "im"; "gamma";
      "lngamma"; "erf"; "erfc"; "fact"; "gcd"; "lcm"; "binom"; "perm";
      "trans"; "trace"; "solvelin"; "mod"; "floor"; "ceil"; "toint";
      "toreal"; "add"; "sub"; "mult"; "div"; "neg"; "store"; "eval";
      "purge"; "total"; "mean"; "sumsq"; "var"; "varbias"; "stdev";
      "stdevbias"; "min"; "max"; "utpn"; "uconvert"; "ustand"; "uvalue";
    ]
  in
  List.iter
    (fun abbreviation ->
      try ignore (Rcfile.translate_abbrev abbreviation)
      with Not_found -> failf "missing abbreviation %S" abbreviation)
    abbreviations

let test_command_abbreviation_examples_are_registered () =
  ensure_orpie_defaults ();
  let abbreviations =
    [
      "drop"; "clear"; "swap"; "dup"; "undo"; "rad"; "deg"; "rect";
      "polar"; "bin"; "oct"; "dec"; "hex"; "view"; "edit"; "pi"; "rand";
      "refresh"; "about"; "quit";
    ]
  in
  List.iter
    (fun abbreviation ->
      try ignore (Rcfile.translate_abbrev abbreviation)
      with Not_found -> failf "missing abbreviation %S" abbreviation)
    abbreviations

let manual_example_tests =
  [
    ("overview addition example", `Quick, test_overview_addition_example);
    ("real number entry examples", `Quick, test_real_number_entry_examples);
    ("complex entry examples", `Quick, test_complex_entry_examples);
    ("matrix entry examples", `Quick, test_matrix_entry_examples);
    ("classic bracket key matrix entry example", `Quick,
     test_classic_bracket_key_matrix_entry_example);
    ("unit entry examples", `Quick, test_unit_entry_examples);
    ("exact integer entry examples", `Quick, test_exact_integer_entry_examples);
    ("variable entry example", `Quick, test_variable_entry_example);
    ("external editor sample inputs", `Quick, test_external_editor_sample_inputs);
    ("external editor multiple entries example", `Quick,
     test_external_editor_multiple_entries_example);
    ("function shortcut examples", `Quick, test_function_shortcut_examples);
    ("classic operation abbreviation executes functions", `Quick,
     test_classic_operation_abbreviation_executes_functions);
    ("classic command abbreviation executes commands", `Quick,
     test_classic_command_abbreviation_executes_commands);
    ("classic unknown abbreviation stays in mode", `Quick,
     test_classic_unknown_abbreviation_stays_in_mode);
    ("classic constant mode pushes constant", `Quick,
     test_classic_constant_mode_pushes_constant);
    ("classic variable mode and eval", `Quick, test_classic_variable_mode_and_eval);
    ("classic variable completion cycles", `Quick,
     test_classic_variable_completion_cycles);
    ("classic browse echo and drop selected", `Quick,
     test_classic_browse_echo_and_drop_selected);
    ("classic browse keep and roll", `Quick, test_classic_browse_keep_and_roll);
    ("fullscreen render round trips through txtin", `Quick,
     test_fullscreen_render_round_trips_through_txtin);
    ("classic view uses external editor without mutating stack", `Quick,
     test_classic_view_uses_external_editor_without_mutating_stack);
    ("classic edit input reuses buffer and pushes values", `Quick,
     test_classic_edit_input_reuses_buffer_and_pushes_values);
    ("classic browse view and edit selected entry", `Quick,
     test_classic_browse_view_and_edit_selected_entry);
    ("classic external editor parse error preserves stack", `Quick,
     test_classic_external_editor_parse_error_preserves_stack);
    ("unit formatting example parses", `Quick, test_unit_formatting_example_parses);
    ("polar examples support rad and degree parsing", `Quick,
     test_rad_parser_variant_for_polar_examples);
    ("function abbreviation examples registered", `Quick,
     test_function_abbreviation_examples_are_registered);
    ("command abbreviation examples registered", `Quick,
     test_command_abbreviation_examples_are_registered);
  ]
