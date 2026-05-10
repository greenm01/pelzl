type entry_mode = Normal | Integer | Complex | Matrix | Units

type ui_mode = Repl | Classic

type classic_abbrev_kind = OperationAbbrev | ConstantAbbrev

type classic_mode =
  | ClassicMain
  | ClassicAbbrev of classic_abbrev_kind
  | ClassicVariable of { completion_prefix : string option }
  | ClassicBrowse of { selected_level : int; hscroll : int }

(* A committed REPL exchange: the input (echoed) and either a result
   string, an error message, or meta-command output. *)
type repl_record =
  | Repl_ok of { input : string; result : string }
  | Repl_err of { input : string; error : string }
  | Repl_msg of string

type model = {
  calc : Pelzl_engine.calc_state;
  entry : string;
  entry_cursor : int;
  entry_mode : entry_mode;
  classic_mode : classic_mode;
  ui_mode : ui_mode;
  slogan : string;
  error_msg : string option;          (* transient, cleared on next keystroke *)
  (* Repl: REPL input history, newest first.
     Classic: legacy trace log. *)
  history : string list;
  history_idx : int option;           (* Repl: None=live; Some k=k steps back *)
  history_save : string;              (* Repl: live entry stashed during nav *)
  show_help : bool;
  help_page : int;
  width : int;
  height : int;
  repl_transcript : repl_record list;  (* session-local visual transcript *)
}

type msg =
  | Key_input of Mosaic.Event.Key.t
  | Set_entry of string
  | Submit of string
  | History_prev
  | History_next
  | Backspace
  | Enter
  | Clear_error
  | Toggle_help
  | Toggle_angle
  | Toggle_complex
  | Cycle_base
  | Resize of int * int
  | Quit

let all_taglines = [|
  "RPN for the masses";
  "'=' is for the weak";
  "swap drop dup view";
  "I hate the mouse";
  "now w/ 800% more stack";
  "powered by OCaml";
  "compute _this_";
  "interface as art";
  "kick that data's ass";
  "Nice.";
  "configurability is key";
  ":wq";
  "the \"Mutt\" of calcs"
|]

let register_default_bindings () =
  let open Operations in
  let bind s op = Rcfile.register_binding s op in
  (* Edit operations *)
  bind "<backspace>" (Edit Backspace);
  bind "<return>" (Edit Enter);
  bind "n" (Edit Minus);
  bind "`" (Edit SciNotBase);
  bind "<space>" (Edit SciNotBase);
  bind "0o177" (Edit Backspace);
  bind "#" (Edit BeginInteger);
  bind "(" (Edit BeginComplex);
  bind "[" (Edit BeginMatrix);
  bind "," (Edit Separator);
  bind "<" (Edit Angle);
  bind "_" (Edit BeginUnits);
  (* Function operations *)
  bind "+" (Function Add);
  bind "-" (Function Sub);
  bind "*" (Function Mult);
  bind "/" (Function Div);
  bind "i" (Function Inv);
  bind "^" (Function Pow);
  bind "s" (Function Sqrt);
  bind "a" (Function Abs);
  bind "\\Ca" (Function Arg);
  bind "e" (Function Exp);
  bind "l" (Function Ln);
  bind "c" (Function Conj);
  bind "!" (Function Fact);
  bind "%" (Function Mod);
  bind "S" (Function Store);
  bind ";" (Function Eval);
  bind "G" (Function Gcd);
  bind "L" (Function Lcm);
  bind "B" (Function Binom);
  bind "M" (Function Perm);
  bind "t" (Function Total);
  bind "m" (Function Mean);
  bind "A" (Function Sumsq);
  bind "V" (Function Var);
  bind "W" (Function VarBias);
  bind "D" (Function Stdev);
  bind "X" (Function StdevBias);
  bind "N" (Function Min);
  bind "O" (Function Max);
  bind "U" (Function Utpn);
  bind "Z" (Function StandardizeUnits);
  bind "Y" (Function UnitValue);
  bind "T" (Function Trace);
  (* Command operations *)
  bind "\\" (Command Drop);
  bind "|" (Command Clear);
  bind "u" (Command Undo);
  bind "<pageup>" (Command Swap);
  bind "<pagedown>" (Command Swap);
  bind "<return>" (Command Dup);
  bind "<up>" (Command BeginBrowse);
  bind "C" (Command BeginConst);
  bind "r" (Command ToggleAngleMode);
  bind "p" (Command ToggleComplexMode);
  bind "b" (Command CycleBase);
  bind "P" (Command EnterPi);
  bind "E" (Command EditInput);
  bind "h" (Command CycleHelp);
  bind "Q" (Command Quit);
  bind "d" (Command Dup);
  bind "w" (Command Swap);
  bind "\\Cl" (Command Refresh);
  bind "v" (Command View);
  bind "'" (Command BeginAbbrev);
  bind "@" (Command BeginVar);
  bind "R" (Command SetRadians);
  ()

let normalize_for_mode mode model =
  let history =
    match mode with
    | Repl -> Pelzl_history.load ()
    | Classic -> []
  in
  { model with
    entry = "";
    entry_cursor = 0;
    entry_mode = Normal;
    classic_mode = ClassicMain;
    ui_mode = mode;
    error_msg = None;
    history;
    history_idx = None;
    history_save = "";
    show_help = false;
    help_page = 0;
    repl_transcript = model.repl_transcript }

let init ?initial_model mode () =
  match initial_model with
  | Some model ->
      normalize_for_mode mode model, Mosaic.Cmd.none
  | None ->
  register_default_bindings ();
  (try Rcfile.process_rcfile None with _ -> ());
  Random.self_init ();
  let slogan = all_taglines.(Random.int (Array.length all_taglines)) in
  let calc = Pelzl_engine.empty_state in
  let history = match mode with
    | Repl -> Pelzl_history.load ()
    | Classic -> []
  in
  ({ calc; entry = ""; entry_cursor = 0; entry_mode = Normal;
     classic_mode = ClassicMain; ui_mode = mode; slogan; error_msg = None;
     history; history_idx = None; history_save = ""; show_help = false;
     help_page = 0; width = 80; height = 24; repl_transcript = [] },
   Mosaic.Cmd.none)
