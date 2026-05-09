type entry_mode = Normal | Integer | Complex | Matrix | Units

type ui_mode = Modern | Classic

type model = {
  calc : Pelzl_engine.calc_state;
  entry : string;
  entry_mode : entry_mode;
  ui_mode : ui_mode;
  error_msg : string option;
  show_help : bool;
  help_page : int;
  width : int;
  height : int;
}

type msg =
  | Key_input of Mosaic.Event.Key.t
  | Backspace
  | Enter
  | Clear_error
  | Toggle_help
  | Toggle_angle
  | Toggle_complex
  | Cycle_base
  | Resize of int * int
  | Quit

let register_default_bindings () =
  let open Operations in
  let bind s op = Rcfile.register_binding s op in
  (* Edit operations *)
  bind "<backspace>" (Edit Backspace);
  bind "<return>" (Edit Enter);
  bind "n" (Edit Minus);
  bind "`" (Edit SciNotBase);
  bind "<space>" (Edit SciNotBase);
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
  bind "C" (Function ConvertUnits);
  bind "Y" (Function UnitValue);
  bind "T" (Function Trace);
  (* Command operations *)
  bind "\\" (Command Drop);
  bind "|" (Command Clear);
  bind "u" (Command Undo);
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

let init mode () =
  register_default_bindings ();
  (try Rcfile.process_rcfile None with _ -> ());
  let calc = Pelzl_engine.empty_state in
  let calc = { calc with modes = { angle = Deg; base = Dec; complex = Rect } } in
  ({ calc; entry = ""; entry_mode = Normal; ui_mode = mode; error_msg = None;
     show_help = false; help_page = 0; width = 80; height = 24 },
   Mosaic.Cmd.none)
