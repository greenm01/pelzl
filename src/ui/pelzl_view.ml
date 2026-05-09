open Pelzl_model

let get_mode_str calc =
  let m = Pelzl_engine.get_modes calc in
  Printf.sprintf "[%s %s %s]"
    (match m.angle with Rad -> "RAD" | Deg -> "DEG")
    (match m.base with Bin -> "BIN" | Oct -> "OCT" | Hex -> "HEX" | Dec -> "DEC")
    (match m.complex with Rect -> "RECT" | Polar -> "POLAR")

let classic_view model =
  let is_wide = model.width >= 80 in
  let max_stack_lines = max 1 (model.height - 2) in
  let left_width = if is_wide then 38 else 0 in
  let stack_width = model.width - left_width in
  
  let stack_lines =
    List.init max_stack_lines (fun i ->
      let idx = max_stack_lines - i in
      let line = Pelzl_engine.get_display_line idx model.calc in
      let num_str = Printf.sprintf "%2d:" idx in
      let bar = if is_wide then "| " else "" in
      let prefix = bar ^ num_str in
      if line = "" then
        prefix
      else
        let pad_len = stack_width - String.length prefix - String.length line in
        let pad = if pad_len > 0 then String.make pad_len ' ' else " " in
        prefix ^ pad ^ line)
  in
  let stack_text = String.concat "\n" stack_lines in
  
  let m = Pelzl_engine.get_modes model.calc in
  let angle_str = match m.angle with Rad -> "RAD" | Deg -> "DEG" in
  let base_str = match m.base with Bin -> "BIN" | Oct -> "OCT" | Hex -> "HEX" | Dec -> "DEC" in
  let complex_str = match m.complex with Rect -> "REC" | Polar -> "POL" in
  let help_text =
    (Printf.sprintf "Pelzl v1.0 -- %-24s \n" model.slogan) ^
    "--------------------------------------\n" ^
    "Calculator Modes:                     \n" ^
    (Printf.sprintf "  angle: %-3s  base: %-3s  complex: %-3s \n" angle_str base_str complex_str) ^
    "                                      \n" ^
    "Common Operations:                    \n" ^
    "  enter    : <return>                 \n" ^
    "  drop     : \\                        \n" ^
    "  swap     : <pagedown>               \n" ^
    "  backspace: \\177                     \n" ^
    "  add      : +                        \n" ^
    "  subtract : -                        \n" ^
    "  multiply : *                        \n" ^
    "  divide   : /                        \n" ^
    "  y^x      : ^                        \n" ^
    "  negation : n                        \n" ^
    "Miscellaneous:                        \n" ^
    "  scientific notation     : <space>   \n" ^
    "  abbreviation entry mode : '         \n" ^
    "  stack browsing mode     : <up>      \n" ^
    "  refresh display         : C-L       \n" ^
    "  quit                    : Q         "
  in
  let left_pane =
    Mosaic.box ~display:Mosaic.Display.Block
      ~size:(Mosaic.size_wh (Mosaic.px 38) (Mosaic.pct 100))
      [ Mosaic.text help_text ]
  in
  let right_pane =
    Mosaic.box ~display:Mosaic.Display.Block
      ~size:(Mosaic.size_wh (if is_wide then Mosaic.px stack_width else Mosaic.pct 100) (Mosaic.pct 100))
      [ Mosaic.text stack_text ]
  in
  let main_area =
    if is_wide then
      Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
        ~flex_grow:1. [ left_pane; right_pane ]
    else
      Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
        ~flex_grow:1. [ right_pane ]
  in
  let divider_text = String.make model.width '-' in
  let entry_line =
    match model.error_msg with
    | Some msg -> Mosaic.text msg
    | None ->
        let cursor_style = Mosaic.Ansi.Style.make ~inverse:true () in
        let pad_len = max 0 (model.width - String.length model.entry - 1) in
        let pad = String.make pad_len ' ' in
        Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
          [ Mosaic.text pad;
            Mosaic.text model.entry;
            Mosaic.text ~style:cursor_style " " ]
  in
  Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Column
    ~size:(Mosaic.size_wh (Mosaic.px model.width) (Mosaic.pct 100))
    [ main_area;
      Mosaic.box ~display:Mosaic.Display.Block
        ~size:(Mosaic.size_wh (Mosaic.px model.width) (Mosaic.px 2))
        [ Mosaic.text divider_text;
          entry_line ]
    ]

let modern_view model =
  let max_stack_lines = max 1 (model.height - 4) in
  let stack_lines =
    List.init max_stack_lines (fun i ->
      let idx = max_stack_lines - i in
      let line = Pelzl_engine.get_display_line idx model.calc in
      if line = "" then "  " else Printf.sprintf "%2d: %s" idx line)
  in
  let stack_text = String.concat "\n" stack_lines in
  let mode_str = get_mode_str model.calc in
  let style_cyan = Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Cyan () in
  let style_magenta = Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Magenta () in
  let style_green = Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Green ~bold:true () in
  
  let left_pane =
    Mosaic.box ~display:Mosaic.Display.Block
      ~size:(Mosaic.size_wh (Mosaic.pct 60) (Mosaic.pct 100))
      [ Mosaic.text ~style:style_cyan stack_text ]
  in
  let right_pane =
    Mosaic.box ~display:Mosaic.Display.Block
      ~size:(Mosaic.size_wh (Mosaic.pct 40) (Mosaic.pct 100))
      [ Mosaic.text ~style:style_magenta "COMMANDS\n--------\n+ - * / ^\ns l e i c\nu d w \\ |\nh help  Q quit" ]
  in
  let entry_line =
    match model.error_msg with
    | Some msg -> Mosaic.text ~style:style_green (" \u{26A0} " ^ msg)
    | None ->
        let cursor_style = Mosaic.Ansi.Style.make ~inverse:true () in
        Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
          [ Mosaic.text ~style:style_green (Printf.sprintf "\u{25B6} %s" model.entry);
            Mosaic.text ~style:cursor_style " ";
            Mosaic.text " ";
            Mosaic.text mode_str ]
  in
  Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Column
    ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.pct 100))
    [ Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
        ~flex_grow:1. [ left_pane; right_pane ];
      Mosaic.box ~display:Mosaic.Display.Block
        ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.px 2))
        [ entry_line ]
    ]

let view model =
  match model.ui_mode with
  | Classic -> classic_view model
  | Modern -> modern_view model
