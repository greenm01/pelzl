open Pelzl_model

let get_mode_str calc =
  let m = Pelzl_engine.get_modes calc in
  Printf.sprintf "[%s %s %s]"
    (match m.angle with Rad -> "RAD" | Deg -> "DEG")
    (match m.base with Bin -> "BIN" | Oct -> "OCT" | Hex -> "HEX" | Dec -> "DEC")
    (match m.complex with Rect -> "RECT" | Polar -> "POLAR")

let classic_view model =
  let view_width = 80 in
  let max_stack_lines = max 1 (model.height - 2) in
  let left_width = 38 in
  let stack_width = view_width - left_width in
  
  let stack_lines =
    List.init max_stack_lines (fun i ->
      let idx = max_stack_lines - i in
      let line = Pelzl_engine.get_display_line idx model.calc in
      let num_str = Printf.sprintf "%2d:" idx in
      let bar = "| " in
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
      ~size:(Mosaic.size_wh (Mosaic.px stack_width) (Mosaic.pct 100))
      [ Mosaic.text stack_text ]
  in
  let main_area =
    Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
      ~flex_grow:1. [ left_pane; right_pane ]
  in
  let divider_text = String.make view_width '-' in
  let entry_line =
    match model.error_msg with
    | Some msg ->
        Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
          [ Mosaic.box ~flex_grow:1. [];
            Mosaic.text msg ]
    | None ->
        let cursor_style = Mosaic.Ansi.Style.make ~inverse:true () in
        Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
          [ Mosaic.box ~flex_grow:1. [];
            Mosaic.text model.entry;
            Mosaic.text ~style:cursor_style " " ]
  in
  let ui_content =
    Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Column
      ~size:(Mosaic.size_wh (Mosaic.px view_width) (Mosaic.pct 100))
      [ main_area;
        Mosaic.box ~display:Mosaic.Display.Block
          ~size:(Mosaic.size_wh (Mosaic.px view_width) (Mosaic.px 2))
          [ Mosaic.text divider_text;
            entry_line ]
      ]
  in
  Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
    ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.pct 100))
    [ ui_content;
      Mosaic.box ~flex_grow:1. [] ]

let repl_view model =
  let style_cyan = Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Cyan () in
  let style_magenta = Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Magenta () in
  let style_green = Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Green ~bold:true () in
  let style_yellow = Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Yellow ~bold:true () in
  let style_white = Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.White () in

  (* Left Pane: Stack *)
  let max_stack_lines = max 1 (model.height - 4) in
  let stack_lines =
    List.init max_stack_lines (fun i ->
      let idx = max_stack_lines - i in
      let line = Pelzl_engine.get_display_line idx model.calc in
      if line = "" then "  " else Printf.sprintf "%2d: %s" idx line)
  in
  let stack_text = String.concat "\n" stack_lines in
  let stack_pane =
    Mosaic.box ~display:Mosaic.Display.Block
      ~size:(Mosaic.size_wh (Mosaic.pct 40) (Mosaic.pct 100))
      [ Mosaic.text ~style:style_white "STACK\n-----\n";
        Mosaic.text ~style:style_cyan stack_text ]
  in

  (* Middle Pane: History/Trace Log *)
  let history_lines =
    let len = List.length model.history in
    let max_lines = model.height - 4 in
    if len > max_lines then
      let _, h = List.fold_left (fun (i, acc) x -> if i >= len - max_lines then (i+1, acc @ [x]) else (i+1, acc)) (0, []) model.history in h
    else model.history
  in
  let history_text = String.concat "\n" (List.map (fun s -> " \u{2192} " ^ s) history_lines) in
  let history_pane =
    Mosaic.box ~display:Mosaic.Display.Block ~flex_grow:1.
      [ Mosaic.text ~style:style_white "TRACE LOG\n---------\n";
        Mosaic.text ~style:style_yellow history_text ]
  in

  (* Right Pane: Modes and Vars *)
  let mode_str = get_mode_str model.calc in
  let info_pane =
    Mosaic.box ~display:Mosaic.Display.Block
      ~size:(Mosaic.size_wh (Mosaic.px 20) (Mosaic.pct 100))
      [ Mosaic.text ~style:style_white "INFO\n----\n";
        Mosaic.text ~style:style_magenta mode_str;
        Mosaic.text ~style:style_magenta "\n\nh help\nQ quit" ]
  in

  let entry_line =
    match model.error_msg with
    | Some msg -> Mosaic.text ~style:style_green (" \u{26A0} " ^ msg)
    | None ->
        let cursor_style = Mosaic.Ansi.Style.make ~inverse:true () in
        Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
          [ Mosaic.text ~style:style_green (Printf.sprintf ">>> %s" model.entry);
            Mosaic.text ~style:cursor_style " " ]
  in

  Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Column
    ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.pct 100))
    [ Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
        ~flex_grow:1. [ stack_pane; history_pane; info_pane ];
      Mosaic.box ~display:Mosaic.Display.Block
        ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.px 2))
        [ entry_line ]
    ]

let view model =
  match model.ui_mode with
  | Classic -> classic_view model
  | Repl -> repl_view model
