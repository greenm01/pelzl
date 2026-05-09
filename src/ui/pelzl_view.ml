open Pelzl_model

let get_mode_str calc =
  let m = Pelzl_engine.get_modes calc in
  Printf.sprintf "[%s %s %s]"
    (match m.angle with Rad -> "RAD" | Deg -> "DEG")
    (match m.base with Bin -> "BIN" | Oct -> "OCT" | Hex -> "HEX" | Dec -> "DEC")
    (match m.complex with Rect -> "RECT" | Polar -> "POLAR")

let classic_view model =
  let is_wide = model.width >= 80 in
  let max_stack_lines = max 1 (model.height - 3) in
  let stack_lines =
    List.init max_stack_lines (fun i ->
      let idx = max_stack_lines - i in
      let line = Pelzl_engine.get_display_line idx model.calc in
      if line = "" then "" else Printf.sprintf "%2d: %s" idx line)
    |> List.filter (fun s -> s <> "")
  in
  let stack_text = String.concat "\n" stack_lines in
  let entry_text = Printf.sprintf "> %s" model.entry in
  let mode_str = get_mode_str model.calc in
  let status =
    match model.error_msg with
    | Some msg -> "Error: " ^ msg
    | None -> mode_str
  in
  let help_text =
    if model.show_help then
      "HELP:\n+ add  - sub  * mult  / div\n" ^
      "i inv  s sqrt  a abs  n neg\n" ^
      "e exp  l ln    c conj\n" ^
      "u undo d dup   w swap  \\ drop\n" ^
      "| clear  r angle  p complex  b base\n" ^
      "h help  Q quit"
    else
      "Press h for help"
  in
  let left_pane =
    Mosaic.box ~display:Mosaic.Display.Block
      ~size:(Mosaic.size_wh (Mosaic.pct 50) (Mosaic.pct 100))
      [ Mosaic.text (mode_str ^ "\n\n" ^ help_text) ]
  in
  let right_pane =
    Mosaic.box ~display:Mosaic.Display.Block
      ~size:(Mosaic.size_wh (if is_wide then Mosaic.pct 50 else Mosaic.pct 100) (Mosaic.pct 100))
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
  let bold = Mosaic.Ansi.Style.make ~bold:true () in
  let rev = Mosaic.Ansi.Style.make ~inverse:true () in
  Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Column
    ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.pct 100))
    [ main_area;
      Mosaic.box ~display:Mosaic.Display.Block
        ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.px 2))
        [ Mosaic.text ~style:bold entry_text;
          Mosaic.text ~style:rev status ]
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
  let entry_text = Printf.sprintf "\u{25B6} %s" model.entry in
  let mode_str = get_mode_str model.calc in
  let status =
    match model.error_msg with
    | Some msg -> " \u{26A0} " ^ msg
    | None -> mode_str
  in
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
  Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Column
    ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.pct 100))
    [ Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
        ~flex_grow:1. [ left_pane; right_pane ];
      Mosaic.box ~display:Mosaic.Display.Block
        ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.px 2))
        [ Mosaic.text ~style:style_green entry_text;
          Mosaic.text status ]
    ]

let view model =
  match model.ui_mode with
  | Classic -> classic_view model
  | Modern -> modern_view model
