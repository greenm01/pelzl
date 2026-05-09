open Pelzl_model

let view model =
  let max_stack_lines = max 1 (model.height - 4) in
  let stack_lines =
    List.init max_stack_lines (fun i ->
      let idx = max_stack_lines - i in
      let line = Pelzl_engine.get_display_line idx model.calc in
      if line = "" then "  " else Printf.sprintf "%2d: %s" idx line)
  in
  let stack_text = String.concat "\n" stack_lines in
  let entry_text = Printf.sprintf "> %s" model.entry in
  let mode_str =
    let m = Pelzl_engine.get_modes model.calc in
    Printf.sprintf "[%s %s %s]"
      (match m.angle with Rad -> "RAD" | Deg -> "DEG")
      (match m.base with Bin -> "BIN" | Oct -> "OCT" | Hex -> "HEX" | Dec -> "DEC")
      (match m.complex with Rect -> "RECT" | Polar -> "POLAR")
  in
  let status =
    match model.error_msg with
    | Some msg -> "Error: " ^ msg
    | None -> mode_str
  in
  let help_text =
    if model.show_help then
      "Help:\n+ add  - sub  * mult  / div\n" ^
      "i inv  s sqrt  a abs  n neg\n" ^
      "e exp  l ln    c conj\n" ^
      "u undo d dup   w swap  \\ drop\n" ^
      "| clear  r angle  p complex  b base\n" ^
      "h help  Q quit"
    else
      "Press h for help"
  in
  let bold = Mosaic.Ansi.Style.make ~bold:true () in
  let rev = Mosaic.Ansi.Style.make ~inverse:true () in
  let left_pane =
    Mosaic.box ~display:Mosaic.Display.Block
      ~size:(Mosaic.size_wh (Mosaic.pct 50) (Mosaic.pct 100))
      [ Mosaic.text stack_text ]
  in
  let right_pane =
    Mosaic.box ~display:Mosaic.Display.Block
      ~size:(Mosaic.size_wh (Mosaic.pct 50) (Mosaic.pct 100))
      [ Mosaic.text help_text ]
  in
  let bottom_bar =
    Mosaic.box ~display:Mosaic.Display.Block
      ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.px 2))
      [ Mosaic.text ~style:bold entry_text;
        Mosaic.text ~style:rev status ]
  in
  Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Column
    ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.pct 100))
    [ Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
        ~flex_grow:1. [ left_pane; right_pane ];
      bottom_bar ]
