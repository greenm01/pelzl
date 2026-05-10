open Pelzl_model

let get_mode_str calc =
  let m = Pelzl_engine.get_modes calc in
  Printf.sprintf "[%s %s %s]"
    (match m.angle with Rad -> "RAD" | Deg -> "DEG")
    (match m.base with Bin -> "BIN" | Oct -> "OCT" | Hex -> "HEX" | Dec -> "DEC")
    (match m.complex with Rect -> "RECT" | Polar -> "POLAR")

(* -------------------------------------------------------------------- *)
(* Classic RPN view.                                                    *)
(* -------------------------------------------------------------------- *)

let starts_with ~prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let take n xs =
  let rec aux n xs acc =
    if n <= 0 then List.rev acc
    else
      match xs with
      | [] -> List.rev acc
      | x :: rest -> aux (n - 1) rest (x :: acc)
  in
  aux n xs []

let scroll_string offset s =
  let offset = max 0 offset in
  if offset >= String.length s then "" else String.sub s offset (String.length s - offset)

let variable_names calc =
  Hashtbl.fold
    (fun name _ acc -> name :: acc)
    (Pelzl_engine.get_variables calc)
    []
  |> List.sort String.compare

let matching_names prefix names =
  names |> List.filter (starts_with ~prefix) |> take 10

let clip_string width s =
  if String.length s <= width then s else String.sub s 0 width

let fit_string width s =
  let s = clip_string width s in
  s ^ String.make (max 0 (width - String.length s)) ' '

let fit_lines width height lines =
  let rec take n xs acc =
    if n <= 0 then List.rev acc
    else
      match xs with
      | [] -> take (n - 1) [] (String.make width ' ' :: acc)
      | x :: rest -> take (n - 1) rest (fit_string width x :: acc)
  in
  take height lines []

let abbreviation_help_lines =
  [
    "                                      ";
    "Abbreviations:                        ";
    " Common Functions:                    ";
    "  sin  asin  cos  acos  tan  atan     ";
    "  exp  ln  10^  log10  sq  sqrt  inv  ";
    "  gamma  lngamma  erf  erfc  trans    ";
    "  re  im  mod  floor  ceil  toint     ";
    "  toreal  eval  store  purge          ";
    "                                      ";
    " Change Modes:                        ";
    "  rad  deg  bin  oct  dec  hex  rect  ";
    "  polar                               ";
    "                                      ";
    " Miscellaneous:                       ";
    "  pi  undo  view                      ";
    "                                      ";
    " execute abbreviation : <return>      ";
    " cancel abbreviation  : '             ";
  ]

let modal_help_lines model =
  let candidates label names =
    let matches = matching_names model.entry names in
    "" :: (label ^ ":") ::
    (match matches with
     | [] -> [ "  no matches" ]
     | xs -> List.map (Printf.sprintf "  %s") xs)
  in
  match model.classic_mode with
  | ClassicMain -> []
  | ClassicAbbrev OperationAbbrev -> abbreviation_help_lines
  | ClassicAbbrev ConstantAbbrev ->
      candidates "Constants" !Rcfile.constant_symbols
  | ClassicVariable _ ->
      candidates "Variables" (variable_names model.calc)
  | ClassicBrowse { selected_level; hscroll } ->
      [ ""; "Browse:";
        Printf.sprintf "  level: %d" selected_level;
        Printf.sprintf "  hscroll: %d" hscroll ]

let entry_prefix = function
  | ClassicMain | ClassicBrowse _ -> ""
  | ClassicAbbrev OperationAbbrev -> "'"
  | ClassicAbbrev ConstantAbbrev -> "C "
  | ClassicVariable _ -> "@ "

let classic_help_rows model left_width height =
  let m = Pelzl_engine.get_modes model.calc in
  let angle_str = match m.angle with Rad -> "RAD" | Deg -> "DEG" in
  let base_str = match m.base with Bin -> "BIN" | Oct -> "OCT" | Hex -> "HEX" | Dec -> "DEC" in
  let complex_str = match m.complex with Rect -> "REC" | Polar -> "POL" in
  let header_lines = [
    Printf.sprintf "Pelzl v1.0 -- %-24s " model.slogan;
    "--------------------------------------";
    "Calculator Modes:                     ";
    Printf.sprintf "  angle: %-3s  base: %-3s  complex: %-3s " angle_str base_str complex_str;
    "                                      ";
  ] in
  let main_lines = [
    "Common Operations:                    ";
    "  enter    : <return>                 ";
    "  drop     : \\                        ";
    "  swap     : <pagedown>               ";
    "  backspace: \\177                     ";
    "  add      : +                        ";
    "  subtract : -                        ";
    "  multiply : *                        ";
    "  divide   : /                        ";
    "  y^x      : ^                        ";
    "  negation : n                        ";
    "Miscellaneous:                        ";
    "  scientific notation     : <space>   ";
    "  abbreviation entry mode : '         ";
    "  stack browsing mode     : <up>      ";
    "  repl mode               : 'repl RET ";
    "  refresh display         : C-L       ";
    "  quit                    : Q         ";
  ] in
  let lines =
    match model.classic_mode with
    | ClassicAbbrev OperationAbbrev -> header_lines @ abbreviation_help_lines
    | _ -> header_lines @ main_lines @ modal_help_lines model
  in
  lines
  |> fit_lines left_width height

let classic_stack_rows model stack_width height =
  let selected =
    match model.classic_mode with
    | ClassicBrowse { selected_level; hscroll } -> Some (selected_level, hscroll)
    | _ -> None
  in
  List.init height (fun i ->
    let idx = height - i in
    let selected_line =
      match selected with
      | Some (level, _) -> level = idx
      | None -> false
    in
    let raw = Pelzl_engine.get_display_line idx model.calc in
    let line =
      match selected with
      | Some (_, hscroll) when selected_line -> scroll_string hscroll raw
      | _ -> raw
    in
    let prefix = Printf.sprintf "| %2d:" idx in
    let text =
      if line = "" then prefix
      else
        let pad_len = stack_width - String.length prefix - String.length line in
        let pad = if pad_len > 0 then String.make pad_len ' ' else " " in
        prefix ^ pad ^ line
    in
    selected_line, fit_string stack_width text)

let classic_view model =
  let view_width = 80 in
  let max_stack_lines = max 1 (model.height - 2) in
  let left_width = 38 in
  let stack_width = view_width - left_width in
  let selected_style = Mosaic.Ansi.Style.make ~inverse:true () in
  let stack_nodes =
    classic_stack_rows model stack_width max_stack_lines
    |> List.map (fun (selected_line, text) ->
      if selected_line then Mosaic.text ~style:selected_style text
      else Mosaic.text text)
  in
  let help_nodes =
    classic_help_rows model left_width max_stack_lines
    |> List.map Mosaic.text
  in
  let left_pane =
    Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Column
      ~size:(Mosaic.size_wh (Mosaic.px 38) (Mosaic.pct 100))
      help_nodes
  in
  let right_pane =
    Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Column
      ~size:(Mosaic.size_wh (Mosaic.px stack_width) (Mosaic.pct 100))
      stack_nodes
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
            Mosaic.text (entry_prefix model.classic_mode);
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

(* -------------------------------------------------------------------- *)
(* Repl (default) view: a single sober prompt line with live syntax     *)
(* highlighting. Renders inline above terminal scrollback (Mosaic       *)
(* `Primary mode); committed input/result records are pushed into the   *)
(* scrollback area via Cmd.static_commit from the update function.      *)
(* -------------------------------------------------------------------- *)

(* Highlight styles. *)
let style_prompt =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Cyan ~bold:true ()
let style_number =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Yellow ()
let style_op =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Magenta ()
let style_func =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Cyan ()
let style_ident_known =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Green ()
let style_ident_unknown =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.White ()
let style_assign =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Magenta ~bold:true ()
let style_paren =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Bright_black ()
let style_err =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Red ~underline:true ()
let style_meta =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Bright_blue ()
let style_dim =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Bright_black ()
let style_cursor =
  Mosaic.Ansi.Style.make ~inverse:true ()

(* Tokenize the current entry and produce a list of styled spans
   covering the entire string. Whitespace and inter-token gaps are
   emitted as plain text. *)
let highlight_entry calc s : _ Mosaic.t list =
  if s = "" then []
  else if String.length s > 0 && s.[0] = ':' then
    [ Mosaic.text ~style:style_meta s ]
  else
    let toks = Pelzl_algebraic.tokenize s in
    let vars = Pelzl_engine.get_variables calc in
    let n = String.length s in
    let rec loop pos toks acc =
      match toks with
      | [] ->
          let acc =
            if pos < n then
              Mosaic.text (String.sub s pos (n - pos)) :: acc
            else acc
          in
          List.rev acc
      | t :: rest ->
          let { Pelzl_algebraic.start; len } = t.Pelzl_algebraic.span in
          let acc =
            if pos < start then
              Mosaic.text (String.sub s pos (start - pos)) :: acc
            else acc
          in
          let text = String.sub s start len in
          let style =
            match t.kind with
            | Pelzl_algebraic.T_num _ | T_int _ -> Some style_number
            | T_op _ -> Some style_op
            | T_func _ -> Some style_func
            | T_ident name ->
                if Hashtbl.mem vars name then Some style_ident_known
                else Some style_ident_unknown
            | T_lparen | T_rparen | T_comma -> Some style_paren
            | T_assign -> Some style_assign
            | T_error _ -> Some style_err
          in
          let node =
            match style with
            | Some st -> Mosaic.text ~style:st text
            | None -> Mosaic.text text
          in
          loop (start + len) rest (node :: acc)
    in
    loop 0 toks []

let highlight_entry_with_cursor calc s cursor =
  let n = String.length s in
  let cursor = max 0 (min cursor n) in
  let before = String.sub s 0 cursor in
  let before_nodes = highlight_entry calc before in
  if cursor >= n then
    before_nodes @ [ Mosaic.text ~style:style_cursor " " ]
  else
    let cursor_text = String.sub s cursor 1 in
    let after = String.sub s (cursor + 1) (n - cursor - 1) in
    before_nodes
    @ [ Mosaic.text ~style:style_cursor cursor_text ]
    @ highlight_entry calc after

(* Optional dim ghost preview: show "= <result>" inline if the current
   entry parses, has no assignment, and evaluates without error. *)
let isolated_preview_calc calc =
  let open Pelzl_engine in
  {
    calc with
    stack = { data = Array.copy calc.stack.data; len = 0 };
    variables = Hashtbl.copy (get_variables calc);
    backup = None;
  }

let preview_for calc s : string option =
  let trimmed =
    let n = String.length s in
    let i = ref 0 in
    while !i < n && (s.[!i] = ' ' || s.[!i] = '\t') do incr i done;
    if !i >= n then "" else String.sub s !i (n - !i)
  in
  if trimmed = "" || (String.length trimmed > 0 && trimmed.[0] = ':')
  then None
  else
    match Pelzl_algebraic.parse trimmed with
    | Error _ -> None
    | Ok (Pelzl_algebraic.S_assign _) -> None
    | Ok stmt ->
        (match Pelzl_algebraic.eval (isolated_preview_calc calc) stmt with
         | Ok (_, display) -> Some display
         | Error _ -> None)

let repl_hint_text =
  "  ↑↓ history  :rpn  :help  :vars  :quit  Ctrl-D exit"

let repl_view model =
  let prompt =
    Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row (
      [ Mosaic.text ~style:style_prompt "> " ]
      @ highlight_entry_with_cursor model.calc model.entry model.entry_cursor)
  in
  let status_row =
    match model.error_msg with
    | None ->
        (match preview_for model.calc model.entry with
         | None -> Mosaic.text " "
         | Some r ->
             Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
               [ Mosaic.text ~style:style_dim "  = ";
                 Mosaic.text ~style:style_dim r ])
    | Some msg ->
        Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
          [ Mosaic.text ~style:style_err ("  ! " ^ msg) ]
  in
  let hint_row =
    Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
      [ Mosaic.text ~style:style_dim repl_hint_text ]
  in
  Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Column
    [ prompt; status_row; hint_row ]

let view model =
  match model.ui_mode with
  | Classic -> classic_view model
  | Repl -> repl_view model
