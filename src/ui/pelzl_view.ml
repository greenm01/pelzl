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
    " repl mode            : Alt-R         ";
    " quit                 : Ctrl-D        ";
  ]

let control_help_lines lines =
  "" :: "Controls:" :: lines

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
      @ control_help_lines [
          "  execute constant : <return>";
          "  edit name        : <backspace>";
          "  cancel           : Esc";
          "  repl mode        : Alt-R";
          "  quit             : Ctrl-D";
        ]
  | ClassicVariable _ ->
      candidates "Variables" (variable_names model.calc)
      @ control_help_lines [
          "  complete         : <tab>";
          "  enter variable   : <return>";
          "  edit name        : <backspace>";
          "  cancel           : Esc";
          "  repl mode        : Alt-R";
          "  quit             : Ctrl-D";
        ]
  | ClassicBrowse { selected_level; hscroll } ->
      [ ""; "Browse:";
        Printf.sprintf "  level: %d" selected_level;
        Printf.sprintf "  hscroll: %d" hscroll;
        "";
        "Browse Controls:";
        "  move selection   : <up>/<down>";
        "  scroll entry     : <left>/<right>";
        "  echo selected    : <return>";
        "  view/edit        : v / E";
        "  drop/drop-N      : d or \\ / D";
        "  keep/keep-N      : k / K";
        "  roll down/up     : r / R";
        "  cancel           : q or Esc";
        "  repl mode        : Alt-R";
        "  quit             : Ctrl-D" ]

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
    "  repl mode               : Alt-R     ";
    "  refresh display         : C-L       ";
    "  quit                    : Ctrl-D/Q  ";
  ] in
  let lines =
    match model.classic_mode with
    | ClassicMain -> header_lines @ main_lines
    | _ -> header_lines @ modal_help_lines model
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
(* Repl (default) view: session-local transcript plus a sober prompt    *)
(* with live syntax highlighting.                                       *)
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
let style_result =
  Mosaic.Ansi.Style.make ~fg:Mosaic.Ansi.Color.Green ()
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
let style_plain =
  Mosaic.Ansi.Style.default

(* Tokenize the current entry and produce a list of styled spans
   covering the entire string. Whitespace and inter-token gaps are
   emitted as plain text. *)
let entry_spans calc s : Mosaic.span list =
  if s = "" then []
  else if String.length s > 0 && s.[0] = ':' then
    [ { Mosaic.text = s; style = style_meta } ]
  else
    let toks = Pelzl_algebraic.tokenize s in
    let vars = Pelzl_engine.get_variables calc in
    let n = String.length s in
    let rec loop pos toks acc =
      match toks with
      | [] ->
          let acc =
            if pos < n then
              { Mosaic.text = String.sub s pos (n - pos);
                style = style_plain } :: acc
            else acc
          in
          List.rev acc
      | t :: rest ->
          let { Pelzl_algebraic.start; len } = t.Pelzl_algebraic.span in
          let acc =
            if pos < start then
              { Mosaic.text = String.sub s pos (start - pos);
                style = style_plain } :: acc
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
          let style = Option.value style ~default:style_plain in
          loop (start + len) rest ({ Mosaic.text; style } :: acc)
    in
    loop 0 toks []

let clamp_cursor s cursor =
  let n = String.length s in
  max 0 (min cursor n)

let repl_prompt_plain s =
  "> " ^ s

let entry_spans_plain calc s =
  entry_spans calc s
  |> List.map (fun (span : Mosaic.span) -> span.text)
  |> String.concat ""

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
  "  ↑↓ history  [Alt-R] RPN  [Ctrl-Q] Quit  :help  :vars"

type repl_line_style =
  | Plain
  | Prompt
  | Result
  | Arrow
  | Error
  | Msg

type repl_line = (repl_line_style * string) list

let repl_msg_lines s =
  match String.split_on_char '\n' s with
  | [] -> [""]
  | lines -> lines

let repl_record_lines (r : repl_record) : repl_line list =
  match r with
  | Repl_ok { input; result } ->
      [
        [ Prompt, "> "; Plain, input ];
        [ Arrow, "  = "; Result, result ];
      ]
  | Repl_err { input; error } ->
      [
        [ Prompt, "> "; Plain, input ];
        [ Arrow, "  ! "; Error, error ];
      ]
  | Repl_msg s ->
      List.map
        (fun line -> if line = "" then [] else [ Msg, line ])
        (repl_msg_lines s)

let take_last n xs =
  if n <= 0 then []
  else
    let len = List.length xs in
    let drop = max 0 (len - n) in
    let rec aux i = function
      | [] -> []
      | x :: rest when i < drop -> aux (i + 1) rest
      | xs -> xs
    in
    aux 0 xs

let repl_transcript_lines ~height records =
  let available = max 0 (height - 3) in
  records
  |> List.concat_map repl_record_lines
  |> take_last available

let repl_line_plain line =
  String.concat "" (List.map snd line)

let repl_transcript_plain_lines ~height records =
  repl_transcript_lines ~height records |> List.map repl_line_plain

let style_of_repl_line = function
  | Plain -> None
  | Prompt -> Some style_prompt
  | Result -> Some style_result
  | Arrow -> Some style_dim
  | Error -> Some style_err
  | Msg -> Some style_dim

let empty_repl_row () =
  Mosaic.box
    ~display:Mosaic.Display.Flex
    ~flex_direction:Row
    ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.px 1))
    []

let repl_line_view line =
  let nodes =
    match line with
    | [] -> []
    | segments ->
        List.map
          (fun (style, text) ->
            match style_of_repl_line style with
            | None -> Mosaic.text text
            | Some style -> Mosaic.text ~style text)
          segments
  in
  match nodes with
  | [] -> empty_repl_row ()
  | _ -> Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row nodes

let repl_status_plain model =
  match model.error_msg with
  | None ->
      (match preview_for model.calc model.entry with
       | None -> ""
       | Some r -> "  = " ^ r)
  | Some msg -> "  ! " ^ msg

let repl_status_view model =
  match model.error_msg with
  | None ->
      (match preview_for model.calc model.entry with
       | None -> empty_repl_row ()
       | Some r ->
           Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
             [ Mosaic.text ~style:style_dim "  = ";
               Mosaic.text ~style:style_dim r ])
  | Some msg ->
      Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
        [ Mosaic.text ~style:style_err ("  ! " ^ msg) ]

let repl_view model =
  let transcript_rows =
    repl_transcript_lines ~height:model.height model.repl_transcript
    |> List.map repl_line_view
  in
  let prompt =
    Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
      [ Mosaic.text ~style:style_prompt "> ";
        Mosaic.textarea
          ~id:"repl-entry"
          ~autofocus:true
          ~focusable:true
          ~flex_grow:1.
          ~size:(Mosaic.size_wh (Mosaic.pct 100) (Mosaic.px 1))
          ~value:model.entry
          ~cursor:(clamp_cursor model.entry model.entry_cursor)
          ~spans:(entry_spans model.calc model.entry)
          ~wrap:`None
          ~text_color:Mosaic.Ansi.Color.White
          ~focused_text_color:Mosaic.Ansi.Color.White
          ~background_color:Mosaic.Ansi.Color.default
          ~focused_background_color:Mosaic.Ansi.Color.default
          ~cursor_style:`Line
          ~cursor_color:Mosaic.Ansi.Color.White
          ~cursor_blinking:false
          () ]
  in
  let status_row = repl_status_view model in
  let hint_row =
    Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Row
      [ Mosaic.text ~style:style_dim repl_hint_text ]
  in
  Mosaic.box ~display:Mosaic.Display.Flex ~flex_direction:Column
    (transcript_rows @ [ prompt; status_row; hint_row ])

let view model =
  match model.ui_mode with
  | Classic -> classic_view model
  | Repl -> repl_view model
