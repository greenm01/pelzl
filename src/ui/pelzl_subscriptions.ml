open Pelzl_model

let is_ctrl_char (data : Input.Key.event) ch =
  match data.key with
  | Char c ->
      (data.modifier.ctrl
       && Uchar.is_char c
       && Char.lowercase_ascii (Uchar.to_char c) = ch)
      || Uchar.to_int c = Char.code ch - Char.code 'a' + 1
  | _ -> false

let subscriptions model =
  Mosaic.Sub.batch [
    Mosaic.Sub.on_key_all (fun ev ->
      let data = Mosaic.Event.Key.data ev in
      if is_ctrl_char data 'q' then Some Quit
      else if is_ctrl_char data 'd' && model.entry = "" then Some Quit
      else match data.key with
      | _ -> Some (Key_input ev)
    );
    Mosaic.Sub.on_resize (fun ~width ~height -> Resize (width, height));
  ]
