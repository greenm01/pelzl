open Pelzl_model

let subscriptions _model =
  Mosaic.Sub.batch [
    Mosaic.Sub.on_key_all (fun ev ->
      let data = Mosaic.Event.Key.data ev in
      match data.key with
      | Char c when Uchar.equal c (Uchar.of_char 'q') && data.modifier.ctrl -> Some Quit
      | _ -> Some (Key_input ev)
    );
    Mosaic.Sub.on_resize (fun ~width ~height -> Resize (width, height));
  ]
