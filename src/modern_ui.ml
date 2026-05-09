(*  Pelzl -- a modern RPN calculator for the console
 *  Copyright (C) 2003-2004, 2005, 2006-2007, 2010, 2018 Paul Pelzl
 *
 *  This program is free software; you can redistribute it and/or modify
*  Copyright (C) 2026 Mason Austin Green
 *  it under the terms of the GNU General Public License, Version 3,
 *  as published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Mosaic

type model = {
  stack : float list;
  input : string;
}

type msg =
  | Key of string
  | Enter
  | Add
  | Sub
  | Mult
  | Div
  | Clear
  | Quit

let init () = ({ stack = []; input = "" }, Cmd.none)

let update msg model =
  match msg with
  | Key k -> ({ model with input = model.input ^ k }, Cmd.none)
  | Enter ->
      if model.input = "" then (model, Cmd.none)
      else
        let new_val = try float_of_string model.input with _ -> 0.0 in
        ({ stack = new_val :: model.stack; input = "" }, Cmd.none)
  | Add ->
      (match model.stack with
      | a :: b :: rest -> ({ stack = (b +. a) :: rest; input = "" }, Cmd.none)
      | _ -> (model, Cmd.none))
  | Sub ->
      (match model.stack with
      | a :: b :: rest -> ({ stack = (b -. a) :: rest; input = "" }, Cmd.none)
      | _ -> (model, Cmd.none))
  | Mult ->
      (match model.stack with
      | a :: b :: rest -> ({ stack = (b *. a) :: rest; input = "" }, Cmd.none)
      | _ -> (model, Cmd.none))
  | Div ->
      (match model.stack with
      | a :: b :: rest -> ({ stack = (b /. a) :: rest; input = "" }, Cmd.none)
      | _ -> (model, Cmd.none))
  | Clear -> ({ stack = []; input = "" }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let vbox ?gap children =
  box ?gap ~flex_direction:Flex_direction.Column children

let view model =
  let stack_items =
    model.stack
    |> List.rev
    |> List.map (fun x -> text (string_of_float x))
  in
  vbox [
    text ~style:(Ansi.Style.make ~fg:Ansi.Color.Yellow ()) "--- Mosaic RPN Calc ---";
    vbox stack_items;
    text ~style:(Ansi.Style.make ~fg:Ansi.Color.Green ()) ("> " ^ model.input);
    text ~style:(Ansi.Style.make ~fg:Ansi.Color.Bright_black ()) "Keys: [0-9] [+-*/] [Enter] [C] [Q]";
  ]

let subscriptions _model =
  Sub.on_key_all (fun ev ->
    let data = Event.Key.data ev in
    match data.key with
    | Char c ->
        let i = Uchar.to_int c in
        if i < 256 then
          let s = Char.chr i in
          if s >= '0' && s <= '9' then Some (Key (String.make 1 s))
          else if s = '+' then Some Add
          else if s = '-' then Some Sub
          else if s = '*' then Some Mult
          else if s = '/' then Some Div
          else if s = 'c' || s = 'C' then Some Clear
          else if s = 'q' || s = 'Q' then Some Quit
          else None
        else None
    | Enter -> Some Enter
    | _ -> None)

let run_modern () = run { init; update; view; subscriptions }
