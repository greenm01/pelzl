(*  Pelzl -- a modern RPN calculator for the console
 *  Copyright (C) 2003-2004, 2005, 2006-2007, 2010, 2018 Paul Pelzl
 *  Copyright (C) 2026 Mason Austin Green
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License, Version 3,
 *  as published by the Free Software Foundation.
 *)

(* statefile.ml
 * This file contains code for saving and loading the calculator
 * state. *)

open Pelzl_engine

(* save to a datafile using the Marshal module *)
let save_state st =
   try
      let version_file = Utility.join_path !(Rcfile.datadir) "version" in
      let version_channel = Utility.open_or_create_out_bin version_file in
      output_string version_channel Version.version;
      close_out version_channel;
      let save_file = Utility.join_path !(Rcfile.datadir) "calc_state" in
      let save_channel = Utility.open_or_create_out_bin save_file in
      Marshal.to_channel save_channel
      (st.modes, st.variables, !Rcfile.autobind_keys, st.stack.data, st.stack.len) [];
      close_out save_channel
   with
      |Sys_error _ -> raise (Invalid_argument "can't open data file for writing")
      |Failure _   -> raise (Invalid_argument "can't serialize calculator data to file")


(* load from a datafile using the Marshal module *)
let load_state () =
   try
      let version_file = Utility.join_path !(Rcfile.datadir) "version" in
      if Sys.file_exists (Utility.expand_file version_file) then begin
         let version_channel = Utility.expand_open_in_ascii version_file in
         let ver_string = input_line version_channel in
         close_in version_channel;
         if ver_string = Version.version then begin
            let datafile = Utility.join_path !(Rcfile.datadir) "calc_state" in
            if Sys.file_exists (Utility.expand_file datafile) then begin
               let load_channel = Utility.expand_open_in_bin datafile in
               let data_modes, data_variables, data_autobind_keys, data_stack, data_len =
                  (Marshal.from_channel load_channel : calculator_modes *
                   (string, pelzl_data_t) Hashtbl.t *
                   (Pelzl_engine.key_binding * string * Operations.operation_t option * int) array *
                   (pelzl_data_t array) * int)
               in
               close_in load_channel;
               Rcfile.validate_saved_autobindings data_autobind_keys;
               (data_modes, data_variables, Some { data = data_stack; len = data_len })
            end else
               ({angle = Rad; base = Dec; complex = Rect}, Hashtbl.create 20, None)
         end else
            ({angle = Rad; base = Dec; complex = Rect}, Hashtbl.create 20, None)
      end else
         ({angle = Rad; base = Dec; complex = Rect}, Hashtbl.create 20, None)
   with
      |Sys_error _ -> raise (Invalid_argument "can't open calculator state data file")
      |Failure _   -> raise (Invalid_argument "can't deserialize calculator data from file")
