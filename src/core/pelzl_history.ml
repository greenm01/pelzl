(*  Pelzl -- a modern RPN calculator for the console
 *  Copyright (C) 2026 Mason Austin Green
 *
 *  Persistent REPL input history.
 *)

let getenv_opt name =
  match Sys.getenv_opt name with
  | Some "" -> None
  | x -> x

let state_dir () =
  match getenv_opt "XDG_STATE_HOME" with
  | Some d -> Filename.concat d "pelzl"
  | None ->
      let home =
        match getenv_opt "HOME" with
        | Some h -> h
        | None -> Filename.get_temp_dir_name ()
      in
      Filename.concat home (Filename.concat ".local" (Filename.concat "state" "pelzl"))

let path () = Filename.concat (state_dir ()) "history"

(* mkdir -p, best effort. Silently ignores failures. *)
let rec mkdir_p dir =
  if dir = "" || dir = "/" || dir = "." then ()
  else if Sys.file_exists dir then ()
  else begin
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o700 with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    | _ -> ()
  end

let flatten_newlines s =
  String.map (fun c -> if c = '\n' || c = '\r' then ' ' else c) s

let trim_blank s =
  let s = String.trim s in
  s

let load ?(max_entries = 1000) () =
  let p = path () in
  if not (Sys.file_exists p) then []
  else
    try
      let ic = open_in p in
      let lines = ref [] in
      (try
         while true do
           let line = input_line ic in
           let line = trim_blank line in
           if line <> "" then lines := line :: !lines
         done
       with End_of_file -> ());
      close_in_noerr ic;
      (* !lines is newest-first (we prepended each as we read top-to-bottom,
         which is oldest-to-newest; so prepending yields newest first). *)
      let lines = !lines in
      let rec take n = function
        | [] -> []
        | _ when n <= 0 -> []
        | x :: rest -> x :: take (n - 1) rest
      in
      take max_entries lines
    with _ -> []

let append line =
  let line = flatten_newlines (trim_blank line) in
  if line = "" then ()
  else
    try
      let p = path () in
      mkdir_p (Filename.dirname p);
      let oc =
        open_out_gen [ Open_append; Open_creat; Open_wronly ] 0o600 p
      in
      output_string oc line;
      output_char oc '\n';
      close_out_noerr oc
    with _ -> ()
