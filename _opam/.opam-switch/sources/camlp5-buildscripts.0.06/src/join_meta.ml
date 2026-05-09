(** -syntax camlp5o *)

open Rresult
open Bos
open Fpath

let read_fully ifile = OS.File.read ifile

let ( let* ) x f = Rresult.(>>=) x f

let push l x = l := x :: !l

let read_ic_fully ?(msg = "") ?(channel = stdin) () =
  let fd = Unix.descr_of_in_channel channel in
  if Unix.isatty fd && msg <> "" then
    begin Printf.printf "%s\n" msg; flush stdout end;
  let b = Buffer.create 23 in
  let rec rrec () =
    match try Some (input_char channel) with End_of_file -> None with
      None -> Buffer.contents b
    | Some c -> Buffer.add_char b c; rrec ()
  in
  rrec ()

let pkgmap = ref []

let direct_include = ref ""

let wrap_subdirs = ref []

let split2 ~msg s =
  match
    (let __re__ = Re.Perl.compile_pat ~opts:[] "^([^:]+):([^:]+)$" in
     fun __subj__ ->
       match
         Option.map (fun __g__ -> Re.Group.get __g__ 1, Re.Group.get __g__ 2)
           (Re.exec_opt __re__ __subj__)
       with
         exception Not_found -> None
       | rv -> rv)
      s
  with
    Some (name, subdir) -> name, subdir
  | _ -> failwith Fmt.(str "%s: invalid arg <<%s>>" msg s)

let _ =
  Arg.
  (parse
    ["-direct-include", Arg.Set_string direct_include,
     "<subdir>    directly include <subdir>/META file";
     "-wrap-subdir",
     Arg.String (fun s -> push wrap_subdirs (split2 ~msg:"wrap-subdir" s)),
     "<name>:<subdir>    include <subdir>/META file wrapped as subpackage <name>";
     "-rewrite", Arg.String (fun s -> push pkgmap (split2 ~msg:"rewrite" s)),
     "<name1>:<name2>    rewrite packages named <name1> to <name2> in `require' statements"]
    (fun _ -> failwith "join_meta: no anonymous args supported")
    "join_meta -destdir <dir>")


let indent n txt =
  let pfx = String.make n ' ' in
  Re.replace ~all:true (Re.Perl.compile_pat ~opts:[`Multiline] "^")
    ~f:(fun __g__ -> String.concat "" [pfx]) txt

let fix txt =
  let l =
    (let __re__ = Re.Perl.compile_pat ~opts:[] "\\s*,\\s*" in
     fun __subj__ -> Re.split __re__ __subj__)
      txt
  in
  let f s =
    match List.assoc s !pkgmap with
      exception Not_found -> s
    | v -> v
  in
  let ol =
    l |>
      List.map
        (fun p ->
           Re.replace ~all:false (Re.Perl.compile_pat ~opts:[] "^([^.]+)")
             ~f:(fun __g__ ->
                f
                  (match Re.Group.get_opt __g__ 1 with
                     None -> ""
                   | Some s -> s))
             p)
  in
  String.concat "," ol

let fix0 txt =
  Re.replace ~all:false (Re.Perl.compile_pat ~opts:[] "\"([^\"]+)\"")
    ~f:(fun __g__ ->
       "\"" ^
       fix
         (match Re.Group.get_opt __g__ 1 with
            None -> ""
          | Some s -> s) ^
       "\"")
    txt


let fixdeps txt =
  Re.replace ~all:true
    (Re.Perl.compile_pat ~opts:[`Multiline] "^(.*require.*)$")
    ~f:(fun __g__ ->
       fix0
         (match Re.Group.get_opt __g__ 1 with
            None -> ""
          | Some s -> s))
    txt

let capturex (cmd, args) =
  let channel = Unix.open_process_args_in cmd args in
  let txt = read_ic_fully ~channel () in close_in channel; txt

let _ =
  if !direct_include <> "" then
    print_string
      (indent 2
         (fixdeps
            (R.failwith_error_msg
               (read_fully
                  (v (String.concat "" ["./"; !direct_include; "/META"]))))))
let _ =
  (!wrap_subdirs |> List.rev) |>
    List.iter
      (fun (name, subdir) ->
         let txt =
           indent 2
             (fixdeps
                (R.failwith_error_msg
                   (read_fully
                      (v (String.concat "" ["./"; subdir; "/META"])))))
         in
         print_string
           (String.concat "" ["\npackage \""; name; "\" (\n"; txt; "\n)\n"]))
