(*  Pelzl -- a modern RPN calculator for the console
 *  Copyright (C) 2026 Mason Austin Green
 *
 *  Persistent REPL input history. XDG-compliant location;
 *  best-effort I/O so a missing/unwritable home does not break the REPL.
 *)

(** Path of the history file, preferring [$XDG_STATE_HOME/pelzl/history]
    and falling back to [$HOME/.local/state/pelzl/history]. *)
val path : unit -> string

(** [load ?max_entries ()] reads the history file and returns the lines
    in newest-first order, capped at [max_entries] (default 1000).
    Returns the empty list on any error. *)
val load : ?max_entries:int -> unit -> string list

(** [append line] appends [line] to the history file, creating the
    parent directory if needed. Silently ignores I/O errors so a broken
    filesystem cannot crash the REPL. Lines containing newline
    characters are flattened to single-line. *)
val append : string -> unit
