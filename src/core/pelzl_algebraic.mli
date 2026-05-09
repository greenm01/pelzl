(*  Pelzl -- a modern RPN calculator for the console
 *  Copyright (C) 2026 Mason Austin Green
 *
 *  Algebraic front-end: tokenizer, parser, evaluator, and structured
 *  errors for the default REPL. All evaluation flows through the
 *  underlying [Pelzl_engine] (single source of truth for stack,
 *  variables, and modes).
 *)

(** A source span (offsets into the original input string). *)
type span = { start : int; len : int }

(** Token kinds emitted by the lexer. The lexer is total and never
    raises; lex errors surface as [TError]. *)
type token_kind =
  | T_num of float
  | T_int of string * int            (** literal text, base in {2,8,10,16} *)
  | T_ident of string
  | T_func of string                 (** identifier immediately followed by '(' *)
  | T_lparen
  | T_rparen
  | T_comma
  | T_assign
  | T_op of char
  | T_error of string                 (** an offending character *)

type token = { kind : token_kind; span : span }

(** Structured evaluation errors with source positions where possible. *)
type error =
  | E_lex of span * string
  | E_parse of span option * string
  | E_unknown_var of string
  | E_unknown_fun of string
  | E_arity of { fn : string; expected : int; got : int }
  | E_engine of string

val pp_error : error -> string

(** Pure tokenizer over the raw input. *)
val tokenize : string -> token list

(** Parsed top-level statement. *)
type stmt =
  | S_expr of token list             (** RPN tokens (postfix) *)
  | S_assign of string * token list  (** target name, postfix expression *)

val parse : string -> (stmt, error) result

(** Evaluate a parsed statement against the engine state. Returns the
    new state and a display string for the produced value, or a
    structured error. Variable assignment is performed through
    [Pelzl_engine.cmd_store]. *)
val eval :
  Pelzl_engine.calc_state ->
  stmt ->
  (Pelzl_engine.calc_state * string, error) result

(** Convenience: parse-then-eval. *)
val run :
  Pelzl_engine.calc_state ->
  string ->
  (Pelzl_engine.calc_state * string, error) result
