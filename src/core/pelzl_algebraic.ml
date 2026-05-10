(*  Pelzl -- a modern RPN calculator for the console
 *  Copyright (C) 2026 Mason Austin Green
 *
 *  Algebraic front-end. See pelzl_algebraic.mli for the public surface.
 *)

type span = { start : int; len : int }

type token_kind =
  | T_num of float
  | T_int of string * int
  | T_ident of string
  | T_func of string
  | T_lparen
  | T_rparen
  | T_comma
  | T_assign
  | T_op of char
  | T_error of string

type token = { kind : token_kind; span : span }

type error =
  | E_lex of span * string
  | E_parse of span option * string
  | E_unknown_var of string
  | E_unknown_fun of string
  | E_arity of { fn : string; expected : int; got : int }
  | E_engine of string

let pp_error = function
  | E_lex (_, m) -> "lex: " ^ m
  | E_parse (_, m) -> "parse: " ^ m
  | E_unknown_var v -> "unknown variable: " ^ v
  | E_unknown_fun f -> "unknown function: " ^ f
  | E_arity { fn; expected; got } ->
      Printf.sprintf "%s: expected %d argument%s, got %d"
        fn expected (if expected = 1 then "" else "s") got
  | E_engine m -> m

(* -------------------------------------------------------------------- *)
(* Lexer                                                                *)
(* -------------------------------------------------------------------- *)

let is_digit c = match c with '0'..'9' -> true | _ -> false
let is_alpha c = match c with 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false
let is_alnum c = is_alpha c || is_digit c

let tokenize (s : string) : token list =
  let n = String.length s in
  let mk start len kind = { kind; span = { start; len } } in
  let rec loop i acc =
    if i >= n then List.rev acc
    else
      let c = s.[i] in
      match c with
      | ' ' | '\t' | '\n' | '\r' -> loop (i + 1) acc
      | '+' | '-' | '*' | '/' | '%' | '^' ->
          loop (i + 1) (mk i 1 (T_op c) :: acc)
      | '(' -> loop (i + 1) (mk i 1 T_lparen :: acc)
      | ')' -> loop (i + 1) (mk i 1 T_rparen :: acc)
      | ',' -> loop (i + 1) (mk i 1 T_comma :: acc)
      | '=' -> loop (i + 1) (mk i 1 T_assign :: acc)
      | c when is_digit c || c = '.' ->
          (* number: integer part [. fractional] [e[+-]?digits]
             with optional trailing base suffix b/o/d/h on integers *)
          let start = i in
          let j = ref i in
          while !j < n && is_digit s.[!j] do incr j done;
          let int_end = !j in
          let has_dot = !j < n && s.[!j] = '.' in
          if has_dot then begin
            incr j;
            while !j < n && is_digit s.[!j] do incr j done
          end;
          let has_exp =
            !j < n && (s.[!j] = 'e' || s.[!j] = 'E')
            && (!j + 1 < n
                && (is_digit s.[!j + 1]
                    || s.[!j + 1] = '+' || s.[!j + 1] = '-'))
          in
          if has_exp then begin
            incr j;
            if !j < n && (s.[!j] = '+' || s.[!j] = '-') then incr j;
            while !j < n && is_digit s.[!j] do incr j done
          end;
          (* Base-suffixed integer (only when no dot/exp) *)
          if (not has_dot) && (not has_exp) && !j < n
             && (s.[!j] = 'b' || s.[!j] = 'o' || s.[!j] = 'd' || s.[!j] = 'h')
             && (!j + 1 >= n || not (is_alnum s.[!j + 1]))
          then begin
            let suffix = s.[!j] in
            let base = match suffix with
              | 'b' -> 2 | 'o' -> 8 | 'd' -> 10 | 'h' -> 16 | _ -> 10
            in
            let txt = String.sub s start (int_end - start) in
            incr j;
            loop !j (mk start (!j - start) (T_int (txt, base)) :: acc)
          end else begin
            let txt = String.sub s start (!j - start) in
            let v = try float_of_string txt with _ -> 0.0 in
            loop !j (mk start (!j - start) (T_num v) :: acc)
          end
      | c when is_alpha c ->
          let start = i in
          let j = ref i in
          while !j < n && is_alnum s.[!j] do incr j done;
          let ident = String.sub s start (!j - start) in
          let len_id = String.length ident in
          (* Recognise base-suffixed integer literals that begin with a
             hex digit: e.g. ffh, 1ah, 7o (the all-digit forms are
             handled by the digit branch). The entire identifier must
             match [valid-digits]+ [bodh]. *)
          let is_base_int =
            len_id >= 2 &&
            (let suffix = ident.[len_id - 1] in
             match suffix with
             | 'b' | 'o' | 'd' | 'h' ->
                 let base = match suffix with
                   | 'b' -> 2 | 'o' -> 8 | 'd' -> 10 | 'h' -> 16 | _ -> 10
                 in
                 let body = String.sub ident 0 (len_id - 1) in
                 let valid_digit ch =
                   match base, ch with
                   | 2, ('0' | '1') -> true
                   | 8, ('0'..'7') -> true
                   | 10, ('0'..'9') -> true
                   | 16, ('0'..'9' | 'a'..'f' | 'A'..'F') -> true
                   | _ -> false
                 in
                 String.length body > 0
                 && String.for_all valid_digit body
             | _ -> false)
          in
          if is_base_int then begin
            let base = match ident.[len_id - 1] with
              | 'b' -> 2 | 'o' -> 8 | 'd' -> 10 | 'h' -> 16 | _ -> 10
            in
            let body = String.sub ident 0 (len_id - 1) in
            loop !j (mk start (!j - start) (T_int (body, base)) :: acc)
          end else begin
            (* peek past whitespace; if next non-ws char is '(' it's a function call *)
            let k = ref !j in
            while !k < n && (s.[!k] = ' ' || s.[!k] = '\t') do incr k done;
            let is_call = !k < n && s.[!k] = '(' in
            let kind = if is_call then T_func ident else T_ident ident in
            loop !j (mk start (!j - start) kind :: acc)
          end
      | _ ->
          loop (i + 1)
            (mk i 1 (T_error (Printf.sprintf "unexpected '%c'" c)) :: acc)
  in
  loop 0 []

(* -------------------------------------------------------------------- *)
(* Parser: shunting-yard producing postfix token list, with structured  *)
(* errors and assignment recognition.                                   *)
(* -------------------------------------------------------------------- *)

let precedence = function
  | '^' -> 4
  | '*' | '/' | '%' -> 3
  | '+' | '-' -> 2
  | _ -> 0

let is_left_assoc = function '^' -> false | _ -> true

(* Insert implicit-multiplication tokens (T_op '*') where appropriate *)
let with_implicit_mul (toks : token list) : token list =
  let rec go prev = function
    | [] -> []
    | t :: rest ->
        let need_mul =
          match prev, t.kind with
          | Some prev_kind, kind ->
              let prev_value =
                match prev_kind with
                | T_num _ | T_int _ | T_ident _ | T_rparen -> true
                | _ -> false
              in
              let cur_starts_term =
                match kind with
                | T_num _ | T_int _ | T_ident _ | T_func _ | T_lparen -> true
                | _ -> false
              in
              prev_value && cur_starts_term
          | None, _ -> false
        in
        if need_mul then
          let synth =
            { kind = T_op '*'; span = { start = t.span.start; len = 0 } }
          in
          synth :: t :: go (Some t.kind) rest
        else
          t :: go (Some t.kind) rest
  in
  go None toks

(* Check for lex errors and surface the first one. *)
let first_lex_error toks =
  let rec go = function
    | [] -> None
    | { kind = T_error m; span } :: _ -> Some (E_lex (span, m))
    | _ :: rest -> go rest
  in
  go toks

let to_postfix (toks : token list) : (token list, error) result =
  let exception Pe of error in
  let push_out t out = t :: out in
  let rec pop_until_lparen stack out =
    match stack with
    | [] ->
        raise (Pe (E_parse (None, "mismatched ')'")))
    | { kind = T_lparen; _ } :: rest ->
        (match rest with
         | { kind = T_func _; _ } as f :: rest2 -> (rest2, push_out f out)
         | _ -> (rest, out))
    | t :: rest -> pop_until_lparen rest (push_out t out)
  in
  let rec pop_until_lparen_keep stack out =
    match stack with
    | { kind = T_lparen; _ } :: _ -> (stack, out)
    | t :: rest -> pop_until_lparen_keep rest (push_out t out)
    | [] -> raise (Pe (E_parse (None, "mismatched ','")))
  in
  let rec drain stack out =
    match stack with
    | [] -> out
    | { kind = T_lparen; span } :: _ ->
        raise (Pe (E_parse (Some span, "unclosed '('")))
    | t :: rest -> drain rest (push_out t out)
  in
  let rec loop tokens stack out =
    match tokens with
    | [] -> List.rev (drain stack out)
    | t :: rest ->
        (match t.kind with
         | T_num _ | T_int _ | T_ident _ ->
             loop rest stack (push_out t out)
         | T_func _ ->
             loop rest (t :: stack) out
         | T_lparen ->
             loop rest (t :: stack) out
         | T_comma ->
             let stack', out' = pop_until_lparen_keep stack out in
             loop rest stack' out'
         | T_rparen ->
             let stack', out' = pop_until_lparen stack out in
             loop rest stack' out'
         | T_op o ->
             let p1 = precedence o in
             let rec pop st out =
               match st with
               | { kind = T_op o2; _ } as top :: r ->
                   let p2 = precedence o2 in
                   if (is_left_assoc o && p1 <= p2)
                      || ((not (is_left_assoc o)) && p1 < p2)
                   then pop r (push_out top out)
                   else (st, out)
               | { kind = T_func _; _ } as top :: r ->
                   pop r (push_out top out)
               | _ -> (st, out)
             in
             let stack', out' = pop stack out in
             loop rest (t :: stack') out'
         | T_assign ->
             raise (Pe (E_parse (Some t.span,
                                  "'=' may only follow an identifier at the start")))
         | T_error m ->
             raise (Pe (E_lex (t.span, m))))
  in
  try Ok (loop toks [] []) with Pe e -> Error e

(* -------------------------------------------------------------------- *)
(* Top-level parse                                                      *)
(* -------------------------------------------------------------------- *)

type stmt =
  | S_expr of token list
  | S_assign of string * token list

let parse (s : string) : (stmt, error) result =
  let toks = tokenize s in
  match first_lex_error toks with
  | Some e -> Error e
  | None ->
      let target, body =
        match toks with
        | { kind = T_ident name; _ } :: { kind = T_assign; _ } :: rest ->
            (Some name, rest)
        | _ -> (None, toks)
      in
      let body = with_implicit_mul body in
      (match to_postfix body with
       | Error e -> Error e
       | Ok rpn ->
           (match target with
            | None -> Ok (S_expr rpn)
            | Some n -> Ok (S_assign (n, rpn))))

(* -------------------------------------------------------------------- *)
(* Evaluator                                                            *)
(* -------------------------------------------------------------------- *)

(* Map a 1-arg function name to an engine call. *)
let unary_fn = function
  | "sin" -> Some Pelzl_engine.calc_sin
  | "cos" -> Some Pelzl_engine.calc_cos
  | "tan" -> Some Pelzl_engine.calc_tan
  | "asin" -> Some Pelzl_engine.calc_asin
  | "acos" -> Some Pelzl_engine.calc_acos
  | "atan" -> Some Pelzl_engine.calc_atan
  | "sinh" -> Some Pelzl_engine.calc_sinh
  | "cosh" -> Some Pelzl_engine.calc_cosh
  | "tanh" -> Some Pelzl_engine.calc_tanh
  | "asinh" -> Some Pelzl_engine.calc_asinh
  | "acosh" -> Some Pelzl_engine.calc_acosh
  | "atanh" -> Some Pelzl_engine.calc_atanh
  | "sqrt" -> Some Pelzl_engine.calc_sqrt
  | "sq" -> Some Pelzl_engine.calc_sq
  | "ln" -> Some Pelzl_engine.calc_ln
  | "log" | "log10" -> Some Pelzl_engine.calc_log10
  | "exp" -> Some Pelzl_engine.calc_exp
  | "abs" -> Some Pelzl_engine.calc_abs
  | "arg" -> Some Pelzl_engine.calc_arg
  | "ceil" | "ceiling" -> Some Pelzl_engine.calc_ceiling
  | "floor" -> Some Pelzl_engine.calc_floor
  | "neg" -> Some Pelzl_engine.calc_neg
  | "inv" -> Some Pelzl_engine.calc_inv
  | "re" -> Some Pelzl_engine.calc_re
  | "im" -> Some Pelzl_engine.calc_im
  | "conj" -> Some Pelzl_engine.calc_conj
  | "gamma" -> Some Pelzl_engine.calc_gamma
  | "lngamma" -> Some Pelzl_engine.calc_lngamma
  | "erf" -> Some Pelzl_engine.calc_erf
  | "erfc" -> Some Pelzl_engine.calc_erfc
  | "fact" -> Some Pelzl_engine.calc_fact
  | _ -> None

(* Push an integer literal (parsed in given base) onto the engine stack. *)
let push_int calc text base =
  let bi = Big_int_str.big_int_of_string_base text base in
  { calc with Pelzl_engine.stack =
    Pelzl_engine.stack_push (Pelzl_engine.RpcInt bi) calc.Pelzl_engine.stack }

let push_float calc f =
  { calc with Pelzl_engine.stack =
    Pelzl_engine.stack_push
      (Pelzl_engine.RpcFloatUnit (f, Units.empty_unit)) calc.Pelzl_engine.stack }

let math_pi = 3.14159265358979323846

let builtin_constant name =
  match String.lowercase_ascii name with
  | "pi" ->
      Some (Pelzl_engine.RpcFloatUnit (math_pi, Units.empty_unit))
  | "tau" ->
      Some (Pelzl_engine.RpcFloatUnit (2.0 *. math_pi, Units.empty_unit))
  | "e" ->
      Some (Pelzl_engine.RpcFloatUnit (exp 1.0, Units.empty_unit))
  | "i" ->
      Some
        (Pelzl_engine.RpcComplexUnit
           ({ Complex.re = 0.0; Complex.im = 1.0 }, Units.empty_unit))
  | _ -> None

let push_var_value calc name =
  let vars = Pelzl_engine.get_variables calc in
  match Hashtbl.find_opt vars name with
  | None ->
      (match builtin_constant name with
       | None -> Error (E_unknown_var name)
       | Some v ->
           Ok { calc with Pelzl_engine.stack =
                  Pelzl_engine.stack_push v calc.Pelzl_engine.stack })
  | Some v ->
      Ok { calc with Pelzl_engine.stack =
             Pelzl_engine.stack_push v calc.Pelzl_engine.stack }

let apply_op calc o =
  match o with
  | '+' -> Pelzl_engine.calc_add calc
  | '-' -> Pelzl_engine.calc_sub calc
  | '*' -> Pelzl_engine.calc_mult calc
  | '/' -> Pelzl_engine.calc_div calc
  | '%' -> Pelzl_engine.calc_mod calc
  | '^' -> Pelzl_engine.calc_pow calc
  | _ -> calc

(* Walk a postfix token list, producing a calc_state with a single
   value pushed on the stack (or an error). *)
let eval_postfix (calc0 : Pelzl_engine.calc_state) (rpn : token list)
    : (Pelzl_engine.calc_state, error) result =
  let exception Ee of error in
  let step calc t =
    try
      match t.kind with
      | T_num f -> push_float calc f
      | T_int (txt, base) -> push_int calc txt base
      | T_ident name ->
          (match push_var_value calc name with
           | Ok c -> c
           | Error e -> raise (Ee e))
      | T_op o -> apply_op calc o
      | T_func f ->
          (match unary_fn (String.lowercase_ascii f) with
           | Some fn -> fn calc
           | None -> raise (Ee (E_unknown_fun f)))
      | T_lparen | T_rparen | T_comma | T_assign ->
          raise (Ee (E_parse (Some t.span, "stray token in postfix")))
      | T_error m -> raise (Ee (E_lex (t.span, m)))
    with
    | Pelzl_engine.Stack_error m -> raise (Ee (E_engine m))
    | Invalid_argument m -> raise (Ee (E_engine m))
  in
  try
    let calc = List.fold_left step calc0 rpn in
    if Pelzl_engine.stack_length calc.Pelzl_engine.stack = 0 then
      Error (E_parse (None, "empty expression"))
    else Ok calc
  with Ee e -> Error e

let eval (calc : Pelzl_engine.calc_state) (s : stmt)
    : (Pelzl_engine.calc_state * string, error) result =
  match s with
  | S_expr rpn ->
      (match eval_postfix calc rpn with
       | Error e -> Error e
       | Ok c ->
           let display = Pelzl_engine.get_display_line 1 c in
           Ok (c, display))
  | S_assign (name, rpn) ->
      (match eval_postfix calc rpn with
       | Error e -> Error e
       | Ok c ->
           (* dup, push var name, store -> top retains the value *)
           (try
              let c = Pelzl_engine.cmd_dup c in
              let c =
                { c with Pelzl_engine.stack =
                    Pelzl_engine.stack_push (Pelzl_engine.RpcVariable name)
                      c.Pelzl_engine.stack }
              in
              let c = Pelzl_engine.cmd_store c in
              let display = Pelzl_engine.get_display_line 1 c in
              Ok (c, display)
            with
            | Pelzl_engine.Stack_error m -> Error (E_engine m)
            | Invalid_argument m -> Error (E_engine m)))

let run calc s =
  match parse s with
  | Error e -> Error e
  | Ok stmt -> eval calc stmt
