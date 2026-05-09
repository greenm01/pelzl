open Alcotest
open Pelzl_engine

let test_empty_stack_length () =
  check int "len" 0 (stack_length empty_stack)

let test_push_increases_length () =
  let st1 = stack_push (RpcFloatUnit (1.0, Units.empty_unit)) empty_stack in
  let st2 = stack_push (RpcFloatUnit (2.0, Units.empty_unit)) st1 in
  let st3 = stack_push (RpcFloatUnit (3.0, Units.empty_unit)) st2 in
  check int "len st1" 1 (stack_length st1);
  check int "len st2" 2 (stack_length st2);
  check int "len st3" 3 (stack_length st3)

let test_pop_returns_top () =
  let st =
    stack_push (RpcFloatUnit (3.0, Units.empty_unit))
      (stack_push (RpcFloatUnit (2.0, Units.empty_unit))
         (stack_push (RpcFloatUnit (1.0, Units.empty_unit)) empty_stack))
  in
  let top, s = stack_pop st in
  check int "len after pop" 2 (stack_length s);
  (match top with
   | RpcFloatUnit (f, _) -> check (float 0.0001) "top value" 3.0 f
   | _ -> fail "expected RpcFloatUnit")

let test_peek_nth () =
  let st =
    stack_push (RpcFloatUnit (3.0, Units.empty_unit))
      (stack_push (RpcFloatUnit (2.0, Units.empty_unit))
         (stack_push (RpcFloatUnit (1.0, Units.empty_unit)) empty_stack))
  in
  check (float 0.0001) "peek 1" 3.0 (match stack_peek 1 st with RpcFloatUnit (f, _) -> f | _ -> 0.0);
  check (float 0.0001) "peek 2" 2.0 (match stack_peek 2 st with RpcFloatUnit (f, _) -> f | _ -> 0.0);
  check (float 0.0001) "peek 3" 1.0 (match stack_peek 3 st with RpcFloatUnit (f, _) -> f | _ -> 0.0)

let test_dup () =
  let st2 = stack_push (RpcFloatUnit (2.0, Units.empty_unit))
    (stack_push (RpcFloatUnit (1.0, Units.empty_unit)) empty_stack) in
  let s = stack_dup st2 in
  check int "len after dup" 3 (stack_length s);
  check (float 0.0001) "top after dup" 2.0 (match stack_peek 1 s with RpcFloatUnit (f, _) -> f | _ -> 0.0);
  check (float 0.0001) "second after dup" 2.0 (match stack_peek 2 s with RpcFloatUnit (f, _) -> f | _ -> 0.0)

let test_swap () =
  let st =
    stack_push (RpcFloatUnit (3.0, Units.empty_unit))
      (stack_push (RpcFloatUnit (2.0, Units.empty_unit))
         (stack_push (RpcFloatUnit (1.0, Units.empty_unit)) empty_stack))
  in
  let s = stack_swap st in
  check (float 0.0001) "top after swap" 2.0 (match stack_peek 1 s with RpcFloatUnit (f, _) -> f | _ -> 0.0);
  check (float 0.0001) "second after swap" 3.0 (match stack_peek 2 s with RpcFloatUnit (f, _) -> f | _ -> 0.0)

let test_pop_empty_raises () =
  check_raises "pop empty" (Stack_error "cannot pop empty stack") (fun () -> ignore (stack_pop empty_stack))

let stack_tests = [
  ("empty stack length is 0", `Quick, test_empty_stack_length);
  ("push increases length", `Quick, test_push_increases_length);
  ("pop returns top and decreases length", `Quick, test_pop_returns_top);
  ("peek nth returns correct element", `Quick, test_peek_nth);
  ("dup duplicates top", `Quick, test_dup);
  ("swap exchanges top two", `Quick, test_swap);
  ("pop empty raises", `Quick, test_pop_empty_raises);
]
