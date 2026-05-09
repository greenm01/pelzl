(* Pelzl test runner *)

let () =
  Alcotest.run "pelzl"
    [
      ("stack", Test_stack.stack_tests);
      ("engine", Test_engine.engine_tests);
      ("bindings", Test_bindings.binding_tests);
      ("ui", Test_ui_model.ui_tests);
      ("algebraic", Test_algebraic.algebraic_tests);
    ]
