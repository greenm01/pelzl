(* Pelzl test runner *)

let () =
  Alcotest.run "pelzl"
    [
      ("stack", Test_stack.stack_tests);
      ("engine", Test_engine.engine_tests);
      ("engine_parity", Test_engine_parity.parity_tests);
      ("engine_regressions", Test_engine_regressions.regression_tests);
      ("engine_improvements", Test_engine_improvements.improvement_tests);
      ("bindings", Test_bindings.binding_tests);
      ("ui", Test_ui_model.ui_tests);
      ("algebraic", Test_algebraic.algebraic_tests);
      ("repl_history", Test_repl_history.repl_history_tests);
    ]
