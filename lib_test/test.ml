
open OUnit

let _ =
  let tests = TestList [
    Testcamldm.suite;
    Testcamldmmock.suite;
  ] in
  run_test_tt tests

