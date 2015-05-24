
open OUnit

let _ =
  let tests = TestList [
    Testlinux.suite;
    Testmock.suite;
  ] in
  run_test_tt tests

