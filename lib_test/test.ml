
open OUnit

let _ =
  let tests = TestList [
    Testlinux.suite;
    Testmock.suite;
  ] in
  OUnit2.run_test_tt_main (ounit2_of_ounit1 tests)
