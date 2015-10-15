open OUnit

let test_parallel () =
  let pid = Unix.getpid () in
  let names = Array.(make 10 ()
  |> mapi (fun i _ -> Printf.sprintf "%d-%d" pid i) |> to_list) in
  List.iter (fun name ->
    Mock.create name []; print_endline name) names;
  List.iter (fun name ->
    assert_bool (name ^ " not in Mock.ls ()") (List.mem name (Mock.ls ()))
  ) names;
  List.iter (fun name -> Mock.remove name) names;
  List.iter (fun name ->
    assert_bool (name ^ " in Mock.ls ()") (not (List.mem name (Mock.ls ())));
  ) names

let suite = "devmapper_mock_parallel" >:::
  [
    "test_parallel" >:: test_parallel;
  ]

let _ = OUnit2.run_test_tt_main (ounit2_of_ounit1 suite)
