open OUnit

let test_parallel () =
  let pid = Unix.getpid () in
  let exns = Hashtbl.create 10 in
  let exns_m = Mutex.create () in
  let th tid =
    try
      let names = Array.(make 10 ()
      |> mapi (fun i _ -> Printf.sprintf "%d-%d-%d" pid tid i) |> to_list) in
      List.iter (fun name ->
        Mock.create name []; print_endline name) names;
      List.iter (fun name ->
        assert_bool (name ^ " not in Mock.ls ()") (List.mem name (Mock.ls ()))
      ) names;
      Thread.delay 0.1;
      List.iter (fun name -> Mock.remove name) names;
      List.iter (fun name ->
        assert_bool (name ^ " in Mock.ls ()") (not (List.mem name (Mock.ls ())));
      ) names
    with exn ->
      Mutex.lock exns_m;
      Hashtbl.add exns tid exn;
      Mutex.unlock exns_m in
  let thread_ids = Array.(make 2 () |> mapi (fun i _ -> i) |> to_list) in
  let threads = List.map (fun tid -> Thread.create th tid) thread_ids in
  List.iter (fun t -> Thread.join t) threads;
  if Hashtbl.length exns <> 0 then begin
    Printf.printf "Some of the threads failed:\n";
    Hashtbl.iter (fun k v ->
      Printf.printf "\tProcess %d, thread %d failed with %s\n"
        pid k (Printexc.to_string v)
    ) exns;
    exit 1
  end

let suite = "devmapper_mock_parallel" >:::
  [
    "test_parallel" >:: test_parallel;
  ]

let _ = OUnit2.run_test_tt_main (ounit2_of_ounit1 suite)
