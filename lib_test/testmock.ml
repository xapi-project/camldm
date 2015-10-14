
open OUnit

open Devmapper

let make_location path offset = {
  Location.
  device = Location.Path path;
  offset = Int64.of_int offset;
}

let make_linear start size path offset = {
  Target.
  start = Int64.of_int start;
  size = Int64.of_int size;
  kind = Target.Linear (make_location path offset);
}

let make_striped start size stripe_size stripes = {
  Target.
  start = Int64.of_int start;
  size = Int64.of_int size;
  kind = Target.Striped {
    Striped.size = Int64.of_int stripe_size;
    Striped.stripes = Array.of_list stripes;
  };
}

let test_overlap () =
  let with_overlap ?(overlaps=1) targets =
    Mock.clear ();
    assert_raises (Failure (Printf.sprintf "%d overlap(s) detected" overlaps)) (fun () -> Mock.create "device" targets)
  in
  let without_overlap targets =
    Mock.clear ();
    Mock.create "name" targets
  in

  (* no target at all *)
  without_overlap [];

  (* one linear target *)
  without_overlap [make_linear 0 1024 "dev1" 0];

  (* one striped target *)
  without_overlap [make_striped 0 1024 32 [make_location "dev1" 0; make_location "dev2" 0]];
  without_overlap [make_striped 0 1024 32 [make_location "dev1" 0; make_location "dev1" 512]];
  with_overlap    [make_striped 0 1024 32 [make_location "dev1" 0; make_location "dev1" 0]];
  with_overlap    [make_striped 0 1024 32 [make_location "dev1" 0; make_location "dev1" 511]];

  (* two linear targets *)
  without_overlap [make_linear 0 1024 "dev1" 0; make_linear 1024 1024 "dev1" 1024];
  without_overlap [make_linear 0 1024 "dev1" 0; make_linear 1024 1024 "dev2" 0];
  with_overlap    [make_linear 0 1024 "dev1" 0; make_linear 1024 1024 "dev1" 0];
  with_overlap    [make_linear 0 1024 "dev1" 0; make_linear 0 1024 "dev1" 1024];

  (* two striped targets *)
  without_overlap [
    make_striped 0 1024 32 [make_location "dev1" 0; make_location "dev2" 0];
    make_striped 1024 1024 32 [make_location "dev1" 512; make_location "dev2" 512];
  ];
  without_overlap [
    make_striped 0 1024 32 [make_location "dev1" 0; make_location "dev2" 512];
    make_striped 1024 1024 32 [make_location "dev1" 512; make_location "dev2" 0];
  ];
  with_overlap [
    make_striped 0 1024 32 [make_location "dev1" 0; make_location "dev2" 512];
    make_striped 1024 1024 32 [make_location "dev1" 0; make_location "dev3" 0];
  ];

  (* clear up *)
  Mock.clear ()

let test_reload () =
  Mock.clear ();

  Mock.create "dev" [];
  begin
    match Mock.stat "dev" with
    | None -> failwith "Mock.stat yield None"
    | Some info -> assert_equal [] info.Mock.targets
  end;

  let new_target = make_linear 0 1024 "dev1" 0 in
  Mock.suspend "dev";
  Mock.reload "dev" [new_target];
  Mock.resume "dev";
  begin
    match Mock.stat "dev" with
    | None -> failwith "Mock.stat yield None"
    | Some info ->
        let targets = info.Mock.targets in
        assert_equal 1 (List.length targets);
        let target = List.hd targets in
        assert_equal new_target target
  end;

  (* clear up *)
  Mock.clear ()

let test_create_remove () =
  Mock.clear ();

  Mock.create "dev" [];
  assert_raises (Failure "dev already exists") (fun () -> Mock.create "dev" []);
  assert_equal ["dev"] (Mock.ls ());

  Mock.remove "dev";
  assert_equal [] (Mock.ls ());
  assert_raises (Failure "dev does not exist") (fun () -> Mock.remove "dev");

  (* clear up *)
  Mock.clear ()

let test_load_save () =
  Mock.clear ();

  let dev1_targets = [] in
  let dev2_targets = [
    make_linear 0 1024 "dev21" 0;
    make_linear 1024 1024 "dev22" 0;
  ] in
  let dev3_targets = [
    make_striped 0 1024 32 [make_location "dev31" 0; make_location "dev32" 512];
    make_striped 1024 1024 32 [make_location "dev33" 0; make_location "dev34" 0];
  ] in
  Mock.create "dev1" dev1_targets;
  Mock.create "dev2" dev2_targets;
  Mock.create "dev3" dev3_targets;

  let temp_file = Filename.temp_file "dm-mock" "" in
  Mock.save_file temp_file;
  Mock.load_file temp_file;

  assert_equal 3 (List.length (Mock.ls ()));
  assert_equal ["dev1";"dev2";"dev3"] (List.sort compare (Mock.ls ()));

  let assert_targets_equal targets dev =
    match Mock.stat dev with
    | None -> failwith "Mock.stat yield None"
    | Some info ->
        assert_equal targets info.Mock.targets
  in
  assert_targets_equal dev1_targets "dev1";
  assert_targets_equal dev2_targets "dev2";
  assert_targets_equal dev3_targets "dev3";

  (* clear up *)
  Unix.unlink temp_file;
  Mock.clear ()

let test_real_mock () =
  let select which : (module Devmapper.S.DEVMAPPER) =
    if which then
      (module Devmapper.Linux)
    else
      (module Devmapper.Mock)
  in
  let dm = select false in
  let module DM = (val dm : Devmapper.S.DEVMAPPER) in
  ignore (DM.ls ())

let test_thread_safe () =
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


let suite = "devmapper_mock" >:::
  [
    "test_overlap" >:: test_overlap;
    "test_reload" >:: test_reload;
    "test_create_remove" >:: test_create_remove;
    "test_load_save" >:: test_load_save;
    "test_real_mock" >:: test_real_mock;
    "test_thread_safe" >:: test_thread_safe;
  ]
