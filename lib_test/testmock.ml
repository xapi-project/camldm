
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

let test_separate_paths () =
  Mock.set_path "dm-mock-1";
  Mock.clear ();
  Mock.create "dev1" [];
  Mock.set_path "dm-mock-2";
  Mock.clear ();
  Mock.create "dev2" [];
  Mock.set_path "dm-mock-1";
  assert_equal ["dev1"] (Mock.ls ());
  Mock.clear ();
  Mock.set_path "dm-mock-2";
  assert_equal ["dev2"] (Mock.ls ());
  Mock.clear ()

let suite = "devmapper_mock" >:::
  [
    "test_overlap" >:: test_overlap;
    "test_reload" >:: test_reload;
    "test_create_remove" >:: test_create_remove;
    "test_real_mock" >:: test_real_mock;
    "test_separate_paths" >:: test_separate_paths;
  ]
