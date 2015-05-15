
open OUnit

module DM = Devmapper_mock

let make_location path offset = {
  DM.Location.
  device = DM.Location.Path path;
  offset = Int64.of_int offset;
}

let make_linear start size path offset = {
  DM.Target.
  start = Int64.of_int start;
  size = Int64.of_int size;
  kind = DM.Target.Linear (make_location path offset);
}

let make_striped start size stripe_size stripes = {
  DM.Target.
  start = Int64.of_int start;
  size = Int64.of_int size;
  kind = DM.Target.Striped {
    DM.Striped.size = Int64.of_int stripe_size;
    DM.Striped.stripes = Array.of_list stripes;
  };
}

let test_overlap () =
  let with_overlap ?(overlaps=1) targets =
    DM.clear ();
    assert_raises (Failure (Printf.sprintf "%d overlap(s) detected" overlaps)) (fun () -> DM.create "device" targets)
  in
  let without_overlap targets =
    DM.clear ();
    DM.create "name" targets
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
  DM.clear ()

let test_reload () =
  DM.clear ();

  DM.create "dev" [];
  begin
    match DM.stat "dev" with
    | None -> failwith "DM.stat yield None"
    | Some info -> assert_equal [] info.DM.targets
  end;

  let new_target = make_linear 0 1024 "dev1" 0 in
  DM.suspend "dev";
  DM.reload "dev" [new_target];
  DM.resume "dev";
  begin
    match DM.stat "dev" with
    | None -> failwith "DM.stat yield None"
    | Some info ->
        let targets = info.DM.targets in
        assert_equal 1 (List.length targets);
        let target = List.hd targets in
        assert_equal new_target target
  end;

  (* clear up *)
  DM.clear ()

let test_create_remove () =
  DM.clear ();

  DM.create "dev" [];
  assert_raises (Failure "dev already exists") (fun () -> DM.create "dev" []);
  assert_equal ["dev"] (DM.ls ());

  DM.remove "dev";
  assert_equal [] (DM.ls ());
  assert_raises (Failure "dev does not exist") (fun () -> DM.remove "dev");

  (* clear up *)
  DM.clear ()

let test_load_save () =
  DM.clear ();

  let dev1_targets = [] in
  let dev2_targets = [
    make_linear 0 1024 "dev21" 0;
    make_linear 1024 1024 "dev22" 0;
  ] in
  let dev3_targets = [
    make_striped 0 1024 32 [make_location "dev31" 0; make_location "dev32" 512];
    make_striped 1024 1024 32 [make_location "dev33" 0; make_location "dev34" 0];
  ] in
  DM.create "dev1" dev1_targets;
  DM.create "dev2" dev2_targets;
  DM.create "dev3" dev3_targets;

  let temp_file = Filename.temp_file "dm-mock" "" in
  DM.save_file temp_file;
  DM.load_file temp_file;

  assert_equal 3 (List.length (DM.ls ()));
  assert_equal ["dev1";"dev2";"dev3"] (List.sort compare (DM.ls ()));

  let assert_targets_equal targets dev =
    match DM.stat dev with
    | None -> failwith "DM.stat yield None"
    | Some info ->
        assert_equal targets info.DM.targets
  in
  assert_targets_equal dev1_targets "dev1";
  assert_targets_equal dev2_targets "dev2";
  assert_targets_equal dev3_targets "dev3";

  (* clear up *)
  Unix.unlink temp_file;
  DM.clear ()

let suite = "devmapper_mock" >:::
  [
    "test_overlap" >:: test_overlap;
    "test_reload" >:: test_reload;
    "test_create_remove" >:: test_create_remove;
    "test_load_save" >:: test_load_save;
  ]

