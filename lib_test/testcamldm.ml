(*
 * Copyright (C) 2006-2014 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
open Camldm
open Utils

let make_temp_volume () =
  let path = Filename.temp_file Sys.argv.(0) "volume" in
  ignore_string (run "dd" [ "if=/dev/zero"; "of=" ^ path; "seek=1024"; "bs=1M"; "count=1"]);
  finally
    (fun () ->
      ignore_string (run "losetup" [ "-f"; path ]);
      (* /dev/loop0: [fd00]:1973802 (/tmp/SR.createc04251volume) *)
      let line = run "losetup" [ "-j"; path ] in
      try
        let i = String.index line ' ' in
        String.sub line 0 (i - 1)
      with e ->
        error "Failed to parse output of losetup -j: [%s]" line;
        ignore_string (run "losetup" [ "-d"; path ]);
        failwith (Printf.sprintf "Failed to parse output of losetup -j: [%s]" line)
    ) (fun () -> rm_f path)

let remove_temp_volume volume =
  ignore_string (run "losetup" [ "-d"; volume ])

let with_temp_volume f =
  let dev = make_temp_volume () in
  finally (fun () -> f dev) (fun () -> remove_temp_volume dev)

open Devmapper

let name = "testdevmapper"

let create_destroy () =
  with_temp_volume
    (fun device ->
      create name [];
      finally
        (fun () ->
          let all = ls () in
          if not(List.mem name all)
          then failwith (Printf.sprintf "%s not in [ %s ]" name (String.concat "; " all))
        ) (fun () -> remove name)
    )

let constant_sector c =
  let sector = Cstruct.create 512 in
  for i = 0 to Cstruct.len sector - 1 do
    Cstruct.set_uint8 sector i c
  done;
  sector

let read_sector path sector =
  let open Lwt in
  let offset = Int64.mul sector 512L in
  let buf = constant_sector 9 in
  let t =
    Lwt_unix.openfile path [ Lwt_unix.O_RDONLY ] 0
    >>= fun fd ->
    Lwt_unix.LargeFile.lseek fd offset Unix.SEEK_SET
    >>= fun _ ->
    Lwt_cstruct.complete (Lwt_cstruct.read fd) buf
    >>= fun () ->
    return buf in
  Lwt_main.run t

let write_sector path sector buf =
  let open Lwt in
  let offset = Int64.mul sector 512L in
  let t =
    Lwt_unix.openfile path [ Lwt_unix.O_WRONLY ] 0
    >>= fun fd ->
    Lwt_unix.LargeFile.lseek fd offset Unix.SEEK_SET
    >>= fun _ ->
    Lwt_cstruct.complete (Lwt_cstruct.write fd) buf in
  Lwt_main.run t

let cstruct_equal a b =
  let check_contents a b =
    try
      for i = 0 to Cstruct.len a - 1 do
        let a' = Cstruct.get_uint8 a i in
        let b' = Cstruct.get_uint8 b i in
        if a' <> b' then failwith (Printf.sprintf "buffers differ at %d: %d <> %d" i a' b')
      done;
      true
    with _ -> false in
  (Cstruct.len a = (Cstruct.len b)) && (check_contents a b)

let write_read () =
  with_temp_volume
    (fun device ->
      let device = Linear.Path device in
      let targets = [
        Target.({ start = 0L; size = 1L; kind = Linear Linear.({device; offset = 1L}) });
        Target.({ start = 1L; size = 1L; kind = Linear Linear.({device; offset = 0L}) })
      ] in
      create name targets;
      finally
        (fun () ->
          let all = ls () in
          if not(List.mem name all)
          then failwith (Printf.sprintf "%s not in [ %s ]" name (String.concat "; " all));
          (* write to the real device, read via device mapper *)

        ) (fun () -> remove name)
    )

(*
(* This test nolonger compiles *)
let _ =
  let name = Sys.argv.(1) in
  let start = Int64.of_string Sys.argv.(2) in
  let len = Int64.of_string Sys.argv.(3) in
  let dev = Sys.argv.(4) in
  let offset = Int64.of_string Sys.argv.(5) in
  let dev2 = Sys.argv.(6) in
  let offset2 = Int64.of_string Sys.argv.(7) in

  let buf = String.create 512 in

  Camldm.create name [| { start=start; 
			  len=len; 
			  map = Striped {chunk_size=8L; 
					 dests=[| {device=dev;offset=offset}; 
						  {device=dev2;offset=offset2} |] } } |];

  let s = Camldm.table name in
  let (major,minor) = s.major,s.minor in
  let nod = "/tmp/foobar" in
  Camldm.mknod nod 0o644 (Int32.to_int major) (Int32.to_int minor);
  let ifd = Unix.openfile nod [Unix.O_RDONLY] 0o000 in
  Printf.printf "Status:\nexists: %b\nsuspended: %b\nlive_table: %b\ninactive_table: %b\n" s.exists s.suspended s.live_table s.inactive_table;
  Printf.printf "open_count: %ld\nevent_nr: %ld\nmajor: %ld\nminor: %ld\n"
    s.open_count s.event_nr s.major s.minor;
  Printf.printf "read_only: %b\n" s.read_only;
  Printf.printf "\nTable:\n";
  List.iter (fun (s,l,t,p) -> Printf.printf " %Ld %Ld %s %s\n" s l t p) s.targets;
  let input = Unix.read ifd buf 0 10 in
  Printf.printf "input=%d\n" input;
  for i=0 to 2 do 
    Printf.printf "%d " (int_of_char buf.[i])
  done;
  let name=Printf.sprintf "/sys/block/dm-%ld/dev" minor in
  Printf.printf "Got minor=%ld - looking for: %s\n" minor name;
  let fd = Unix.openfile name [Unix.O_RDONLY] 0o000 in
  let input = Unix.read fd buf 0 10 in
  Printf.printf "input=%d\n" input;
  for i=0 to 2 do 
    Printf.printf "%d " (int_of_char buf.[i])
  done;
  Printf.printf "\n";
  Unix.close fd


  (*List.iter (fun (a,b,c,d,e,f) -> Printf.printf "%d %d %Ld %Ld %s %s" a b c d e f) l; *)
(*  Camldm.remove name*)
*)
open OUnit

let finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    g ();
    raise e

let with_task kind f =
  let open Devmapper.Lowlevel in
  match dm_task_create DM_DEVICE_CREATE with
  | None ->
    failwith "dm_task_create returned NULL: check permissions?"
  | Some dmt ->
    finally
      (fun () -> f dmt)
      (fun () -> dm_task_destroy dmt)

let set_name () =
  let open Devmapper.Lowlevel in
  with_task DM_DEVICE_CREATE
    (fun dmt ->
      if not (dm_task_set_name dmt "hello")
      then failwith "dm_task_set_name";
      if not (dm_task_set_uuid dmt "there")
      then failwith "dm_task_set_uuid";
    )

let create_destroy () = with_task Devmapper.Lowlevel.DM_DEVICE_CREATE (fun _ -> ())

let ls () =
  let (_: string list) = Devmapper.ls () in
  ()

let _ =
  let suite = "devicemapper" >:::
    [
      "create_destroy" >:: create_destroy;
      "set name and uuid" >:: set_name;
      "ls" >:: ls;
      "create_destroy" >:: create_destroy;
      "write_read" >:: write_read;
    ] in
  run_test_tt suite
    
