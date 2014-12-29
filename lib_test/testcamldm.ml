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
let dev_mapper_path = "/dev/mapper/" ^ name

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
    Lwt_unix.close fd
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
    Lwt_cstruct.complete (Lwt_cstruct.write fd) buf
    >>= fun () ->
    Lwt_unix.close fd in
  Lwt_main.run t

let cstruct_equal a b =
  let check_contents a b =
    for i = 0 to Cstruct.len a - 1 do
      let a' = Cstruct.get_uint8 a i in
      let b' = Cstruct.get_uint8 b i in
      if a' <> b' then failwith (Printf.sprintf "buffers differ at %d: %d <> %d" i a' b')
    done in
  if Cstruct.len a <> (Cstruct.len b)
  then failwith "buffers have different lengths";
  check_contents a b

let write_read () =
  with_temp_volume
    (fun path ->
      (* Write data to the underlying loop device first because
         it won't be coherent after the devmapper device is created on
         top. *)

      let ones = constant_sector 1 in
      let twos = constant_sector 2 in
      write_sector path 0L ones;
      write_sector path 1L twos;

      let device = Location.Path path in
      let targets = [
        Target.({ start = 0L; size = 1L; kind = Linear Location.({device; offset = 1L}) });
        Target.({ start = 1L; size = 1L; kind = Linear Location.({device; offset = 0L}) })
      ] in
      create name targets;
      finally
        (fun () ->
          let all = ls () in
          if not(List.mem name all)
          then failwith (Printf.sprintf "%s not in [ %s ]" name (String.concat "; " all));
          (* read via device mapper, expect the first two sectors to be
             permuted (see targets above) *)
          let at_zero = read_sector dev_mapper_path 0L in
          let at_one = read_sector dev_mapper_path 1L in
          cstruct_equal ones at_one;
          cstruct_equal twos at_zero
        ) (fun () -> remove name)
    )

let write_read_striped () =
  with_temp_volume
    (fun path1 ->
      let ones = constant_sector 1 in
      write_sector path1 0L ones;
      with_temp_volume
        (fun path2 ->
          let twos = constant_sector 2 in
          write_sector path2 0L twos;

          let device1 = Location.Path path1 in
          let device2 = Location.Path path2 in
          let stripes = Location.([| { device = device1; offset = 0L }; { device = device2; offset = 0L } |]) in
          let targets = [
            Target.({ start=0L; size = 16L; kind = Striped Striped.({size = 8L; stripes; }) })
          ] in
          create name targets;
          finally
             (fun () ->
               let at_zero = read_sector dev_mapper_path 0L in
               let at_eight = read_sector dev_mapper_path 8L in
               cstruct_equal ones at_zero;
               cstruct_equal twos at_eight;
             ) (fun () -> remove name)
        )
    )

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
      "write_read_striped" >:: write_read_striped;
    ] in
  run_test_tt suite
    
