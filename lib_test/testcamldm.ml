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
open Devmapper
open Utils

let with_temp_file f =
  let path = Filename.temp_file (Filename.basename Sys.argv.(0)) "volume" in
  ignore_string (run "dd" [ "if=/dev/zero"; "of=" ^ path; "seek=1024"; "bs=1M"; "count=1"]);
  finally (fun () -> f path) (fun () -> rm_f path)

let with_temp_volume path f =
  let dev =
    ignore_string (run "losetup" [ "-f"; path ]);
    (* /dev/loop0: [fd00]:1973802 (/tmp/SR.createc04251volume) *)
    let line = run "losetup" [ "-j"; path ] in
    try
      let i = String.index line ' ' in
      String.sub line 0 (i - 1)
    with e ->
      error "Failed to parse output of losetup -j: [%s]" line;
      ignore_string (run "losetup" [ "-d"; path ]);
      failwith (Printf.sprintf "Failed to parse output of losetup -j: [%s]" line) in
  finally
    (fun () -> f dev)
    (fun () ->
      ignore_string (run "losetup" [ "-d"; dev ])
    )

open Devmapper

let name = "testdevmapper"
let dev_mapper_path = "/tmp/" ^ name
let create_dev_mapper_path () =
  (try Unix.unlink dev_mapper_path with _ -> ());
  Devmapper.mknod name dev_mapper_path 0o0644

let create_destroy () =
  with_temp_file
    (fun tmp ->
      with_temp_volume tmp
        (fun device ->
          create name [];
          finally
            (fun () ->
              let all = ls () in
              if not(List.mem name all)
              then failwith (Printf.sprintf "%s not in [ %s ]" name (String.concat "; " all))
            ) (fun () -> remove name)
        )
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
  with_temp_file
    (fun tmp ->
      (* Write data to the underlying file first because
         it won't be coherent after the devmapper device is created on
         top. *)
      let ones = constant_sector 1 in
      let twos = constant_sector 2 in
      write_sector tmp 0L ones;
      write_sector tmp 1L twos;

      with_temp_volume tmp
        (fun path ->
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
              create_dev_mapper_path ();
              let at_zero = read_sector dev_mapper_path 0L in
              let at_one = read_sector dev_mapper_path 1L in
              cstruct_equal ones at_one;
              cstruct_equal twos at_zero
            ) (fun () -> remove name)
        )
    )

let write_read_reload () =
  with_temp_file
    (fun tmp ->
      (* Write data to the underlying file first because
         it won't be coherent after the devmapper device is created on
         top. *)
      let ones = constant_sector 1 in
      let twos = constant_sector 2 in
      write_sector tmp 0L ones;
      write_sector tmp 1L twos;

      with_temp_volume tmp
        (fun path ->
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
              create_dev_mapper_path ();
              let at_zero = read_sector dev_mapper_path 0L in
              let at_one = read_sector dev_mapper_path 1L in
              cstruct_equal ones at_one;
              cstruct_equal twos at_zero;
              let targets = [
                (* The first two are flipped around *)
                Target.({ start = 0L; size = 1L; kind = Linear Location.({device; offset = 0L}) });
                Target.({ start = 1L; size = 1L; kind = Linear Location.({device; offset = 1L}) });
                (* This new target has been added, extending the device *)
                Target.({ start = 2L; size = 1L; kind = Linear Location.({device; offset = 0L}) })
              ] in
              reload name targets;
              suspend name;
              resume name;
              let at_zero = read_sector dev_mapper_path 0L in
              let at_one = read_sector dev_mapper_path 1L in
              let at_two = read_sector dev_mapper_path 2L in
              cstruct_equal ones at_zero;
              cstruct_equal twos at_one;
              cstruct_equal ones at_two;

            ) (fun () -> remove name)
        )
    )
let write_read_striped () =
  with_temp_file
    (fun tmp1 ->
      let ones = constant_sector 1 in
      write_sector tmp1 0L ones;
      with_temp_file
        (fun tmp2 ->
          let twos = constant_sector 2 in
          write_sector tmp2 0L twos;

          with_temp_volume tmp1
            (fun path1 ->
              with_temp_volume tmp2
                (fun path2 ->

                  let device1 = Location.Path path1 in
                  let device2 = Location.Path path2 in
                  let stripes = Location.([| { device = device1; offset = 0L }; { device = device2; offset = 0L } |]) in
                  let targets = [
                    Target.({ start=0L; size = 16L; kind = Striped Striped.({size = 8L; stripes; }) })
                  ] in
                  create name targets;
                  finally
                    (fun () ->
                      create_dev_mapper_path ();
                      let at_zero = read_sector dev_mapper_path 0L in
                      let at_eight = read_sector dev_mapper_path 8L in
                      cstruct_equal ones at_zero;
                      cstruct_equal twos at_eight;
                    ) (fun () -> remove name)
                )
            )
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

let ls () =
  let (_: string list) = Devmapper.ls () in
  ()

let stat_none () =
  match stat "namethatdoesnotexist" with
  | None -> ()
  | Some _ -> failwith "stat_none: got Some rather than None"

let _ =
  (* Clean up leftovers from previous runs *)
  (try Devmapper.remove name with _ -> ())

let suite = "devicemapper" >:::
  [
    "ls" >:: ls;
    "create_destroy" >:: create_destroy;
    "write_read" >:: write_read;
    "write_read_reload" >:: write_read_reload;
    "write_read_striped" >:: write_read_striped;
    "stat_none" >:: stat_none;
  ]

