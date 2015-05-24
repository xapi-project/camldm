(*
 * Copyright (C) 2015 Citrix Systems Inc.
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

open Sexplib.Std

type info = {
  suspended: bool;
  live_table: int;
  inactive_table: int;
  open_count: int32;
  event_nr: int32;
  major: int32;
  minor: int32;
  read_only: bool;
  target_count: int32;
  deferred_remove: int;
  targets: Target.t list;
} with sexp

type device = string with sexp

module Device = struct
  type t = {
    name: device;
    major: int;
    minor: int;
    mutable targets: Target.t list;
    mutable reload_targets: Target.t list option;
    mutable suspended: bool;
  } with sexp

  let create (name : device) (major : int) (minor : int) (targets : Target.t list) : t =
    {
      name = name;
      major = major;
      minor = minor;
      targets = targets;
      reload_targets = None;
      suspended = false;
    }

  let reload (dev : t) (targets : Target.t list) : unit =
    dev.reload_targets <- Some targets

  let suspend (dev : t) : unit =
    (* no need to check if the device is already suspended,
     * `dmsetup suspend` is idempotent
     *)
    dev.suspended <- true

  let resume (dev : t) : unit =
    if dev.suspended then
      begin
        dev.suspended <- false;
        match dev.reload_targets with
        | None -> ()
        | Some targets ->
            dev.targets <- targets
      end

  let mknod (dev : t) (path : string) (mode : int) : unit =
    ()

  let stat (dev : t) : info =
    {
      suspended = dev.suspended;
      live_table = 0;
      inactive_table = 0;
      open_count = 0l;
      event_nr = 0l;
      major = Int32.of_int dev.major;
      minor = Int32.of_int dev.minor;
      read_only = false;
      target_count = Int32.of_int (List.length dev.targets);
      deferred_remove = 0;
      targets = dev.targets;
    }
end

module DeviceSet = struct
  type t = (device, Device.t) Hashtbl.t with sexp

  let make () : t =
    Hashtbl.create 10

  let find_next_available_major_minor (devices : t) : (int * int) =
    let major = 252 in (* FIXME: is this major number okay? *)
    let min_unused_minor =
      let minors_in_use = Array.make 256 false in
      let _ = Hashtbl.iter (fun _ dev ->
        if dev.Device.minor >= 0 && dev.Device.minor < 256 then
          minors_in_use.(dev.Device.minor) <- true
      ) devices in
      let min_unused_minor = ref None in
      Array.iteri (fun idx used ->
        if not used then
          match !min_unused_minor with
          | None -> min_unused_minor := Some idx
          | _ -> ()
      ) minors_in_use;
      !min_unused_minor
    in
    match min_unused_minor with
    | None -> failwith (Printf.sprintf "too many devices (%d)" (Hashtbl.length devices))
    | Some minor -> (major, minor)

  let validate_targets (targets : Target.t list) =
    let check_overlap (spans : (int64 * int64) list) =
      let sorted_spans = List.sort (fun (start_a, _) (start_b, _) -> compare start_a start_b) spans in
      let overlaps = ref 0 in
      let last_end = ref 0L in
      List.iter (fun (start, size) ->
        if start < !last_end then overlaps := !overlaps + 1;
        last_end := Int64.add start size
      ) sorted_spans;
      if !overlaps > 0 then failwith (Printf.sprintf "%d overlap(s) detected" !overlaps)
    in

    (* 1. check the overlap in the logic device *)
    check_overlap (List.map (fun target -> (target.Target.start, target.Target.size)) targets);

    (* 2. check the overlap in the source device *)
    let locations = List.flatten (List.map (fun target ->
      match target.Target.kind with
      | Target.Linear linear -> [linear.Location.device, linear.Location.offset, target.Target.size]
      | Target.Striped striped ->
          let nb_stripes = Array.length striped.Striped.stripes in
          let size_per_stripe = Int64.div target.Target.size (Int64.of_int nb_stripes) in (* FIXME: does all stripes have the same size? *)
          List.mapi (fun idx stripe ->
            (stripe.Location.device, stripe.Location.offset, size_per_stripe)
          ) (Array.to_list striped.Striped.stripes)
      | _ -> []
    ) targets) in

    let device_to_spans = Hashtbl.create 0 in
    List.iter (fun (dev, start, size) ->
      Hashtbl.replace device_to_spans dev ((start, size) ::
        if Hashtbl.mem device_to_spans dev then
          Hashtbl.find device_to_spans dev
        else
          []
      )
    ) locations;
    Hashtbl.iter (fun dev spans ->
      check_overlap spans
    ) device_to_spans

  let create (devices : t) (name : device) (targets : Target.t list) : unit =
    if Hashtbl.mem devices name
    then failwith (Printf.sprintf "%s already exists" name);
    validate_targets targets;

    let major, minor = find_next_available_major_minor devices in
    let dev = Device.create name major minor targets in
    Hashtbl.add devices name dev

  let remove (devices : t) (name : device) : unit =
    if Hashtbl.mem devices name
    then Hashtbl.remove devices name
    else failwith (Printf.sprintf "%s does not exist" name)

  let reload (devices : t) (name : device) (targets : Target.t list) : unit =
    if not (Hashtbl.mem devices name)
    then failwith (Printf.sprintf "%s does not exist" name);
    validate_targets targets;

    let dev = Hashtbl.find devices name in
    Device.reload dev targets

  let suspend (devices : t) (name : device) : unit =
    if not (Hashtbl.mem devices name)
    then failwith (Printf.sprintf "%s does not exist" name);

    let dev = Hashtbl.find devices name in
    Device.suspend dev

  let resume (devices : t) (name : device) : unit =
    if not (Hashtbl.mem devices name)
    then failwith (Printf.sprintf "%s does not exist" name);

    let dev = Hashtbl.find devices name in
    Device.resume dev

  let mknod (devices : t) (name : device) (path : string) (mode : int) : unit =
    if not (Hashtbl.mem devices name)
    then failwith (Printf.sprintf "%s does not exist" name);

    let dev = Hashtbl.find devices name in
    Device.mknod dev path mode

  let stat (devices : t) (name : device) : info option =
    if (Hashtbl.mem devices name)
    then
      let dev = Hashtbl.find devices name in
      Some (Device.stat dev)
    else
      None

  let ls (devices : t) () : device list =
    Hashtbl.fold (fun name dev acc -> name :: acc) devices []

  let clear (devices : t) () : unit =
    Hashtbl.clear devices

  let load_file (path : string) : t =
    t_of_sexp (Sexplib.Sexp.load_sexp path)

  let save_file (devices : t) (path : string) : unit =
    Sexplib.Sexp.save_hum path (sexp_of_t devices)
end

let persistent_fn =
  (* locate the persistent file from the working directory by default *)
  let cwd = Sys.getcwd () in
  let fn = Filename.concat cwd "dm-mock" in
  ref fn

let devices =
  (* load the tables from disk when starting up, or make an empty table if
   * the persistent file does not exist or is corrupted.
   *)
  try
    ref (DeviceSet.load_file !persistent_fn)
  with _ ->
    ref (DeviceSet.make ())

let create device targets =
  DeviceSet.create !devices device targets;
  DeviceSet.save_file !devices !persistent_fn

let remove device =
  DeviceSet.remove !devices device;
  DeviceSet.save_file !devices !persistent_fn

let reload device targets =
  DeviceSet.reload !devices device targets;
  DeviceSet.save_file !devices !persistent_fn

let suspend device =
  DeviceSet.suspend !devices device;
  DeviceSet.save_file !devices !persistent_fn

let resume device =
  DeviceSet.resume !devices device;
  DeviceSet.save_file !devices !persistent_fn

let mknod device path mode =
  DeviceSet.mknod !devices device path mode;
  DeviceSet.save_file !devices !persistent_fn

let stat device =
  let ret = DeviceSet.stat !devices device in
  DeviceSet.save_file !devices !persistent_fn;
  ret

let ls () =
  let ret = DeviceSet.ls !devices () in
  DeviceSet.save_file !devices !persistent_fn;
  ret

let clear () =
  DeviceSet.clear !devices ();
  DeviceSet.save_file !devices !persistent_fn

let get_persistent () =
  !persistent_fn

let set_persistent filename =
  persistent_fn := filename

let save_file path =
  DeviceSet.save_file !devices path

let load_file path =
  devices := DeviceSet.load_file path

