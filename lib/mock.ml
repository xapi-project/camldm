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
let persistent_fn_m = Mutex.create ()

let devices = ref (DeviceSet.make ())

let file_locks : (string, Mutex.t) Hashtbl.t = Hashtbl.create 10
let file_locks_m = Mutex.create ()

let finally f g = let r = begin try f () with e -> g (); raise e end in g (); r

let with_mutex m f =
  Mutex.lock m;
  finally f (fun () -> Mutex.unlock m)

let with_lockf path f =
  let lock_fd = Unix.(openfile path [O_CREAT; O_TRUNC; O_RDWR] 0o600) in
  finally (fun () ->
    Unix.(lockf lock_fd F_LOCK 0);
    finally f (fun () -> Unix.(lockf lock_fd F_ULOCK 0))
  ) (fun () -> Unix.close lock_fd)

let with_locks path f =
  let lock_m =
    with_mutex file_locks_m (fun () ->
      try Hashtbl.find file_locks path
      with Not_found ->
        let m = Mutex.create () in
        Hashtbl.add file_locks path m;
        m
    ) in
  with_mutex lock_m (fun () ->
    let lockf_path = path ^ ".lock" in
    with_lockf lockf_path f
  )

let rmw f =
  let path = with_mutex persistent_fn_m (fun () -> !persistent_fn) in
  with_locks path (fun () ->
    begin
      try devices := DeviceSet.load_file path
      with _ -> devices := DeviceSet.make ()
    end;
    let res = f !devices in
    DeviceSet.save_file !devices path;
    res
  )

let create device targets =
  rmw (fun devices -> DeviceSet.create devices device targets)

let remove device =
  rmw (fun devices -> DeviceSet.remove devices device)

let reload device targets =
  rmw (fun devices -> DeviceSet.reload devices device targets)

let suspend device =
  rmw (fun devices -> DeviceSet.suspend devices device)

let resume device =
  rmw (fun devices -> DeviceSet.resume devices device)

let mknod device path mode =
  rmw (fun devices -> DeviceSet.mknod devices device path mode)

let stat device =
  rmw (fun devices -> DeviceSet.stat devices device)

let ls () =
  rmw (fun devices -> DeviceSet.ls devices ())

let clear () =
  rmw (fun devices -> DeviceSet.clear devices ())

let set_path path =
  with_mutex persistent_fn_m (fun () -> persistent_fn := path)
