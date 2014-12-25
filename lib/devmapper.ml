(*
 * Copyright (C) 2014 Citrix Systems Inc.
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


module Lowlevel = struct
  (** The unsafe direct interface to the C library *)

  open Ctypes
  open PosixTypes
  open Foreign

  type kind =
  | DM_DEVICE_CREATE
  | DM_DEVICE_RELOAD
  | DM_DEVICE_REMOVE
  | DM_DEVICE_REMOVE_ALL
  | DM_DEVICE_SUSPEND
  | DM_DEVICE_RESUME
  | DM_DEVICE_INFO
  | DM_DEVICE_DEPS
  | DM_DEVICE_RENAME
  | DM_DEVICE_VERSION
  | DM_DEVICE_STATUS
  | DM_DEVICE_TABLE
  | DM_DEVICE_WAITEVENT
  | DM_DEVICE_LIST
  | DM_DEVICE_CLEAR
  | DM_DEVICE_MKNODES
  | DM_DEVICE_LIST_VERSIONS
  | DM_DEVICE_TARGET_MSG
  | DM_DEVICE_SET_GEOMETRY
  external dm_kind_to_int: kind -> int = "dm_kind_to_int"

  type dm_task = [ `Dm_task ] structure ptr

  let dm_task : dm_task typ = ptr (structure "dm_task")
  let dm_task_opt : dm_task option typ = ptr_opt (structure "dm_task")

  let dm_task_create' = foreign "dm_task_create" (int @-> returning dm_task_opt)
  let dm_task_create kind = dm_task_create' (dm_kind_to_int kind)

  let dm_task_destroy = foreign "dm_task_destroy" (dm_task @-> returning void)

  let bool = view ~read:((<>)0) ~write:(fun b -> compare b false) int

  let dm_task_set_name = foreign "dm_task_set_name" (dm_task @-> string @-> returning bool)
  let dm_task_set_uuid = foreign "dm_task_set_uuid" (dm_task @-> string @-> returning bool)

  let dm_task_run = foreign "dm_task_run" (dm_task @-> returning bool)
end

let finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    g ();
    raise e

let with_task kind f =
  let open Lowlevel in
  match dm_task_create kind with
  | None -> failwith "Failed to create device-mapper task; check permissions and retry"
  | Some dm_task ->
    finally
      (fun () -> f dm_task)
      (fun () -> dm_task_destroy dm_task)

let _simple kind name =
  let open Lowlevel in
  with_task kind
    (fun dm_task ->
      if not (dm_task_set_name dm_task name)
      then failwith (Printf.sprintf "dm_task_set_name %s failed" name);
      if not (dm_task_run dm_task)
      then failwith "dm_task_run failed"
    )

let remove = _simple Lowlevel.DM_DEVICE_REMOVE    
