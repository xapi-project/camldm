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

module Lowlevel: sig
  (** A raw, unsafe interface to the C library *)

  type dm_task
  (** All operations are phrased as 'tasks' which are created,
      configured, run and then destroyed. *)

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
  (** Every task has an associated kind *)

  val dm_task_create: kind -> dm_task option
  (** [dm_task_create kind] opens the device mapper control interface
      and initialises a task [kind]. This will return None if the
      caller doesn't have permission to talk to the control interface
      or if there is a protocol mismatch between userspace and kernel
      space. No programmatic diagnostics are available but the C library
      will print errors on stderr. *)

  val dm_task_destroy: dm_task -> unit
  (** [dm_task_destroy task] cleans up resources associated with [task]
      and deallocates it. The [task] must not be used again. *)

  val dm_task_set_name: dm_task -> string -> bool
  (** [dm_task_set_name task name] associate [name] with [task], returning
      true if successful. *)

  val dm_task_set_uuid: dm_task -> string -> bool
  (** [dm_task_set_uuid task uuid] associate [uuid] with [task], returning
      true if successful. *)

  val dm_task_run: dm_task -> bool
  (* [dm_tsak_run task] runs the ioctl, returning true if successful. *)
end

val remove: string -> unit
(** [remove name]: remove the device mapper device with name [name] *)

val suspend: string -> unit
(** [suspend name]: suspends the device mapper device with name [name] *)

val resume: string -> unit
(** [resume name]: resumes the suspended device mapper device with name [name] *)

module Linear : sig
  type device =
  | Number of int32 * int32 (** major * minor *)
  | Path of string
  with sexp

  type t = {
    device: device;
    offset: int64; (** sectors *)
  } with sexp
end

module Target : sig
  type kind =
  | Linear of Linear.t
  | Unknown of string * string

  type t = {
    start: Int64.t; (** sectors *)
    size: Int64.t;  (** sectors *)
    kind: kind;
  } with sexp
end

val create: string -> Target.t list -> unit
(** [create name targets]: creates a device with name [name] and targets [targets] *)

val reload: string -> Target.t list -> unit
(** [reload name targets]: modifies the existing device [name] to have targets [targets] *)

val mknods: string option -> unit
(** [mknods (Some device)] creates the device nodes for [device]; [mknods None] creates
    all device nodes system-wide. *)

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

val info: string -> info option
(** [info name] returns [Some info] describing [name], or [None} if [name] doesn't
    exist. *)

val ls: unit -> string list
(** [ls ()] returns a list of all current names *)
