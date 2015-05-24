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

module type DEVMAPPER = sig

  type device = string

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

  val create: device -> Target.t list -> unit
  (** [create device targets]: creates a device with name [device] and
   targets [targets]. This function returns before any generated udev events
   have been processed, but the client may use [mknod] to manually create
   a device node, which will be fully functional.

   It seems that the targets must be contiguous, leaving no unmapped gap
   in the source device. *)

  val remove: device -> unit
  (** [remove device]: remove the device mapper device with name [device] *)

  val reload: device -> Target.t list -> unit
  (** [reload device targets]: modifies the existing device [device] to
      have targets [targets]. The modifications will only take effect
      after the device is suspended and resumed.*)

  val suspend: device -> unit
  (** [suspend device]: suspends the device mapper device with name [device] *)

  val resume: device -> unit
  (** [resume device]: resumes the suspended device mapper device with
      name [device]. If the targets have been reloaded then the new values
      will take effect. *)

  val mknod: device -> string -> int -> unit
  (** [mknod device path mode]: creates a Unix device node for device
      [device] at [path] and with [mode] *)

  val stat: device -> info option
  (** [stat device] describes the device mapper [device], or returns None if
      the device doesn't exist. *)

  val ls: unit -> device list
  (** [ls ()] returns a list of all current devices *)
end

