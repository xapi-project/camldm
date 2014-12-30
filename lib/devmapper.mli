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

type device = string
(** The name of a device mapper device (e.g. vg_st30_lv_root) *)

module Location : sig
  type device =
  | Number of int32 * int32 (** major * minor *)
  | Path of string
  with sexp
  (** Identifies any other block device on the system *)

  type t = {
    device: device;
    offset: int64; (** sectors *)
  } with sexp
  (** A Location is an offset within an existing block device *)
end

module Striped : sig
  type t = {
    size: int64; (** sectors, a power of 2 and at least PAGE_SIZE *)
    stripes: Location.t array;
  } with sexp
  (** A device mapper target may be striped i.e. the physical data
      for a sector [s] is written to [stripes.(i / size % len(stripes))]
      i.e. the first [size] sectors are written to [stripes.(0)], then
      the second [size] sectors are written to [stripes.(1)], cycling
      back to [stripes.(0)] once we run out of stripes. *)
end

module Target : sig
  type kind =
  | Linear of Location.t
  | Striped of Striped.t
  | Unknown of string * string

  type t = {
    start: Int64.t; (** sectors *)
    size: Int64.t;  (** sectors *)
    kind: kind;
  } with sexp
  (** A device mapper target maps a range of sectors from [start] to
      [start + size - 1] to somewhere. We currently recognise linear
      mappings and striped mappings. Unknown mappings are "passed through"
      as a pair of strings so it is safe to [reload] a device with the
      result of [stat] without fearing truncation. *)
end

val remove: device -> unit
(** [remove device]: remove the device mapper device with name [device] *)

val create: device -> Target.t list -> unit
(** [create device targets]: creates a device with name [device] and
    targets [targets]. This function blocks until the udev event has
    fired and the /dev/mapper device has been created. *)

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

val stat: device -> info option
(** [stat device] describes the device mapper [device], or returns None if
    the device doesn't exist. *)

val ls: unit -> device list
(** [ls ()] returns a list of all current devices *)
