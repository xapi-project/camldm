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

val remove: string -> unit
(** [remove name]: remove the device mapper device with name [name] *)

val suspend: string -> unit
(** [suspend name]: suspends the device mapper device with name [name] *)

val resume: string -> unit
(** [resume name]: resumes the suspended device mapper device with name [name] *)

module Location : sig
  type device =
  | Number of int32 * int32 (** major * minor *)
  | Path of string
  with sexp

  type t = {
    device: device;
    offset: int64; (** sectors *)
  } with sexp
end

module Striped : sig
  type t = {
    size: int64; (** sectors, a power of 2 and at least PAGE_SIZE *)
    stripes: Location.t array;
  } with sexp
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
end

val create: string -> Target.t list -> unit
(** [create name targets]: creates a device with name [name] and
    targets [targets]. This function blocks until the udev event has
    fired and the /dev/mapper device has been created. *)

val reload: string -> Target.t list -> unit
(** [reload name targets]: modifies the existing device [name] to have targets [targets] *)

val mknod: string -> string -> int -> unit
(** [mknod name path mode]: creates a Unix device node for device
    [name] at [path] and with [mode] *)

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
