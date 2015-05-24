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

val marshal: t -> string
val unmarshal: string -> [> `Ok of t | `Error of string ]
