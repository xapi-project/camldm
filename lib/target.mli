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

val marshal: kind -> string * string
val unmarshal: string * string -> kind
