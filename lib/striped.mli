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

type t = {
  size: int64; (** sectors, a power of 2 and at least PAGE_SIZE *)
  stripes: Location.t array;
} with sexp
(** A device mapper target may be striped i.e. the physical data
    for a sector [s] is written to [stripes.(i / size % len(stripes))]
    i.e. the first [size] sectors are written to [stripes.(0)], then
    the second [size] sectors are written to [stripes.(1)], cycling
    back to [stripes.(0)] once we run out of stripes. *)

val marshal: t -> string
val unmarshal: string -> [> `Ok of t | `Error of string ]
