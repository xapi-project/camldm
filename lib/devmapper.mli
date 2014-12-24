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

  val dm_task_create: int -> dm_task option
  (** [dm_task_create kind] opens the device mapper control interface
      and initialises a task [kind]. This will return None if the
      caller doesn't have permission to talk to the control interface
      or if there is a protocol mismatch between userspace and kernel
      space. No programmatic diagnostics are available but the C library
      will print errors on stderr. *)

  val dm_task_destroy: dm_task -> unit
  (** [dm_task_destroy task] cleans up resources associated with [task]
      and deallocates it. The [task] must not be used again. *)
end
