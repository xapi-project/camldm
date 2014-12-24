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

  type dm_task = [ `Dm_task ] structure ptr

  let dm_task : dm_task typ = ptr (structure "dm_task")
  let dm_task_opt : dm_task option typ = ptr_opt (structure "dm_task")

  let dm_task_create = foreign "dm_task_create" (int @-> returning dm_task_opt)

  let dm_task_destroy = foreign "dm_task_destroy" (dm_task @-> returning void)
end
