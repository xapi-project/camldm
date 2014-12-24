/*
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
 */
#include <libdevmapper.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

/* Replace this with Ctypes */

/* Must match the order of declaration in the OCaml */
static int dm_task_kind[] = {
  DM_DEVICE_CREATE,
  DM_DEVICE_RELOAD,
  DM_DEVICE_REMOVE,
  DM_DEVICE_REMOVE_ALL,
  DM_DEVICE_SUSPEND,
  DM_DEVICE_RESUME,
  DM_DEVICE_INFO,
  DM_DEVICE_DEPS,
  DM_DEVICE_RENAME,
  DM_DEVICE_VERSION,
  DM_DEVICE_STATUS,
  DM_DEVICE_TABLE,
  DM_DEVICE_WAITEVENT,
  DM_DEVICE_LIST,
  DM_DEVICE_CLEAR,
  DM_DEVICE_MKNODES,
  DM_DEVICE_LIST_VERSIONS,
  DM_DEVICE_TARGET_MSG,
  DM_DEVICE_SET_GEOMETRY
};

CAMLprim value dm_kind_to_int(value kind){
  CAMLparam1(kind);
  int ctor_tag = Int_val(kind);
  /* This can only happen if the OCaml is out-of-sync with the C */
  if ((ctor_tag < 0) || (ctor_tag >= sizeof(dm_task_kind)))
    abort();
  CAMLreturn(Val_int(dm_task_kind[ctor_tag]));
}
