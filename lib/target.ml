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

open Sexplib.Std

type kind =
| Linear of Location.t
| Striped of Striped.t
| Unknown of string * string
with sexp

type t = {
  start: int64; (* sectors *)
  size: int64;  (* sectors *)
  kind: kind;
} with sexp

let marshal = function
| Unknown(ttype, params) -> ttype, params
| Linear l -> "linear", Location.marshal l
| Striped s -> "striped", Striped.marshal s

let unmarshal (ttype, params) = match ttype with
| "linear" ->
  begin match Location.unmarshal params with
  | `Ok l -> Linear l
  | `Error msg -> failwith msg
  end
| "striped" ->
  begin match Striped.unmarshal params with
  | `Ok s -> Striped s
  | `Error msg -> failwith msg
  end
| _ -> Unknown(ttype, params)
