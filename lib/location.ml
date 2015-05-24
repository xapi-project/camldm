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

type device =
| Number of int32 * int32 (** major * minor *)
| Path of string
with sexp

type t = {
  device: device;
  offset: int64; (* sectors *)
} with sexp

let marshal t = match t.device with
| Number (major, minor) -> Printf.sprintf "%ld:%ld %Ld" major minor t.offset
| Path x -> Printf.sprintf "%s %Ld" x t.offset

let unmarshal x =
  let error () = `Error (Printf.sprintf "Cannot parse location: %s" x) in
  match Stringext.split ~max:2 x ~on:' ' with
  | [ majorminor; offset ] ->
    begin match Stringext.split ~max:2 majorminor ~on:':' with
    | [ major; minor ] ->
      begin
        try
          let major = Int32.of_string major in
          let minor = Int32.of_string minor in
          let device = Number (major, minor) in
          let offset = Int64.of_string offset in
          `Ok { device; offset }
        with _ ->
          error ()
      end
    | _ ->
      begin
        try
          let device = Path majorminor in
          let offset = Int64.of_string offset in
          `Ok { device; offset }
        with _ ->
          error ()
      end
    end
  | _ -> error ()

