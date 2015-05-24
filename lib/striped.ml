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

type t = {
  size: int64; (* sectors, a power of 2 and at least PAGE_SIZE *)
  stripes: Location.t array;
} with sexp

let marshal t =
  Printf.sprintf "%d %Ld %s" (Array.length t.stripes) t.size
    (String.concat " " (Array.(to_list (map Location.marshal t.stripes))))

let unmarshal x =
  try
    match Stringext.split ~max:3 x ~on:' ' with
    | [ length; size; stripes ] ->
      let rec loop remaining =
        match Stringext.split ~max:3 remaining ~on:' ' with
        | [] -> []
        | a :: b :: rest ->
          let this = match Location.unmarshal (a ^ " " ^ b) with
          | `Ok x -> x
          | `Error x -> failwith x in
          let remaining = String.concat "" rest in
          this :: (loop remaining)
        | [ x ] ->
          failwith ("Trailing junk in Striped.unmarshal: " ^ x) in
      let length = int_of_string length in
      let size = Int64.of_string size in
      let stripes = Array.of_list (loop stripes) in
      if Array.length stripes <> length
      then failwith (Printf.sprintf "Striped.unmarshal length doesn't match: %d <> %d" length (Array.length stripes));
      `Ok { size; stripes }
    | _ ->
      failwith ("Failed to parse in Striped.unmarshal: " ^ x)
  with Failure msg ->
    `Error msg
  | e ->
    `Error ("Striped.unmarshal caught: " ^ (Printexc.to_string e))
