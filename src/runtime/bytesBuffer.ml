(* ppx_deriving_binary_bytes

   Copyright (c) 2021 Akinori Abe

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** Byte sequence with extensible capacity *)

type t =
  {
    mutable data : Bytes.t;
    mutable len : int;
  }

let create n =
  {
    data = Bytes.make (max n 1) '\x00';
    len = 0;
  }

let length buf = buf.len

let to_bytes buf = Bytes.sub buf.data 0 buf.len

let to_string buf = Bytes.sub_string buf.data 0 buf.len

let contents = to_bytes

let equal b1 b2 = Bytes.equal (contents b1) (contents b2)

let compare b1 b2 = Bytes.compare (contents b1) (contents b2)

let extend_if_necessary buf len =
  let rec aux n = if n >= len then n else aux (n lsl 1) in
  let curr_size = Bytes.length buf.data in
  let new_size = aux curr_size in
  if new_size > curr_size then begin
    let data' = Bytes.make new_size '\x00' in
    Bytes.blit buf.data 0 data' 0 buf.len ;
    buf.data <- data'
  end ;
  if len > buf.len then buf.len <- len

let wrap ~n ~f buf pos x =
  extend_if_necessary buf (pos + n) ;
  try
    f buf.data pos x
  with Invalid_argument _ ->
    Format.printf "Oops"

let set_char = wrap ~n:1 ~f:Bytes.set
let set_int8 = wrap ~n:1 ~f:Bytes.set_int8
let set_uint8 = wrap ~n:1 ~f:Bytes.set_uint8
let set_int16be = wrap ~n:2 ~f:Bytes.set_int16_be
let set_int16le = wrap ~n:2 ~f:Bytes.set_int16_le
let set_uint16be = wrap ~n:2 ~f:Bytes.set_uint16_be
let set_uint16le = wrap ~n:2 ~f:Bytes.set_uint16_le
let set_int32be = wrap ~n:4 ~f:Bytes.set_int32_be
let set_int32le = wrap ~n:4 ~f:Bytes.set_int32_le
let set_int64be = wrap ~n:8 ~f:Bytes.set_int64_be
let set_int64le = wrap ~n:8 ~f:Bytes.set_int64_le

let add_of_set set buf x = set buf buf.len x

let add_char = add_of_set set_char
let add_int8 = add_of_set set_int8
let add_uint8 = add_of_set set_uint8
let add_int16be = add_of_set set_int16be
let add_int16le = add_of_set set_int16le
let add_uint16be = add_of_set set_uint16be
let add_uint16le = add_of_set set_uint16le
let add_int32be = add_of_set set_int32be
let add_int32le = add_of_set set_int32le
let add_int64be = add_of_set set_int64be
let add_int64le = add_of_set set_int64le

let wrap_blit ~f src src_pos buf pos len =
  extend_if_necessary buf (pos + len) ;
  f src src_pos buf.data pos len

let blit = wrap_blit ~f:Bytes.blit

let blit_string = wrap_blit ~f:Bytes.blit_string

let fill buf pos len x =
  extend_if_necessary buf (pos + len) ;
  for i = 0 to len - 1 do
    set_uint8 buf (pos + i) x
  done

(** {2 Printers} *)

let pp ppf buf =
  Format.pp_print_string ppf (Bytes.unsafe_to_string (contents buf))

let pp_hex ppf buf = Printer.pp_bytes_hex ppf (contents buf)
