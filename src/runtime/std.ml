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

(** Definitions for convenience *)

(** Deserialization errors [Error type_name] *)
exception Parse_error of string

(** {2 Integers} *)

(** {3 Standard integers} *)

type int8 = int
type uint8 = int
type int16be = int
type int16le = int
type uint16be = int
type uint16le = int
type uint32be = int32
type uint32le = int32
type uint64be = int64
type uint64le = int64

let char_of_binary_bytes b i = (Bytes.get b i, i + 1)
let int8_of_binary_bytes b i = (Bytes.get_int8 b i, i + 1)
let uint8_of_binary_bytes b i = (Bytes.get_uint8 b i, i + 1)
let int16be_of_binary_bytes b i = (Bytes.get_int16_be b i, i + 2)
let int16le_of_binary_bytes b i = (Bytes.get_int16_le b i, i + 2)
let uint16be_of_binary_bytes b i = (Bytes.get_uint16_be b i, i + 2)
let uint16le_of_binary_bytes b i = (Bytes.get_uint16_le b i, i + 2)
let int32be_of_binary_bytes b i = (Bytes.get_int32_be b i, i + 4)
let int32le_of_binary_bytes b i = (Bytes.get_int32_le b i, i + 4)
let int64be_of_binary_bytes b i = (Bytes.get_int64_be b i, i + 8)
let int64le_of_binary_bytes b i = (Bytes.get_int64_le b i, i + 8)

let binary_bytes_of_char = BytesBuffer.add_char
let binary_bytes_of_int8 = BytesBuffer.add_int8
let binary_bytes_of_uint8 = BytesBuffer.add_uint8
let binary_bytes_of_int16be = BytesBuffer.add_int16be
let binary_bytes_of_int16le = BytesBuffer.add_int16le
let binary_bytes_of_uint16be = BytesBuffer.add_uint16be
let binary_bytes_of_uint16le = BytesBuffer.add_uint16le
let binary_bytes_of_int32be = BytesBuffer.add_int32be
let binary_bytes_of_int32le = BytesBuffer.add_int32le
let binary_bytes_of_int64be = BytesBuffer.add_int64be
let binary_bytes_of_int64le = BytesBuffer.add_int64le

let pp_int8 = Format.pp_print_int
let pp_uint8 = Format.pp_print_int
let pp_int16be = Format.pp_print_int
let pp_int16le = Format.pp_print_int
let pp_uint16be = Format.pp_print_int
let pp_uint16le = Format.pp_print_int
let pp_int32be ppf = Format.fprintf ppf "%ld"
let pp_int32le ppf = Format.fprintf ppf "%ld"
let pp_int64be ppf = Format.fprintf ppf "%Ld"
let pp_int64le ppf = Format.fprintf ppf "%Ld"

(** {3 Aliases of [int]} *)

type int32bei = int
type int32lei = int
type int64bei = int
type int64lei = int

let int32bei_of_binary_bytes b i = (Bytes.get_int32_be b i |> Int32.to_int, i + 4)
let int32lei_of_binary_bytes b i = (Bytes.get_int32_le b i |> Int32.to_int, i + 4)
let int64bei_of_binary_bytes b i = (Bytes.get_int64_be b i |> Int64.to_int, i + 8)
let int64lei_of_binary_bytes b i = (Bytes.get_int64_le b i |> Int64.to_int, i + 8)

let binary_bytes_of_int32bei b x = BytesBuffer.add_int32be b (Int32.of_int x)
let binary_bytes_of_int32lei b x = BytesBuffer.add_int32le b (Int32.of_int x)
let binary_bytes_of_int64bei b x = BytesBuffer.add_int64be b (Int64.of_int x)
let binary_bytes_of_int64lei b x = BytesBuffer.add_int64le b (Int64.of_int x)

let pp_int32bei = Format.pp_print_int
let pp_int32lei = Format.pp_print_int
let pp_int64bei = Format.pp_print_int
let pp_int64lei = Format.pp_print_int

(** {2 Floating-point values} *)

type float32be = float
type float32le = float
type float64be = float
type float64le = float

let float32be_of_binary_bytes cs i =
  let n, i = int32be_of_binary_bytes cs i in
  Int32.float_of_bits n, i

let float32le_of_binary_bytes cs i =
  let n, i = int32le_of_binary_bytes cs i in
  Int32.float_of_bits n, i

let float64be_of_binary_bytes cs i =
  let n, i = int64be_of_binary_bytes cs i in
  Int64.float_of_bits n, i

let float64le_of_binary_bytes cs i =
  let n, i = int64le_of_binary_bytes cs i in
  Int64.float_of_bits n, i

let pp_float32be = Format.pp_print_float
let pp_float32le = Format.pp_print_float
let pp_float64be = Format.pp_print_float
let pp_float64le = Format.pp_print_float

(** {2 Constant-length string-like types} *)

let string_of_binary_bytes ~n b i =
  (Bytes.sub_string b i n, i + n)

let binary_bytes_of_string ~n b s =
  let i = BytesBuffer.length b in
  let m = min n (String.length s) in
  BytesBuffer.blit_string s 0 b i m ;
  BytesBuffer.fill b m (n - m) 0x00

let bytes_of_binary_bytes ~n b i =
  (Bytes.sub b i n, i + n)

let binary_bytes_of_bytes ~n b s =
  let i = BytesBuffer.length b in
  let m = min n (Bytes.length s) in
  BytesBuffer.blit s 0 b i m ;
  BytesBuffer.fill b m (n - m) 0x00

(** {2 Constant-length lists} *)

let list_of_binary_bytes ~n elt_of_binary_bytes b ofs =
  let rec aux acc ofs i =
    if i >= n then List.rev acc, ofs else begin
      let x, ofs = elt_of_binary_bytes b ofs in
      aux (x :: acc) ofs (i + 1)
    end in
  aux [] ofs 0

let binary_bytes_of_list ~n binary_bytes_of_elt b lst =
  if n <> List.length lst
  then invalid_arg "inconsistent length of list" ;
  List.iter (binary_bytes_of_elt b) lst

(** {2 Constant-length arrays} *)

let array_of_binary_bytes ~n elt_of_binary_bytes b ofs =
  let lst, ofs = list_of_binary_bytes ~n elt_of_binary_bytes b ofs in
  (Array.of_list lst, ofs)

let binary_bytes_of_array ~n binary_bytes_of_elt b arr =
  if n <> Array.length arr
  then invalid_arg "inconsistent length of array" ;
  binary_bytes_of_list ~n binary_bytes_of_elt b (Array.to_list arr)

(** {2 Variable-length strings} *)

module CString = CString

module PrefixedString = PrefixedString

module GreedyString = GreedyString

(** {2 Variable-length lists} *)

module PrefixedList = PrefixedList

module GreedyList = GreedyList

(** {2 Byte sequence buffers} *)

module BytesBuffer = BytesBuffer
