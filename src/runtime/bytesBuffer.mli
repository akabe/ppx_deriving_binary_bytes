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

type t

val create : int -> t

val length : t -> int

val to_string : t -> string

val to_bytes : t -> bytes

(** An alias of [to_bytes]. *)
val contents : t -> bytes

(** {2 Comparison} *)

val equal : t -> t -> bool

val compare : t -> t -> int

(** {2 Setters} *)

val set_char : t -> int -> char -> unit

val set_int8 : t -> int -> int -> unit

val set_uint8 : t -> int -> int -> unit

val set_int16be : t -> int -> int -> unit

val set_int16le : t -> int -> int -> unit

val set_uint16be : t -> int -> int -> unit

val set_uint16le : t -> int -> int -> unit

val set_int32be : t -> int -> int32 -> unit

val set_int32le : t -> int -> int32 -> unit

val set_int64be : t -> int -> int64 -> unit

val set_int64le : t -> int -> int64 -> unit

val add_char : t -> char -> unit

val add_int8 : t -> int -> unit

val add_uint8 : t -> int -> unit

val add_int16be : t -> int -> unit

val add_int16le : t -> int -> unit

val add_uint16be : t -> int -> unit

val add_uint16le : t -> int -> unit

val add_int32be : t -> int32 -> unit

val add_int32le : t -> int32 -> unit

val add_int64be : t -> int64 -> unit

val add_int64le : t -> int64 -> unit

val blit : Bytes.t -> int -> t -> int -> int -> unit

val blit_string : string -> int -> t -> int -> int -> unit

(** [fill buf pos len v] *)
val fill : t -> int -> int -> int -> unit

(** {2 Printers} *)

val pp : Format.formatter -> t -> unit

val pp_hex : Format.formatter -> t -> unit
