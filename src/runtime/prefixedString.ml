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

(** Prefixed strings

    ['a PrefixedString.t] is a type of strings with prefixes of a type ['a].

    For example, a string ["Hello"] is represented as follows:
    {v
05  00  48  65  6c  6c  6f
<---->  H   e   l   l   o
# of characters
v} *)

type 'a t = string

let of_binary_bytes prefix_of_binary_bytes b i =
  let n, j = prefix_of_binary_bytes b i in
  (Bytes.sub_string b j n, j + n)

let binary_bytes_of prefix_binary_bytes_of buf s =
  let len = String.length s in
  prefix_binary_bytes_of buf len ;
  let ofs = BytesBuffer.length buf in
  BytesBuffer.blit_string s 0 buf ofs len ;
