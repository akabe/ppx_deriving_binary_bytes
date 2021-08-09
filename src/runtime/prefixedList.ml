(* ppx_deriving_binary

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

(** Prefixed lists

    [('a, 'b) PrefixedList.t] is a type of lists of elements of a type ['a],
    and a prefix of a type ['b].

    For example, [[0x00112233l; 0x44556677l; 0x8899aabb]] of a type
    [(uint32be, uint16be) PrefixedList.t] is represented as
    - the heading two bytes for the length of a list (0x0003), and
    - the 4x3 bytes for contents of a list.
    {v
00  03  00  11  22  33  44  55  66  77  88  99  aa  bb
<---->  <--- x[0] --->  <--- x[1] --->  <--- x[2] --->
# of elements
v} *)

type ('a, 'b) t = 'a list

let of_binary_bytes elt_of_binary_bytes prefix_of_binary_bytes cs ofs =
  let n, ofs = prefix_of_binary_bytes cs ofs in
  let rec aux acc i ofs =
    if n <= i then List.rev acc, ofs else begin
      let x, ofs = elt_of_binary_bytes cs ofs in
      aux (x :: acc) (i + 1) ofs
    end in
  aux [] 0 ofs

let binary_bytes_of binary_bytes_of_elt binary_bytes_of_prefix cb xs =
  let len = List.length xs in
  binary_bytes_of_prefix cb len ;
  List.iter (binary_bytes_of_elt cb) xs
