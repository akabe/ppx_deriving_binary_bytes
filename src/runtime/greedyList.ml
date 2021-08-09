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

(** Greedy lists

    ['a GreedyList.t] is a type of lists whose elements have a type ['a].
    Its elements are decoded as many as possible.

    For example, binary data
    {v
00  11  22  33  44  55  66  77    88  99  aa  bb  cc  dd
<--- x[0] --->  <--- x[1] --->    <--- x[2] --->
v}
    can be decoded to a list [[0x00112233l; 0x44556677l; 0x8899aabb]] as
    a type [uint32be GreedyList.t]. The trailing [cc dd] is not able to
    converted due to lack of length. *)

type 'a t = 'a list

let of_binary_bytes elm_of_binary_bytes cs i =
  let rec aux acc i =
    match elm_of_binary_bytes cs i with
    | x, i -> aux (x :: acc) i
    | exception Invalid_argument _ -> List.rev acc, i
  in
  aux [] i

let binary_bytes_of binary_bytes_of_elt cb xs =
  List.iter (binary_bytes_of_elt cb) xs
