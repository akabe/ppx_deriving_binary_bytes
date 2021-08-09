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

(** Conversion on C-style strings

    [CString.t] is corresponding to null-terminated string. For example,
    string ["Hello"] is encoded as ["Hello\x00"]. *)

type t = string

let of_binary_bytes b i =
  let n = Bytes.length b in
  let buf = Buffer.create 8 in
  let rec aux i =
    if i >= n then n else begin
      let c = Bytes.get b i in
      if c = '\x00' then i + 1 else begin
        Buffer.add_char buf c ;
        aux (i + 1)
      end
    end in
  let j = aux i in
  Buffer.contents buf, j

let binary_bytes_of buf s =
  let len = String.length s in
  let ofs = BytesBuffer.length buf in
  BytesBuffer.blit_string s 0 buf ofs len ;
  BytesBuffer.set_char buf (ofs + len) '\x00'
