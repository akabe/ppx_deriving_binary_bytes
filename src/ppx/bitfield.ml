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

open Ppx_deriving.Ast_convenience
open Ppxlib
open Ppxlib.Ast_helper

type t =
  {
    rbf_name : string;
    rbf_offset : int;
    rbf_length : int;
    rbf_mask : int;
    rbf_loc : Location.t;
  }

let of_ocaml_label_declarations ~deriver label_decls =
  let aux (default_ofs, acc) ld =
    let ofs =
      match Astmisc.attr_offset ~deriver ld.pld_attributes with
      | Some n -> n
      | None -> default_ofs in
    let len =
      match Astmisc.attr_length ~deriver ld.pld_attributes with
      | Some n -> n
      | None -> 1 in
    let field = {
      rbf_name = ld.pld_name.txt;
      rbf_offset = ofs;
      rbf_length = len;
      rbf_mask = 1 lsl len - 1;
      rbf_loc = ld.pld_loc;
    } in
    (ofs + len, field :: acc)
  in
  let _, rev_fields = List.fold_left aux (0, []) label_decls in
  List.rev rev_fields
