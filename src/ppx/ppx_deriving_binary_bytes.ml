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

open Ppx_deriving

let () =
  let deriver = "of_binary_bytes" in
  register (create "of_binary_bytes"
              ~core_type:(Decoder.decoder_of_core_type
                            ~deriver ~path:"<abstract>")
              ~type_decl_str:(Decoder.type_decl_str ~deriver)
              ~type_decl_sig:Decoder.type_decl_sig
              ())

let () =
  let deriver = "binary_bytes_of" in
  register (create "binary_bytes_of"
              ~core_type:(Encoder.encoder_of_core_type
                            ~deriver ~path:"<abstract>")
              ~type_decl_str:(Encoder.type_decl_str ~deriver)
              ~type_decl_sig:Encoder.type_decl_sig
              ())

let () =
  let deriver = "binary_bytes" in
  register (create "binary_bytes"
              ~type_decl_str:(fun ~options ~path type_decls ->
                  Encoder.type_decl_str ~deriver ~options ~path type_decls
                  @ Decoder.type_decl_str ~deriver ~options ~path type_decls)
              ~type_decl_sig:(fun ~options ~path type_decls ->
                  Encoder.type_decl_sig ~options ~path type_decls
                  @ Decoder.type_decl_sig ~options ~path type_decls)
              ())
