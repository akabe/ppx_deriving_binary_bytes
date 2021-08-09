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

open Format
open Ppx_deriving.Ast_convenience
open Ppxlib
open Ppxlib.Ast_helper
open Variant

let affix = `Suffix "of_binary_bytes"

let attr_decoder ~deriver attrs =
  Ppx_deriving.(attrs
                |> attr ~deriver "of_binary_bytes"
                |> Arg.(get_attr ~deriver expr))
  |> function
  | None -> None
  | Some e -> Some (Astmisc.get_labelled_args_from_fun e, e)

let erecord labels =
  let label_bindings =
    List.map
      (fun ld -> (Astmisc.mklid ld.pld_name.txt, evar ld.pld_name.txt))
      labels in
  Exp.record label_bindings None

let rec decoder_of_core_type ~deriver typ =
  let loc = typ.ptyp_loc in
  match typ with
  | [%type: unit] -> [%expr fun _ _i -> ((), _i)]
  | [%type: string]
  | [%type: String.t] ->
    decoder_of_string_like_type ~deriver typ
      ~func:[%expr Ppx_deriving_binary_runtime.Std.string_of_binary_bytes]
  | [%type: bytes]
  | [%type: Bytes.t] ->
    decoder_of_string_like_type ~deriver typ
      ~func:[%expr Ppx_deriving_binary_runtime.Std.bytes_of_binary_bytes]
  | [%type: [%t? elt] list] ->
    decoder_of_list_like_type ~deriver typ elt
      ~func:[%expr Ppx_deriving_binary_runtime.Std.list_of_binary_bytes]
  | [%type: [%t? elt] array] ->
    decoder_of_list_like_type ~deriver typ elt
      ~func:[%expr Ppx_deriving_binary_runtime.Std.array_of_binary_bytes]
  | [%type: [%t? typ] ref] ->
    [%expr fun _b _i ->
      let _x, _i = [%e decoder_of_core_type ~deriver typ] _b _i in
      (ref _x, _i)]
  | { ptyp_desc = Ptyp_constr (lid, args); _ } ->
    let f = Exp.ident (mknoloc (Ppx_deriving.mangle_lid affix lid.txt)) in
    let args' = List.map (decoder_of_core_type ~deriver) args in
    let fwd = app f args' in
    [%expr fun _b _i -> [%e fwd] _b _i]
  | { ptyp_desc = Ptyp_tuple typs; _ } ->
    decoder_of_tuple ~deriver ~constructor:(fun exps -> Exp.tuple exps) ~loc typs
  | { ptyp_desc = Ptyp_var name; _ } ->
    [%expr ([%e evar ("poly_" ^ name)] : bytes -> int -> _ * int)]
  (* Errors *)
  | _ ->
    Ppx_deriving.raise_errorf ~loc "%s cannot be derived for %s"
      deriver (Ppx_deriving.string_of_core_type typ)

and decoder_of_string_like_type ~deriver ~func t =
  let loc = t.ptyp_loc in
  let len = Astmisc.attr_length_exn ~loc ~deriver t.ptyp_attributes in
  [%expr [%e func] ~n:[%e Astmisc.eint len]]

and decoder_of_list_like_type ~deriver ~func t elt =
  let decoder = decoder_of_core_type ~deriver elt in
  let loc = t.ptyp_loc in
  let len = Astmisc.attr_length_exn ~loc ~deriver t.ptyp_attributes in
  [%expr [%e func] ~n:[%e Astmisc.eint len] [%e decoder]]

and decoder_of_tuple ~deriver ~constructor ~loc typs =
  let vars_typs = List.mapi (fun i t -> (sprintf "_%d" i, t)) typs in
  let tuple = constructor (List.map (fun (s, _) -> evar s) vars_typs) in
  let body =
    List.fold_right
      (fun (x, t) k' ->
         let loc = t.ptyp_loc in
         let decoder = decoder_of_core_type ~deriver t in
         [%expr let ([%p pvar x], _i) = [%e decoder] _b _i in [%e k']])
      vars_typs [%expr ([%e tuple], _i)] in
  [%expr fun _b _i -> [%e body]]

and decoder_of_record ~deriver ~constructor ~loc labels =
  let record = constructor (erecord labels) in
  let body =
    List.fold_right
      (fun ld k' ->
         let loc = ld.pld_type.ptyp_loc in
         let decoder =
           match attr_decoder ~deriver ld.pld_attributes with
           | None -> decoder_of_core_type ~deriver ld.pld_type
           | Some (labels, decoder) ->
             let args = List.map (fun s -> Labelled s, evar s) labels in
             Exp.apply decoder args (* apply pre-decoded record fields *)
         in
         let field_name = pvar ld.pld_name.txt in
         [%expr let ([%p field_name], _i) = [%e decoder] _b _i in [%e k']])
      labels
      [%expr ([%e record], _i)] in
  [%expr fun _b _i -> [%e body]]
