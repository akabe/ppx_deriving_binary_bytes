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

(* Naming rules:
   - [encoder_of_xxx] function converts [xxx] into an expression
     of a type [CstructBuffer.t -> 'a -> unit].
   - [encoding_of_xxx] function converts [xxx] into an expression
     for encoding a value, including [_b : BytesBuffer.t] and other
     free variables. *)

let affix = `Prefix "binary_bytes_of"

let prj_tuple = function
  | [(v, _)] -> pvar v
  | vars_typs -> Pat.tuple (List.map (fun (v, _) -> pvar v) vars_typs)

let prj_record labels =
  let lid_var =
    List.map
      (fun ld -> Astmisc.mklid ld.pld_name.txt, pvar ld.pld_name.txt)
      labels in
  Pat.record lid_var Closed

let attr_encoder ~deriver attrs =
  Ppx_deriving.(attrs
                |> attr ~deriver "binary_bytes_of"
                |> Arg.(get_attr ~deriver expr))
  |> function
  | None -> None
  | Some e -> Some (Astmisc.get_labelled_args_from_fun e, e)

let rec encoder_of_core_type ~deriver ~path typ =
  let loc = typ.ptyp_loc in
  match typ with
  | [%type: unit] -> [%expr fun _b () -> ()]
  | [%type: string] ->
    encoder_of_string_like_type ~deriver ~loc typ
      ~func:[%expr Ppx_deriving_binary_runtime.Std.binary_bytes_of_string]
  | [%type: bytes] ->
    encoder_of_string_like_type ~deriver ~loc typ
      ~func:[%expr Ppx_deriving_binary_runtime.Std.binary_bytes_of_bytes]
  | [%type: [%t? elt] list] ->
    encoder_of_list_like_type ~deriver ~path ~loc typ elt
      ~func:[%expr Ppx_deriving_binary_runtime.Std.binary_bytes_of_list]
  | [%type: [%t? elt] array] ->
    encoder_of_list_like_type ~deriver ~path ~loc typ elt
      ~func:[%expr Ppx_deriving_binary_runtime.Std.binary_bytes_of_array]
  | [%type: [%t? typ] ref] ->
    [%expr fun _b _x -> [%e encoder_of_core_type ~deriver ~path typ] _b (!_x)]
  | { ptyp_desc = Ptyp_constr (lid, args); _ } ->
    let f = Exp.ident (mknoloc (Ppx_deriving.mangle_lid affix lid.txt)) in
    let args' = List.map (encoder_of_core_type ~deriver ~path) args in
    app f args'
  | { ptyp_desc = Ptyp_tuple typs; _ } ->
    let vars_typs = List.mapi (fun i t -> (sprintf "_%d" i, t)) typs in
    let prj = prj_tuple vars_typs in
    let fun_body = encoding_of_tuple_type ~loc ~deriver ~path vars_typs in
    [%expr fun _b [%p prj] -> [%e fun_body]]
  | { ptyp_desc = Ptyp_var name; _ } ->
    [%expr ([%e evar ("poly_" ^ name)]
            : Ppx_deriving_binary_runtime.BytesBuffer.t -> _ -> unit)]
  | { ptyp_desc = Ptyp_variant (rows, Closed, None); _ } ->
    encoder_of_polymorphic_variant ~deriver ~path ~loc rows typ.ptyp_attributes
  (* Errors *)
  | _ ->
    Ppx_deriving.raise_errorf ~loc "%s cannot be derived for %s"
      deriver (Ppx_deriving.string_of_core_type typ)

and encoder_of_string_like_type ~deriver ~loc ~func t =
  let len = Astmisc.attr_length_exn ~deriver ~loc t.ptyp_attributes in
  let loc = t.ptyp_loc in
  [%expr [%e func] ~n:[%e Astmisc.eint len]]

and encoder_of_list_like_type ~deriver ~path ~loc ~func t elt =
  let len = Astmisc.attr_length_exn ~deriver ~loc t.ptyp_attributes in
  let encoder = encoder_of_core_type ~deriver ~path elt in
  let loc = t.ptyp_loc in
  [%expr [%e func] ~n:[%e Astmisc.eint len] [%e encoder]]

and encoding_of_compound_type ~loc (vars_encoders : (string * expression) list) =
  List.fold_right
    (fun (x, encoder) k ->
       [%expr [%e encoder] _b [%e evar x] ; [%e k]])
    vars_encoders [%expr ()]

and encoding_of_tuple_type ~deriver ~path ~loc vars_typs =
  let vars_encoders =
    List.map
      (fun (v, t) -> (v, encoder_of_core_type ~deriver ~path t))
      vars_typs in
  encoding_of_compound_type ~loc vars_encoders

and encoder_of_polymorphic_variant ~deriver ~path ~loc row_fields attrs =
  let base_type = Astmisc.attr_base_type_exn ~deriver ~loc attrs in
  Variant.constructors_of_ocaml_row_fields ~deriver row_fields
  |> encoder_variant_of_constructors
    ~deriver ~path ~loc ~base_type ~constructor:Pat.variant

and encoder_variant_of_constructors
    ~deriver
    ~path
    ~loc
    ~base_type
    ~constructor
    (constrs : Variant.constructor list)
  =
  let base_encoder = encoder_of_core_type ~deriver ~path base_type in
  let cases =
    constrs
    |> List.map (fun c ->
        let name = c.con_name in
        let loc = c.con_loc in
        let set_tag = [%expr [%e base_encoder] _b [%e Exp.constant c.con_value]] in
        let path = path ^ "." ^ c.con_name in
        match c.con_args with
        | `TUPLE [] -> (* without arguments *)
          Exp.case (constructor name None) set_tag
        | `TUPLE typs -> (* with tuple arguments *)
          let vars_typs = List.mapi (fun i t -> (sprintf "_%d" i, t)) typs in
          let prj = prj_tuple vars_typs in
          let fun_body = encoding_of_tuple_type ~loc ~deriver ~path vars_typs in
          let enc = Exp.sequence set_tag fun_body in
          Exp.case (constructor name (Some prj)) enc
        | `RECORD labels -> (* with record arguments *)
          let enc_record = encoding_of_record_type ~deriver ~path ~loc labels in
          let enc = Exp.sequence set_tag enc_record in
          Exp.case (constructor name (Some (prj_record labels))) enc) in
  [%expr fun _b -> [%e Exp.function_ cases]]

and encoding_of_record_type ~deriver ~path ~loc labels =
  let vars_encoders =
    List.map
      (fun ld ->
         let path = path ^ "." ^ ld.pld_name.txt in
         let encoder = match attr_encoder ~deriver ld.pld_attributes with
           | None -> encoder_of_core_type ~deriver ~path ld.pld_type
           | Some (labels, encoder) ->
             let args = List.map (fun s -> Labelled s, evar s) labels in
             Exp.apply encoder args (* apply pre-decoded record fields *)
         in
         (ld.pld_name.txt, encoder))
      labels in
  encoding_of_compound_type ~loc vars_encoders
