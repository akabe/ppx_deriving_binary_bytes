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

let mklid ?(loc = Location.none) s = { loc; txt = Longident.Lident s }

let pint ?loc ?suffix x = Pat.constant ?loc (Const.int ?suffix x)
let eint ?loc ?suffix x = Exp.constant ?loc (Const.int ?suffix x)
let estring ?loc x = Exp.constant ?loc (Const.string x)

let attr_type ~deriver attrs =
  let open Ppx_deriving in
  match attr ~deriver "type" attrs with
  | Some { attr_payload = PTyp core_type; _ } -> Some core_type
  | _ -> None

let attr_tag_type ~deriver attrs =
  let open Ppx_deriving in
  match attr ~deriver "tag_type" attrs with
  | Some { attr_payload = PTyp core_type; _ } -> Some core_type
  | _ -> None

let attr_tag_type_exn ~deriver ~loc attrs =
  let open Ppx_deriving in
  match attr_tag_type ~deriver attrs with
  | Some core_type -> core_type
  | None ->
    Ppx_deriving.raise_errorf ~loc
      "ppx_deriving_binary_bytes requires [@tag_type: t] for variants or polymorphic variants"

let attr_length ~deriver attrs =
  Ppx_deriving.(attrs |> attr ~deriver "length" |> Arg.(get_attr ~deriver int))

let attr_length_exn ~deriver ~loc attrs =
  match attr_length ~deriver attrs with
  | Some x -> x
  | None ->
    Ppx_deriving.raise_errorf ~loc
      "ppx_deriving_binary_bytes requires [@length] for string, bytes, list and array"

let attr_offset ~deriver attrs =
  Ppx_deriving.(attrs |> attr ~deriver "offset" |> Arg.(get_attr ~deriver int))

(** Collect labelled arguments from an expression of
    form [fun ~lb1 ~lb2 ... -> ...]. *)
let get_labelled_args_from_fun efun =
  let rec aux acc e = match e.pexp_desc with
    | Pexp_fun (Labelled s, _, _, e') -> aux (s :: acc) e'
    | _ -> List.rev acc in
  aux [] efun

let parametrize_expression type_params expr =
  List.fold_right (fun (typ, _) acc ->
      let loc = typ.ptyp_loc in
      let ppar = match typ.ptyp_desc with
        | Ptyp_any -> Pat.any ()
        | Ptyp_var s -> Pat.var (mknoloc ("poly_" ^ s))
        | _ ->
          Ppx_deriving.raise_errorf "Unexpected type parameter: %s"
            (Ppx_deriving.string_of_core_type typ) in
      [%expr fun [%p ppar] -> [%e acc]])
    type_params expr

let create_str_value ~mkexp affix type_decls =
  let value_bindings =
    List.map
      (fun type_decl ->
         let body = mkexp type_decl in
         let fun_name = Ppx_deriving.mangle_type_decl affix type_decl in
         Vb.mk ~loc:type_decl.ptype_loc (pvar fun_name) body)
      type_decls in
  let recflag =
    match type_decls with
    | [] | [_] -> Nonrecursive
    | _ -> Recursive (* maybe mutual recursion is required *) in
  Str.value recflag value_bindings

let create_sig_value ~mktype affix type_decl =
  let loc = type_decl.ptype_loc in
  let typename = mklid ~loc type_decl.ptype_name.txt in
  let return_t = Typ.constr typename (List.map fst type_decl.ptype_params) in
  let fun_name = Ppx_deriving.mangle_type_decl affix type_decl in
  List.fold_right
    (fun (t, _) acc -> [%type: ([%t mktype t]) -> [%t acc]])
    type_decl.ptype_params
    (mktype return_t)
  |> Val.mk ~loc (mkloc fun_name loc)
  |> Sig.value ~loc
