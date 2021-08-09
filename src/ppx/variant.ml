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

open Ppx_deriving.Ast_convenience
open Ppxlib
open Ppxlib.Ast_helper

type constuctor_arguments =
  [
    | `TUPLE of core_type list
    | `RECORD of label_declaration list
  ]

type constructor =
  {
    con_name : string;
    con_args : constuctor_arguments;
    con_value : constant;
    con_loc : Location.t;
  }

let assert_not_GADT type_decl ocaml_constrs =
  List.iter
    (fun c -> match c.pcd_res with
       | None -> ()
       | Some _ ->
         Ppx_deriving.raise_errorf ~loc:type_decl.ptype_loc
           "ppx_deriving_cstruct does not support GADT: %s.%s"
           type_decl.ptype_name.txt c.pcd_name.txt)
    ocaml_constrs

let get_value ~deriver i attrs =
  Ppx_deriving.(attrs |> attr ~deriver "value" |> Arg.(get_attr ~deriver expr))
  |> function
  | Some { pexp_desc = Pexp_constant const; _ } -> const
  | _ -> Const.int i

(** Converts a list of OCaml constructors into a list of constructors in this module. *)
let constructors_of_ocaml_constructors ~deriver ocaml_constrs =
  let aux i c =
    let value = get_value ~deriver i c.pcd_attributes in
    let args = match c.pcd_args with
      | Pcstr_tuple args -> `TUPLE args
      | Pcstr_record labels -> `RECORD labels in
    {
      con_name = c.pcd_name.txt;
      con_args = args;
      con_value = value;
      con_loc = c.pcd_loc;
    }
  in
  List.mapi aux ocaml_constrs

(** Converts a list of OCaml row_fields into a list of constructors in this module. *)
let constructors_of_ocaml_row_fields ~deriver ocaml_row_fields =
  let aux i rf =
    match rf.prf_desc with
    | Rinherit _ ->
      Ppx_deriving.raise_errorf ~loc:rf.prf_loc
        "ppx_deriving_cstruct does not support inheritance of \
         polymorphic variants"
    | Rtag (label, _, arg_typs) ->
      let value = get_value ~deriver i rf.prf_attributes in
      {
        con_name = label.txt;
        con_args = `TUPLE arg_typs;
        con_value = value;
        con_loc = rf.prf_loc;
      }
  in
  List.mapi aux ocaml_row_fields
