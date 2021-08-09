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

let rec decoder_of_core_type ~deriver ~path typ =
  let loc = typ.ptyp_loc in
  match typ with
  | [%type: unit] -> [%expr fun _ _i -> ((), _i)]
  | [%type: string]
  | [%type: String.t] ->
    decoder_of_string_like_type ~deriver typ
      ~func:[%expr Ppx_deriving_binary_bytes_runtime.Std.string_of_binary_bytes]
  | [%type: bytes]
  | [%type: Bytes.t] ->
    decoder_of_string_like_type ~deriver typ
      ~func:[%expr Ppx_deriving_binary_bytes_runtime.Std.bytes_of_binary_bytes]
  | [%type: [%t? elt] list] ->
    decoder_of_list_like_type ~deriver ~path typ elt
      ~func:[%expr Ppx_deriving_binary_bytes_runtime.Std.list_of_binary_bytes]
  | [%type: [%t? elt] array] ->
    decoder_of_list_like_type ~deriver ~path typ elt
      ~func:[%expr Ppx_deriving_binary_bytes_runtime.Std.array_of_binary_bytes]
  | [%type: [%t? typ] ref] ->
    [%expr fun _b _i ->
      let _x, _i = [%e decoder_of_core_type ~deriver ~path typ] _b _i in
      (ref _x, _i)]
  | { ptyp_desc = Ptyp_constr (lid, args); _ } ->
    let f = Exp.ident (mknoloc (Ppx_deriving.mangle_lid affix lid.txt)) in
    let args' = List.map (decoder_of_core_type ~deriver ~path) args in
    let fwd = app f args' in
    [%expr fun _b _i -> [%e fwd] _b _i]
  | { ptyp_desc = Ptyp_tuple typs; _ } ->
    decoder_of_tuple ~deriver ~path ~constructor:(fun exps -> Exp.tuple exps) ~loc typs
  | { ptyp_desc = Ptyp_var name; _ } ->
    [%expr ([%e evar ("poly_" ^ name)] : bytes -> int -> _ * int)]
  | { ptyp_desc = Ptyp_variant (rows, Closed, None); _ } ->
    decoder_of_polymorphic_variant ~deriver ~path ~loc rows typ.ptyp_attributes
  (* Errors *)
  | _ ->
    Ppx_deriving.raise_errorf ~loc "%s cannot be derived for %s"
      deriver (Ppx_deriving.string_of_core_type typ)

and decoder_of_string_like_type ~deriver ~func t =
  let loc = t.ptyp_loc in
  let len = Astmisc.attr_length_exn ~loc ~deriver t.ptyp_attributes in
  [%expr [%e func] ~n:[%e Astmisc.eint len]]

and decoder_of_list_like_type ~deriver ~path ~func t elt =
  let decoder = decoder_of_core_type ~deriver ~path elt in
  let loc = t.ptyp_loc in
  let len = Astmisc.attr_length_exn ~loc ~deriver t.ptyp_attributes in
  [%expr [%e func] ~n:[%e Astmisc.eint len] [%e decoder]]

and decoder_of_tuple ~deriver ~path ~constructor ~loc typs =
  let vars_typs = List.mapi (fun i t -> (sprintf "_%d" i, t)) typs in
  let tuple = constructor (List.map (fun (s, _) -> evar s) vars_typs) in
  let body =
    List.fold_right
      (fun (x, t) k' ->
         let loc = t.ptyp_loc in
         let decoder = decoder_of_core_type ~deriver ~path t in
         [%expr let ([%p pvar x], _i) = [%e decoder] _b _i in [%e k']])
      vars_typs [%expr ([%e tuple], _i)] in
  [%expr fun _b _i -> [%e body]]

and decoder_of_polymorphic_variant
    ~deriver
    ~path
    ~loc
    row_fields attrs
  =
  let base_type = Astmisc.attr_base_type_exn ~deriver ~loc attrs in
  Variant.constructors_of_ocaml_row_fields ~deriver row_fields
  |> decoder_of_constructors
    ~deriver ~path ~base_type ~loc
    ~constructor:(fun name -> Exp.variant name)

and decoder_of_constructors
    ~deriver
    ~path
    ~base_type
    ~loc
    ~constructor
    (constrs : Variant.constructor list)
  =
  let case_of_constructor c =
    let loc = c.con_loc in
    let name = c.Variant.con_name in
    let tag = Pat.constant c.Variant.con_value in
    let path' = path ^ "." ^ c.con_name in
    let decoder =
      match c.con_args with
      | `TUPLE typs -> (* C (arg1, ..., argN) *)
        decoder_of_tuple
          ~deriver ~path:path' ~loc typs
          ~constructor:(function
              | [] -> constructor name None
              | [_] -> constructor name (Some [%expr _0])
              | exps -> constructor name (Some (Exp.tuple exps)))
      | `RECORD labels -> (* C { label1: arg1; ...; labelN: argN; } *)
        decoder_of_record
          ~deriver ~path:path' ~loc:c.con_loc labels
          ~constructor:(fun args -> constructor name (Some args))
    in
    Exp.case tag [%expr [%e decoder] _b _i]
  in
  let cases = List.map case_of_constructor constrs in
  let err_mesg = Astmisc.estring ~loc path in
  let raise_ = [%expr raise (Ppx_deriving_binary_bytes_runtime.Std.Parse_error [%e err_mesg])] in
  let cases = cases @ [Exp.case [%pat? _] raise_] in
  let base_decoder = decoder_of_core_type ~deriver ~path base_type in
  [%expr
    fun _b _i ->
      let _x, _i = [%e base_decoder] _b _i in
      [%e Exp.match_ [%expr _x] cases]]

and decoder_of_record ~deriver ~path ~constructor ~loc labels =
  let record = constructor (erecord labels) in
  let body =
    List.fold_right
      (fun ld k' ->
         let loc = ld.pld_type.ptyp_loc in
         let path = path ^ "." ^ ld.pld_name.txt in
         let decoder =
           match attr_decoder ~deriver ld.pld_attributes with
           | None ->
             decoder_of_core_type ~deriver ~path ld.pld_type
           | Some (labels, decoder) -> (* a decoder function is given by a user. *)
             let args = List.map (fun s -> Labelled s, evar s) labels in
             Exp.apply decoder args (* apply pre-decoded record fields *)
         in
         let field_name = pvar ld.pld_name.txt in
         [%expr let ([%p field_name], _i) = [%e decoder] _b _i in [%e k']])
      labels
      [%expr ([%e record], _i)] in
  [%expr fun _b _i -> [%e body]]

let decoder_of_variant ~deriver ~path ~loc type_decl constrs attrs =
  Variant.assert_not_GADT type_decl constrs ;
  let base_type = Astmisc.attr_base_type_exn ~deriver ~loc attrs in
  Variant.constructors_of_ocaml_constructors ~deriver constrs
  |> decoder_of_constructors
    ~deriver
    ~path
    ~base_type
    ~loc
    ~constructor:(fun name -> Exp.construct (Astmisc.mklid name))

let mk_record labels =
  let label_bindings =
    List.map
      (fun ld -> (Astmisc.mklid ld.pld_name.txt, evar ld.pld_name.txt))
      labels in
  Exp.record label_bindings None

let decoder_of_record_bitfield ~deriver ~path ~loc base_type labels =
  let open Bitfield in
  let mk_rec = mk_record labels in
  let fields = Bitfield.of_ocaml_label_declarations ~deriver labels in
  let dec =
    List.fold_right
      (fun field acc ->
         let name = pvar field.rbf_name in
         let loc = field.rbf_loc in
         let ofs = Exp.constant (Const.int field.rbf_offset) in
         let mask = Exp.constant (Const.int field.rbf_mask) in
         let rhs = [%expr (_x lsr [%e ofs]) land [%e mask]] in
         [%expr let [%p name] = [%e rhs] in [%e acc]])
      fields mk_rec in
  let base_decoder = decoder_of_core_type ~deriver ~path base_type in
  [%expr
    fun _cs _i ->
      let _x, _i = [%e base_decoder] _cs _i in
      ([%e dec], _i)]

let str_decoder_of_type_decl ~deriver ~path type_decl =
  let loc = type_decl.ptype_loc in
  let path = String.concat "." path ^ "." ^ type_decl.ptype_name.txt in
  let decoder = match type_decl.ptype_kind with
    (* Record type declaration: *)
    | Ptype_record labels ->
      begin
        match Astmisc.attr_base_type ~deriver type_decl.ptype_attributes with
        | None ->
          decoder_of_record
            ~deriver ~path ~loc:type_decl.ptype_loc labels
            ~constructor:(fun x -> x)
        | Some base_type ->
          decoder_of_record_bitfield ~deriver ~path ~loc base_type labels
      end
    (* (Non-polymorphics) varia nt type declarations: *)
    | Ptype_variant constrs ->
      decoder_of_variant ~deriver ~path ~loc type_decl constrs type_decl.ptype_attributes
    (* Other types: *)
    | Ptype_abstract | Ptype_open ->
      match type_decl.ptype_manifest with
      | Some typ -> decoder_of_core_type ~deriver ~path typ
      | None ->
        Ppx_deriving.raise_errorf
          "ppx_deriving_binary_bytes does not support empty types: %s"
          type_decl.ptype_name.txt in
  (* Converts type parameters into function parameters *)
  Astmisc.parametrize_expression type_decl.ptype_params decoder

let type_decl_str ~deriver ~options:_ ~path type_decls =
  [Astmisc.create_str_value
     ~mkexp:(str_decoder_of_type_decl ~deriver ~path)
     affix type_decls]

let sig_decoder_of_type_decl type_decl =
  Astmisc.create_sig_value
    affix type_decl
    ~mktype:(fun t ->
        let loc = t.ptyp_loc in
        [%type: Bytes.t -> int -> [%t t] * int])

let type_decl_sig ~options:_ ~path:_ type_decls =
  List.map (sig_decoder_of_type_decl) type_decls
