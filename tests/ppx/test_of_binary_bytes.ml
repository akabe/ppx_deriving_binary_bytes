open OUnit2
open Ppx_deriving_binary_runtime.Std

let b_ = Bytes.of_string

let test_of_binary_bytes_uint8 ctxt =
  let b = b_ "\x00\x42" in
  let actual = [%of_binary_bytes: uint8] b 1 in
  let expected = (0x42, 2) in
  assert_equal ~ctxt ~printer:[%show: int * int] expected actual

let test_of_binary_bytes_uint16be ctxt =
  let b = b_ "\x00\x12\x34" in
  let actual = [%of_binary_bytes: uint16be] b 1 in
  let expected = (0x1234, 3) in
  assert_equal ~ctxt ~printer:[%show: int * int] expected actual

let test_of_binary_bytes_uint16le ctxt =
  let b = b_ "\x00\x12\x34" in
  let actual = [%of_binary_bytes: uint16le] b 1 in
  let expected = (0x3412, 3) in
  assert_equal ~ctxt ~printer:[%show: int * int] expected actual

let test_of_binary_bytes_int32be ctxt =
  let b = b_ "\x00\x12\x34\x56\x78" in
  let actual = [%of_binary_bytes: int32be] b 1 in
  let expected = (0x12345678l, 5) in
  assert_equal ~ctxt ~printer:[%show: int32 * int] expected actual

let test_of_binary_bytes_int32le ctxt =
  let b = b_ "\x00\x12\x34\x56\x78" in
  let actual = [%of_binary_bytes: int32le] b 1 in
  let expected = (0x78563412l, 5) in
  assert_equal ~ctxt ~printer:[%show: int32 * int] expected actual

let test_of_binary_bytes_int64be ctxt =
  let b = b_ "\x00\x11\x22\x33\x44\x55\x66\x77\x88" in
  let actual = [%of_binary_bytes: int64be] b 1 in
  let expected = (0x1122334455667788L, 9) in
  assert_equal ~ctxt ~printer:[%show: int64 * int] expected actual

let test_of_binary_bytes_int64le ctxt =
  let b = b_ "\x00\x11\x22\x33\x44\x55\x66\x77\x88" in
  let actual = [%of_binary_bytes: int64le] b 1 in
  let expected = (0x8877665544332211L, 9) in
  assert_equal ~ctxt ~printer:[%show: int64 * int] expected actual

let test_of_binary_bytes_ref ctxt =
  let b = b_ "\x00\x12\x34" in
  let actual = [%of_binary_bytes: uint16le ref] b 1 in
  let expected = (ref 0x3412, 3) in
  assert_equal ~ctxt ~printer:[%show: int ref * int] expected actual

let test_of_binary_bytes_tuple ctxt =
  let b = b_ "\x00\x12\x34\x56" in
  let actual = [%of_binary_bytes: uint16le * uint8] b 1 in
  let expected = ((0x3412, 0x56), 4) in
  assert_equal ~ctxt ~printer:[%show: (int * int) * int] expected actual
  ;
  let b = b_ "\x00Hello\x00\x12\x34" in
  let actual = [%of_binary_bytes: CString.t * uint16le] b 1 in
  let expected = (("Hello", 0x3412), 9) in
  assert_equal ~ctxt ~printer:[%show: (string * int) * int] expected actual

let test_of_binary_bytes_string ctxt =
  let b = b_ "\x00Hello" in
  let actual = [%of_binary_bytes: string [@length 3]] b 1 in
  let expected = ("Hel", 4) in
  assert_equal ~ctxt ~printer:[%show: string * int] expected actual
  ;
  let actual = [%of_binary_bytes: string [@length 5]] b 1 in
  let expected = ("Hello", 6) in
  assert_equal ~ctxt ~printer:[%show: string * int] expected actual

let test_of_binary_bytes_list ctxt =
  let b = b_ "\x00Hello" in
  let actual = [%of_binary_bytes: char list [@length 3]] b 1 in
  let expected = (['H'; 'e'; 'l'], 4) in
  assert_equal ~ctxt ~printer:[%show: char list * int] expected actual
  ;
  let actual = [%of_binary_bytes: char list [@length 5]] b 1 in
  let expected = (['H'; 'e'; 'l'; 'l'; 'o'], 6) in
  assert_equal ~ctxt ~printer:[%show: char list * int] expected actual

let test_of_binary_bytes_polymorphic_variant ctxt =
  let b = b_ "\x00\x1c" in
  let actual = [%of_binary_bytes: [ `A [@value 0x1c] | `B of uint8 * uint16le ] [@base_type: uint8]] b 1 in
  let expected = (`A, 2) in
  assert_equal ~ctxt ~printer:[%show: [ `A | `B of uint8 * uint16le ] * int] expected actual
  ;
  let b = b_ "\x00\x01\x11\x22\x33" in
  let actual = [%of_binary_bytes: [ `A [@value 0x1c] | `B of uint8 * uint16le ] [@base_type: uint8]] b 1 in
  let expected = (`B (0x11, 0x3322), 5) in
  assert_equal ~ctxt ~printer:[%show: [ `A | `B of uint8 * uint16le ] * int] expected actual

type t1 =
  {
    a : uint16le;
    b : uint8;
  }
[@@deriving of_binary_bytes, show]

let test_of_binary_bytes_str_record ctxt =
  let b = b_ "\x00\x12\x34\x56" in
  let actual = [%of_binary_bytes: t1] b 1 in
  let expected = ({ a = 0x3412; b = 0x56 }, 4) in
  assert_equal ~ctxt ~printer:[%show: t1 * int] expected actual

type t2 =
  | Foo [@value 0x42]
  | Bar
[@@base_type: int32lei]
[@@deriving of_binary_bytes, show]

let test_of_binary_bytes_str_variant_noargs ctxt =
  let b = b_ "\x00\x42\x00\x00\x00" in
  let actual = [%of_binary_bytes: t2] b 1 in
  let expected = (Foo, 5) in
  assert_equal ~ctxt ~printer:[%show: t2 * int] expected actual
  ;
  let b = b_ "\x00\x01\x00\x00\x00" in
  let actual = [%of_binary_bytes: t2] b 1 in
  let expected = (Bar, 5) in
  assert_equal ~ctxt ~printer:[%show: t2 * int] expected actual
  ;
  let b = b_ "\x00\x02\x00\x00\x00" in
  assert_raises (Parse_error "Test_of_binary_bytes.t2") (fun () -> [%of_binary_bytes: t2] b 1)

type t3 =
  | Foo of uint8
  | Bar of uint8 * uint16le
[@@base_type: uint16le]
[@@deriving of_binary_bytes, show]

let test_of_binary_bytes_str_variant_tuple ctxt =
  let b = b_ "\x00\x00\x00\x42" in
  let actual = [%of_binary_bytes: t3] b 1 in
  let expected = (Foo 0x42, 4) in
  assert_equal ~ctxt ~printer:[%show: t3 * int] expected actual
  ;
  let b = b_ "\x00\x01\x00\x11\x22\x33" in
  let actual = [%of_binary_bytes: t3] b 1 in
  let expected = (Bar (0x11, 0x3322), 6) in
  assert_equal ~ctxt ~printer:[%show: t3 * int] expected actual

type t4 =
  | Foo of { a : uint8; b : uint16le; }
[@@base_type: uint16le]
[@@deriving of_binary_bytes, show]

let test_of_binary_bytes_str_variant_record ctxt =
  let b = b_ "\x00\x00\x00\x11\x22\x33" in
  let actual = [%of_binary_bytes: t4] b 1 in
  let expected = (Foo { a = 0x11; b = 0x3322 }, 6) in
  assert_equal ~ctxt ~printer:[%show: t4 * int] expected actual

type ('a, 'b) t5 = 'a * 'b
[@@deriving of_binary_bytes, show]

let test_of_binary_bytes_str_parametrized_type ctxt =
  let b = b_ "\x00\x11\x22\x33\x44" in
  let actual = [%of_binary_bytes: (uint16le, uint16le) t5] b 1 in
  let expected = ((0x2211, 0x4433), 5) in
  assert_equal ~ctxt ~printer:[%show: (uint16le, uint16le) t5 * int] expected actual

type t6 = [ `Foo | `Bar of uint16le ] [@base_type: uint16le]
[@@deriving of_binary_bytes, show]

let test_of_binary_bytes_str_poly_variant ctxt =
  let b = b_ "\x00\x00\x00" in
  let actual = [%of_binary_bytes: t6] b 1 in
  let expected = (`Foo, 3) in
  assert_equal ~ctxt ~printer:[%show: t6 * int] expected actual
  ;
  let b = b_ "\x00\x01\x00\x11\x22" in
  let actual = [%of_binary_bytes: t6] b 1 in
  let expected = (`Bar 0x2211, 5) in
  assert_equal ~ctxt ~printer:[%show: t6 * int] expected actual
  ;
  let b = b_ "\x00\x02\x00" in
  assert_raises
    (Parse_error "Test_of_binary_bytes.t6")
    (fun () -> [%of_binary_bytes: t6] b 1)

type t9 =
  {
    foo : uint8;
    baz : uint8 option [@of_binary_bytes
            fun ~foo b i ->
              if foo = 0 then (None, i)
              else let x, i = uint8_of_binary_bytes b i in (Some x, i)];
    hoge : uint8;
  }
[@@deriving of_binary_bytes, show]

let test_of_binary_bytes_str_conditional_field ctxt =
  let b = b_ "\x00\x00\x00" in
  let actual = [%of_binary_bytes: t9] b 1 in
  let expected = ({ foo = 0; baz = None; hoge = 0; }, 3) in
  assert_equal ~ctxt ~printer:[%show: t9 * int] expected actual
  ;
  let b = b_ "\x00\x01\x02\x03" in
  let actual = [%of_binary_bytes: t9] b 1 in
  let expected = ({ foo = 1; baz = Some 2; hoge = 3; }, 4) in
  assert_equal ~ctxt ~printer:[%show: t9 * int] expected actual

type t10 =
  {
    b0 : int;
    b1 : int [@length 2];
    b2 : int;
    b3 : int [@offset 8] [@length 4];
  }
[@@base_type: uint16be]
[@@deriving of_binary_bytes, show]

let test_of_binary_bytes_str_bitfield ctxt =
  let b = b_ "\x00\x00\x01" in
  let actual = [%of_binary_bytes: t10] b 1 in
  let expected = ({
      b0 = 1;
      b1 = 0;
      b2 = 0;
      b3 = 0;
    }, 3) in
  assert_equal ~ctxt ~printer:[%show: t10 * int] expected actual
  ;
  let b = b_ "\x00\x00\x06" in
  let actual = [%of_binary_bytes: t10] b 1 in
  let expected = ({
      b0 = 0;
      b1 = 3;
      b2 = 0;
      b3 = 0;
    }, 3) in
  assert_equal ~ctxt ~printer:[%show: t10 * int] expected actual
  ;
  let b = b_ "\x00\x00\x08" in
  let actual = [%of_binary_bytes: t10] b 1 in
  let expected = ({
      b0 = 0;
      b1 = 0;
      b2 = 1;
      b3 = 0;
    }, 3) in
  assert_equal ~ctxt ~printer:[%show: t10 * int] expected actual
  ;
  let b = b_ "\x00\x00\xf0" in
  let actual = [%of_binary_bytes: t10] b 1 in
  let expected = ({
      b0 = 0;
      b1 = 0;
      b2 = 0;
      b3 = 0;
    }, 3) in
  assert_equal ~ctxt ~printer:[%show: t10 * int] expected actual
  ;
  let b = b_ "\x00\x0f\x00" in
  let actual = [%of_binary_bytes: t10] b 1 in
  let expected = ({
      b0 = 0;
      b1 = 0;
      b2 = 0;
      b3 = 0xf;
    }, 3) in
  assert_equal ~ctxt ~printer:[%show: t10 * int] expected actual

(* Checks that [@@deriving of_binary_bytes] in signature generates a type consistent
   with one in structure by the OCaml type checker. *)
module M : sig
  type 'a t [@@deriving of_binary_bytes]
end = struct
  type 'a t = 'a * uint8 [@@deriving of_binary_bytes]
end

let suite =
  "of_binary_bytes driver" >::: [
    "[%of_binary_bytes: core-type]" >::: [
      "uint8" >:: test_of_binary_bytes_uint8;
      "uint16be" >:: test_of_binary_bytes_uint16be;
      "uint16le" >:: test_of_binary_bytes_uint16le;
      "int32be" >:: test_of_binary_bytes_int32be;
      "int32le" >:: test_of_binary_bytes_int32le;
      "int64be" >:: test_of_binary_bytes_int64be;
      "int64le" >:: test_of_binary_bytes_int64le;
      "ref" >:: test_of_binary_bytes_ref;
      "tuple" >:: test_of_binary_bytes_tuple;
      "string" >:: test_of_binary_bytes_string;
      "list" >:: test_of_binary_bytes_list;
      "polymorphic_variant" >:: test_of_binary_bytes_polymorphic_variant;
    ];
    "[@@deriving of_binary_bytes] in structure" >::: [
      "record" >:: test_of_binary_bytes_str_record;
      "variant without arguments" >:: test_of_binary_bytes_str_variant_noargs;
      "variant with tuple arguments" >:: test_of_binary_bytes_str_variant_tuple;
      "variant with record arguments" >:: test_of_binary_bytes_str_variant_record;
      "parametrized type" >:: test_of_binary_bytes_str_parametrized_type;
      "polymorphic variant" >:: test_of_binary_bytes_str_poly_variant;
      "record (conditional field)" >:: test_of_binary_bytes_str_conditional_field;
      "record (bitfield)" >:: test_of_binary_bytes_str_bitfield;
    ];
  ]
