open Format
open OUnit2
open Ppx_deriving_binary_bytes_runtime.Std

let b_ = Bytes.of_string

let test_binary_bytes_of_uint8 ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: uint8] b 0x42 ;
  let expected = b_ "\x42" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

let test_binary_bytes_of_uint16be ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: uint16be] b 0x1234 ;
  let expected = b_ "\x12\x34" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

let test_binary_bytes_of_uint16le ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: uint16le] b 0x1234 ;
  let expected = b_ "\x34\x12" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

let test_binary_bytes_of_int32be ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: int32be] b 0x12345678l ;
  let expected = b_ "\x12\x34\x56\x78" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

let test_binary_bytes_of_int32le ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: int32le] b 0x12345678l ;
  let expected = b_ "\x78\x56\x34\x12" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

let test_binary_bytes_of_ref ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: uint16be ref] b (ref 0x1234) ;
  let expected = b_ "\x12\x34" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

let test_binary_bytes_of_tuple ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: uint16be * uint8] b (0x1234, 0x56) ;
  let expected = b_ "\x12\x34\x56" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

let test_binary_bytes_of_string ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: string [@length 3]] b "Hello" ;
  let expected = b_ "Hel" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual
  ;
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: string [@length 8]] b "Hello" ;
  let expected = b_ "Hello\x00\x00\x00" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

let test_binary_bytes_of_list ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: uint16le list [@length 3]] b [0x1122; 0x3344; 0x5566] ;
  let expected = b_ "\x22\x11\x44\x33\x66\x55" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual
  ;
  assert_raises
    (Invalid_argument "inconsistent length of list")
    (fun () -> [%binary_bytes_of: uint16le list [@length 3]] b [0x1122; 0x3344])
  ;
  assert_raises
    (Invalid_argument "inconsistent length of list")
    (fun () -> [%binary_bytes_of: uint16le list [@length 3]] b [0x1122; 0x3344; 0x1122; 0x3344])

let test_binary_bytes_of_polymorphic_variant ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: [ `A [@value 0x1c] | `B of uint8 * uint16le ] [@base_type: uint8]] b `A ;
  let expected = b_ "\x1c" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual
  ;
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: [ `A [@value 0x1c] | `B of uint8 * uint16le ] [@base_type: uint8]] b (`B (0x11, 0x3322)) ;
  let expected = b_ "\x01\x11\x22\x33" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

type t1 =
  {
    a : uint16be;
    b : uint8;
  }
[@@deriving binary_bytes_of]

let test_binary_bytes_of_record ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: t1] b { a = 0x1234; b = 0x56; } ;
  let expected = b_ "\x12\x34\x56" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

type t2 =
  | Foo [@value 0x42]
  | Bar
[@@base_type: int32lei]
[@@deriving binary_bytes_of]

let test_binary_bytes_of_variant_noarg ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: t2] b Foo ;
  let expected = b_ "\x42\x00\x00\x00" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual
  ;
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: t2] b Bar ;
  let expected = b_ "\x01\x00\x00\x00" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

type t3 =
  | Foo of uint8
  | Bar of uint16le * uint8
[@@base_type: int32lei]
[@@deriving binary_bytes_of]

let test_binary_bytes_of_variant_tuple ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: t3] b (Foo 0x42) ;
  let expected = b_ "\x00\x00\x00\x00\x42" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual
  ;
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: t3] b (Bar (0x1122, 0x33)) ;
  let expected = b_ "\x01\x00\x00\x00\x22\x11\x33" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

type t4 =
  | Foo of { a : uint8; b : uint16le; }
[@@base_type: uint16le]
[@@deriving binary_bytes_of]

let test_binary_bytes_of_variant_record ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: t4] b (Foo { a = 0x11; b = 0x2233 }) ;
  let expected = b_ "\x00\x00\x11\x33\x22" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

type ('a, 'b) t5 = 'a * 'b
[@@deriving binary_bytes_of]

let test_binary_bytes_of_str_parametrized_type ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: (uint8, uint16le) t5] b (0x11, 0x2233) ;
  let expected = b_ "\x11\x33\x22" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

type t6 = [ `Foo | `Bar of uint16le ]
          [@base_type: uint16le]
[@@deriving binary_bytes_of]

let test_of_binary_bytes_str_poly_variant ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: t6] b `Foo ;
  let expected = b_ "\x00\x00" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual
  ;
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: t6] b (`Bar 0x2211) ;
  let expected = b_ "\x01\x00\x11\x22" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

type t9 =
  {
    foo : uint8;
    baz : uint8 [@binary_bytes_of
            fun ~foo b x ->
              binary_bytes_of_uint8 b (if foo = 0 then 0x42 else x)];
    hoge : uint8;
  }
[@@deriving binary_bytes_of]

let test_binary_bytes_of_str_conditional_field ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: t9] b { foo = 0; baz = 0; hoge = 0; } ;
  let expected = b_ "\x00\x42\x00" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual
  ;
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: t9] b { foo = 1; baz = 2; hoge = 3; } ;
  let expected = b_ "\x01\x02\x03" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

type t10 =
  {
    b0 : int;
    b1 : int [@length 2];
    b2 : int;
    b3 : int [@offset 8] [@length 4];
  }
[@@base_type: uint16be]
[@@deriving binary_bytes_of, show]

let test_binary_bytes_of_str_bitfield ctxt =
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: t10] b { b0 = 1; b1 = 0; b2 = 0; b3 = 0; } ;
  let expected = b_ "\x00\x01" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual
  ;
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: t10] b { b0 = 0; b1 = 3; b2 = 0; b3 = 0; } ;
  let expected = b_ "\x00\x06" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual
  ;
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: t10] b { b0 = 0; b1 = 0; b2 = 1; b3 = 0; } ;
  let expected = b_ "\x00\x08" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual
  ;
  let b = BytesBuffer.create 1 in
  [%binary_bytes_of: t10] b { b0 = 0; b1 = 0; b2 = 0; b3 = 0xf; } ;
  let expected = b_ "\x0f\x00" in
  let actual = BytesBuffer.contents b in
  assert_equal ~ctxt ~printer:[%show: bytes] expected actual

(* Checks that [@@deriving binary_bytes_of] in signature generates a type consistent
   with one in structure by the OCaml type checker. *)
module M : sig
  type 'a t [@@deriving binary_bytes_of]
end = struct
  type 'a t = 'a * uint8 [@@deriving binary_bytes_of]
end

let suite =
  "binary_bytes_of driver" >::: [
    "[%binary_bytes_of core-type]" >::: [
      "uint8" >:: test_binary_bytes_of_uint8;
      "uint16be" >:: test_binary_bytes_of_uint16be;
      "uint16le" >:: test_binary_bytes_of_uint16le;
      "uint32be" >:: test_binary_bytes_of_int32be;
      "uint32le" >:: test_binary_bytes_of_int32le;
      "ref" >:: test_binary_bytes_of_ref;
      "tuple" >:: test_binary_bytes_of_tuple;
      "string" >:: test_binary_bytes_of_string;
      "list" >:: test_binary_bytes_of_list;
      "polymorphic variant" >:: test_binary_bytes_of_polymorphic_variant;
    ];
    "[@@deriving binary_bytes_of] in structure" >::: [
      "record" >:: test_binary_bytes_of_record;
      "variant without arguments" >:: test_binary_bytes_of_variant_noarg;
      "variant with tuple arguments" >:: test_binary_bytes_of_variant_tuple;
      "variant with record arguments" >:: test_binary_bytes_of_variant_record;
      "parametrized types" >:: test_binary_bytes_of_str_parametrized_type;
      "polymorphic variant" >:: test_of_binary_bytes_str_poly_variant;
      "record (conditional field)" >:: test_binary_bytes_of_str_conditional_field;
      "record (bitfield)" >:: test_binary_bytes_of_str_bitfield;
    ];
  ]
