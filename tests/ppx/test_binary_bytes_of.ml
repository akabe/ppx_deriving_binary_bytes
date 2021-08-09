open Format
open OUnit2
open Ppx_deriving_binary_runtime.Std

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
  ]
