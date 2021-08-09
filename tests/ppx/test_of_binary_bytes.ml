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
    ]
  ]
