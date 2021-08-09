open OUnit2
open Ppx_deriving_binary_runtime

let test_of_binary_bytes ctxt =
  let b = Bytes.of_string "\x00\x05HelloABC" in
  let actual = PrefixedString.of_binary_bytes Std.uint8_of_binary_bytes b 1 in
  let expected = ("Hello", 7) in
  assert_equal ~ctxt ~printer:[%show: string * int] expected actual

let test_binary_bytes_of ctxt =
  let b = BytesBuffer.create 1 in
  PrefixedString.binary_bytes_of Std.binary_bytes_of_uint8 b "Hello" ;
  let expected = "\x05Hello" in
  let actual = BytesBuffer.to_string b in
  assert_equal ~ctxt ~printer:[%show: string] expected actual

let suite =
  "PrefixedString" >::: [
    "of_binary_bytes" >:: test_of_binary_bytes;
    "binary_bytes_of" >:: test_binary_bytes_of;
  ]
