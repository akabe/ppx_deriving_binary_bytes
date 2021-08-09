open OUnit2
open Ppx_deriving_binary_runtime

let test_of_binary_bytes ctxt =
  let b = Bytes.of_string "\x00Hello\x00ABC" in
  let actual = CString.of_binary_bytes b 1 in
  let expected = ("Hello", 7) in
  assert_equal ~ctxt ~printer:[%show: string * int] expected actual

let test_binary_bytes_of ctxt =
  let buf = BytesBuffer.create 1 in
  CString.binary_bytes_of buf "Hello" ;
  let expected = "Hello\x00" in
  let actual = BytesBuffer.to_string buf in
  assert_equal ~ctxt ~printer:[%show: string] expected actual

let suite =
  "CString" >::: [
    "of_binary_bytes" >:: test_of_binary_bytes;
    "binary_bytes_of" >:: test_binary_bytes_of;
  ]
