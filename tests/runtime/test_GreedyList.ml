open OUnit2
open Ppx_deriving_binary_runtime

let test_of_binary_bytes ctxt =
  let b = Bytes.of_string "\x00\x11\x11\x22\x22\x33\x33\x44" in
  let actual = GreedyList.of_binary_bytes Std.uint16be_of_binary_bytes b 1 in
  let expected = ([0x1111; 0x2222; 0x3333], 7) in
  assert_equal ~ctxt ~printer:[%show: int list * int] expected actual

let test_binary_bytes_of ctxt =
  let b = BytesBuffer.create 1 in
  GreedyList.binary_bytes_of Std.binary_bytes_of_uint16be b [0x1111; 0x2222; 0x3333] ;
  let expected = "\x11\x11\x22\x22\x33\x33" in
  let actual = BytesBuffer.to_string b in
  assert_equal ~ctxt ~printer:[%show: string] expected actual

let suite =
  "GreedyList" >::: [
    "of_binary_bytes" >:: test_of_binary_bytes;
    "binary_bytes_of" >:: test_binary_bytes_of;
  ]
