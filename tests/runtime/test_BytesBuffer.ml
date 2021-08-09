open Format
open OUnit2
open Ppx_deriving_binary_runtime

let printer = asprintf "%a" Printer.pp_string_hex

let test_set_uint8 ctxt =
  let buf = BytesBuffer.create 1 in
  BytesBuffer.set_uint8 buf 0 0xff ;
  assert_equal ~ctxt ~printer "\xff" (BytesBuffer.to_string buf)
  ;
  let buf = BytesBuffer.create 1 in
  BytesBuffer.set_uint8 buf 3 0xff ;
  assert_equal ~ctxt ~printer "\x00\x00\x00\xff" (BytesBuffer.to_string buf)

let test_set_int32le ctxt =
  let buf = BytesBuffer.create 1 in
  BytesBuffer.set_int32le buf 0 0x11223344l ;
  assert_equal ~ctxt ~printer "\x44\x33\x22\x11" (BytesBuffer.to_string buf)
  ;
  let buf = BytesBuffer.create 1 in
  BytesBuffer.set_int32le buf 2 0x11223344l ;
  assert_equal ~ctxt ~printer "\x00\x00\x44\x33\x22\x11" (BytesBuffer.to_string buf)

let suite =
  "BytesBuffer" >::: [
    "set_uint8" >:: test_set_uint8;
    "set_int32le" >:: test_set_int32le;
  ]
