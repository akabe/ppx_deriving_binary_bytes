open OUnit2

let suite =
  "ppx_deriving_binary_bytes" >::: [
    "BytesBuffer" >: Test_BytesBuffer.suite;
    "CString" >: Test_CString.suite;
    "PrefixedString" >: Test_PrefixedString.suite;
    "PrefixedList" >: Test_PrefixedList.suite;
    "GreedyList" >: Test_GreedyList.suite;
  ]

let () = run_test_tt_main suite
