open OUnit2

let suite =
  "ppx_deriving_binary_bytes" >::: [
    "of_binary_bytes" >: Test_of_binary_bytes.suite;
    "binary_bytes_of" >: Test_binary_bytes_of.suite;
  ]

let () = run_test_tt_main suite
