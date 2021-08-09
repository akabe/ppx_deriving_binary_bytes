open OUnit2

let suite =
  "ppx_deriving_binary" >::: [
    "of_binary_bytes" >: Test_of_binary_bytes.suite;
  ]

let () = run_test_tt_main suite
