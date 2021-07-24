open OUnit2

let suite =
  "ppx_deriving_binary" >::: [
  ]

let () = run_test_tt_main suite
