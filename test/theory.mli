open Alcotest

type ('a, 'b) test_data

val make_test_data : string -> 'a -> 'b -> ('a, 'b) test_data

val test_theory :
  string -> ('a, 'b) test_data list -> ('a -> 'b -> unit) -> return test
