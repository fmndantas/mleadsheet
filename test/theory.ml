open Alcotest

type ('a, 'b) test_data = { id : string; input : 'a; expected_output : 'b }

let make_test_data id input expected_output = { id; input; expected_output }

let test_theory (name : string) (ds : ('a, 'b) test_data list)
    (assertFn : 'a -> 'b -> return) : string * return test_case list =
  ( name,
    ds
    |> List.mapi (fun _ case ->
           let test_case_name = "[" ^ case.id ^ "]" in
           test_case test_case_name `Quick (fun () ->
               assertFn case.input case.expected_output)) )
