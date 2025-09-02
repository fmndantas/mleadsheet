open Alcotest
open Theory
open Mleadsheet.Types

module TestableNoteName = struct
  include NoteName

  let pp fmt = function
    | C -> Format.fprintf fmt "C"
    | D -> Format.fprintf fmt "D"

  let equal = ( = )
end

module Sut = struct
  include Mleadsheet.Parser
end

let test_note_name_parsing =
  let t = testable TestableNoteName.pp TestableNoteName.equal in
  test_theory "note name parsing"
    [
      make_test_data "C" "c" TestableNoteName.C;
      make_test_data "D" "d" TestableNoteName.D;
    ]
    (fun input expected_output ->
      input |> Sut.parse_note_name |> check t "note name" expected_output)

let suite : return test list = [ test_note_name_parsing ]
