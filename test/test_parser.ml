open Alcotest
open Theory
open Mleadsheet.Types

module TestableNoteName = struct
  include NoteName

  let pp fmt = function
    | C -> Format.fprintf fmt "C"
    | D -> Format.fprintf fmt "D"
    | E -> Format.fprintf fmt "E"
    | F -> Format.fprintf fmt "F"
    | G -> Format.fprintf fmt "G"
    | A -> Format.fprintf fmt "A"
    | B -> Format.fprintf fmt "B"

  let equal = ( = )
end

module TestableClef = struct
  include Clef

  let pp fmt = function
    | F -> Format.fprintf fmt "F"
    | G -> Format.fprintf fmt "G"

  let equal = ( = )
end

let assert_parser (input : string) (parser : 'a Angstrom.t)
    (assertFn : 'a -> unit) =
  match Angstrom.parse_string ~consume:All parser input with
  | Ok v -> assertFn v
  | Error msg -> failwith msg

let test_note_name_parsing =
  let t = testable TestableNoteName.pp TestableNoteName.equal in
  test_theory "note name parsing"
    [
      make_test_data "C" "c" TestableNoteName.C;
      make_test_data "D" "d" TestableNoteName.D;
      make_test_data "E" "e" TestableNoteName.E;
      make_test_data "F" "f" TestableNoteName.F;
      make_test_data "G" "g" TestableNoteName.G;
      make_test_data "A" "a" TestableNoteName.A;
      make_test_data "B" "b" TestableNoteName.B;
    ]
    (fun input expected_output ->
      assert_parser input Mleadsheet.Parser.parse_note_name (fun result ->
          check t "note name" expected_output result))

let test_clef_parsing =
  let t = testable TestableClef.pp TestableClef.equal in
  test_theory "clef parsing"
    [
      make_test_data "F" "f" TestableClef.F;
      make_test_data "G" "g" TestableClef.G;
    ]
    (fun input expected_output ->
      assert_parser input Mleadsheet.Parser.parse_clef (fun result ->
          check t "clef" expected_output result))

let suite : return test list = [ test_note_name_parsing; test_clef_parsing ]
