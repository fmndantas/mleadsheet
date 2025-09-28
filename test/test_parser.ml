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

module TestableDuration = struct
  include Duration

  let pp fmt = function
    | Whole -> Format.fprintf fmt "Whole"
    | WholeDotted -> Format.fprintf fmt "Whole dotted"
    | Half -> Format.fprintf fmt "Half"
    | HalfDotted -> Format.fprintf fmt "Half dotted"
    | Quarter -> Format.fprintf fmt "Quarter"
    | QuarterDotted -> Format.fprintf fmt "Quarter dotted"
    | Eighth -> Format.fprintf fmt "Eighth"
    | EighthDotted -> Format.fprintf fmt "Eighth dotted"
    | Sixteenth -> Format.fprintf fmt "Sixteenth"
    | SixteenthDotted -> Format.fprintf fmt "Sixteenth dotted"

  let equal = ( = )
end

module TestableNote = struct
  type t = note

  let pp fmt = Format.fprintf fmt
  let equal = ( = )
end

(* TODO: ugly! *)
let assert_parser (assert_fn : 'a * 'a -> unit) (parser : 'a Angstrom.t)
    (input : string) (expected_output : 'a) =
  match Angstrom.parse_string ~consume:All parser input with
  | Ok v -> assert_fn (expected_output, v)
  | Error msg -> failwith msg

(* TODO: add sharp and flat *)
let test_note_name_parsing =
  let t = testable TestableNoteName.pp TestableNoteName.equal in
  test_theory "note name parsing"
    [
      make_test_data "C" "c" NoteName.C;
      make_test_data "D" "d" NoteName.D;
      make_test_data "E" "e" NoteName.E;
      make_test_data "F" "f" NoteName.F;
      make_test_data "G" "g" NoteName.G;
      make_test_data "A" "a" NoteName.A;
      make_test_data "B" "b" NoteName.B;
    ]
    (assert_parser
       (fun (expected_output, result) ->
         check t "note name" expected_output result)
       Mleadsheet.Parser.parse_note_name)

let test_clef_parsing =
  let t = testable TestableClef.pp TestableClef.equal in
  test_theory "clef parsing"
    [
      make_test_data "F" "f" TestableClef.F;
      make_test_data "G" "g" TestableClef.G;
    ]
    (assert_parser
       (fun (expected_output, result) -> check t "clef" expected_output result)
       Mleadsheet.Parser.parse_clef)

let test_duration_parsing =
  let t = testable TestableDuration.pp TestableDuration.equal in
  test_theory "duration parsing"
    [
      make_test_data "whole note" "1" Duration.Whole;
      make_test_data "dotted whole note" "1." Duration.WholeDotted;
      make_test_data "half note" "2" Duration.Half;
      make_test_data "dotted half note" "2." Duration.HalfDotted;
      make_test_data "quarter note" "4" Duration.Quarter;
      make_test_data "dotted quarter note" "4." Duration.QuarterDotted;
      make_test_data "eighth note" "8" Duration.Eighth;
      make_test_data "dotted eighth note" "8." Duration.EighthDotted;
      make_test_data "sixteenth note" "16" Duration.Sixteenth;
      make_test_data "dotted sixteenth note" "16." Duration.SixteenthDotted;
    ]
    (assert_parser
       (fun (expected_output, result) ->
         check t "duration" expected_output result)
       Mleadsheet.Parser.parse_duration)

module M = struct
  open Angstrom

  let lift (s0 : 's) (p : 'a Angstrom.t) : ('a * 's) Angstrom.t =
    p >>| fun x -> (x, s0)

  let update_state (f : 's -> 's) : 'a * 's -> ('a * 's) Angstrom.t =
   fun (v, s0) -> Angstrom.return (v, f s0)
end

let test_lift () =
  let p0 = Angstrom.char 'f' in
  let p1 = M.lift "this is the state" p0 in
  match Angstrom.parse_string ~consume:All p1 "f" with
  | Ok (_, s) -> check string "foo" "this is the state" s
  | Error _ -> failwith "todo"

let test_update_state () =
  let p0 = Angstrom.char 'f' in
  let p1 = M.lift "this is the state" p0 in
  let (p2 : (char * string) Angstrom.t) =
    Angstrom.bind p1 ~f:(M.update_state (fun s0 -> s0 ^ ", but updated"))
  in
  match Angstrom.parse_string ~consume:All p2 "f" with
  | Ok (_, s) -> check string "foo" "this is the state, but updated" s
  | Error _ -> failwith "todo"

let suite : return test list =
  [
    (* test_note_name_parsing; *)
    (* test_clef_parsing; *)
    (* test_duration_parsing; *)
    ( "test_foo",
      [
        test_case "lift" `Quick test_lift;
        test_case "update state" `Quick test_update_state;
      ] );
  ]
