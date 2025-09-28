open Angstrom
open Types

let (parse_note_name : NoteName.t t) =
  [ "c"; "d"; "e"; "f"; "g"; "a"; "b" ] |> List.map string |> choice
  >>| function
  | "c" -> NoteName.C
  | "d" -> NoteName.D
  | "e" -> NoteName.E
  | "f" -> NoteName.F
  | "g" -> NoteName.G
  | "a" -> NoteName.A
  | "b" -> NoteName.B
  | v -> Printf.sprintf "Unknown note name: \"%s\"" v |> failwith

let (parse_clef : Clef.t t) =
  [ "f"; "g" ] |> List.map string |> choice >>| function
  | "f" -> Clef.F
  | "g" -> Clef.G
  | v -> Printf.sprintf "Unknown clef: \"%s\"" v |> failwith

let (parse_duration : Duration.t t) =
  [ "16."; "16"; "8."; "8"; "4."; "4"; "2."; "2"; "1."; "1" ]
  |> List.map string |> choice
  >>| function
  | "1" -> Duration.Whole
  | "1." -> Duration.WholeDotted
  | "2" -> Duration.Half
  | "2." -> Duration.HalfDotted
  | "4" -> Duration.Quarter
  | "4." -> Duration.QuarterDotted
  | "8" -> Duration.Eighth
  | "8." -> Duration.EighthDotted
  | "16" -> Duration.Sixteenth
  | "16." -> Duration.SixteenthDotted
  | v -> Printf.sprintf "Unknown duration: \"%s\"" v |> failwith

let (parse_note: note t) = failwith "todo"
