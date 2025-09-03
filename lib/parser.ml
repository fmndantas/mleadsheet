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
