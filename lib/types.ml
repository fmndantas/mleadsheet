module NoteName = struct
  type t = C | D | E | F | G | A | B
end

module Clef = struct
  type t = F | G
end

module Duration = struct
  type t =
    | Whole
    | WholeDotted
    | Half
    | HalfDotted
    | Quarter
    | QuarterDotted
    | Eighth
    | EighthDotted
    | Sixteenth
    | SixteenthDotted
end
