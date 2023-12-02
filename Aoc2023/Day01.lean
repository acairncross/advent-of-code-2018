import AocLib

def mkCalibrationValue (a b : Int) : Int :=
  10 * a + b

def digitParsers1 : Array (String × Int) := #[
  ("1", 1),
  ("2", 2),
  ("3", 3),
  ("4", 4),
  ("5", 5),
  ("6", 6),
  ("7", 7),
  ("8", 8),
  ("9", 9)
]

def digitParsers2 : Array (String × Int) := digitParsers1 ++ #[
  ("one", 1),
  ("two", 2),
  ("three", 3),
  ("four", 4),
  ("five", 5),
  ("six", 6),
  ("seven", 7),
  ("eight", 8),
  ("nine", 9)
]

def parseFirstDigit (s : String) (digitParsers : Array (String × Int)) : Int := Id.run do
  for pos in [0:s.length] do
    for (digitString, digitValue) in digitParsers do
      if pos + digitString.length > s.length then
        continue
      else
        let substr := Substring.mk s (String.Pos.mk pos) (String.Pos.mk (pos + digitString.length))
        if substr.toString == digitString then
          return digitValue
        else
          continue
  panic "no calibration value found fwd"

def parseLastDigit (s : String) (digitParsers : Array (String × Int)) : Int := Id.run do
  for pos in [0:s.length] do
    for (digitString, digitValue) in digitParsers do
      if s.length - 1 - pos + digitString.length > s.length then
        continue
      else
        let substr := Substring.mk s (String.Pos.mk (s.length - 1 - pos)) (String.Pos.mk (s.length - 1 - pos + digitString.length))
        if substr == digitString then
          return digitValue
        else
          continue
  panic "no calibration value found bwd"

def main : IO Unit := do
  let inputHandle <- IO.FS.Handle.mk "inputs/aoc2023/day01.txt" IO.FS.Mode.read
  let inputStream := IO.FS.Stream.ofHandle inputHandle
  let lines <- inputStream.readlines
  let part1 := Int.sum (lines.map (fun line => mkCalibrationValue (parseFirstDigit line digitParsers1) (parseLastDigit line digitParsers1)))
  let part2 := Int.sum (lines.map (fun line => mkCalibrationValue (parseFirstDigit line digitParsers2) (parseLastDigit line digitParsers2)))

  IO.println s!"{part1}"
  IO.println s!"{part2}"
