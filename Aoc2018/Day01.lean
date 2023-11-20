import AocLib
import Std.Data.HashMap

def parseChange (line : String) : Int :=
  let sign := match String.front line with
  | '+' => 1
  | _ => -1
  let magnitude := (String.drop line 1).toInt!
  sign * magnitude

def solvePart1 (changes : List Int) := Id.run do
  let mut frequency : Int := 0
  for n in changes do
    frequency := frequency + n
  return frequency

def solvePart2 (changes : List Int) := Id.run do
  let mut frequency : Int := 0
  let mut seen_frequencies : Std.HashMap Int Unit := Std.mkHashMap
  let mut i : Nat := 0

  while not (seen_frequencies.contains frequency) do
    seen_frequencies := seen_frequencies.insert frequency ()
    frequency := frequency + changes[i]!
    i := (i + 1) % changes.length

  return frequency

def main : IO Unit := do
  let inputHandle <- IO.FS.Handle.mk "inputs/aoc2018/day01.txt" IO.FS.Mode.read
  let inputStream := IO.FS.Stream.ofHandle inputHandle
  let changes := (<- inputStream.readlines).map parseChange
  IO.println s!"{solvePart1 changes}"
  IO.println s!"{solvePart2 changes}"
