import Aoc2018
import Std.Data.HashMap

def parseInput (input : String) : List Int :=
  input
  |>.splitOn "\n"
  |> List.filter (fun (line : String) => line.length != 0)
  |> List.map (fun (line : String) =>
    let sign := match String.front line with
    | '+' => 1
    | _ => -1
    let magnitude := (String.drop line 1).toInt!
    sign * magnitude)

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
  let input <- IO.FS.readFile "inputs/day01.txt"
  let changes := parseInput input
  IO.println s!"{solvePart1 changes}"
  IO.println s!"{solvePart2 changes}"
