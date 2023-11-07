import «Aoc2018»

def solvePart1 (input : String) := Id.run do
  let mut frequency : Int := 0

  for line in input.splitOn "\n" do
    if line.length == 0 then continue
    let sign := match String.front line with
    | '+' => 1
    | _ => -1
    let magnitude := (String.drop line 1).toInt!
    let n := sign * magnitude
    frequency := frequency + n

  return frequency

def main : IO Unit := do
  let input <- IO.FS.readFile "inputs/day01.txt"
  IO.println s!"{solvePart1 input}"
