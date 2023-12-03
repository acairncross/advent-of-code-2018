import AocLib
import Std.Data.HashMap

structure ScanState where
  valid : Bool := false
  digits : Array Int := Array.empty
  stars : Std.HashMap (Nat × Nat) Unit := Std.mkHashMap

instance : ToString ScanState where
  toString
  | { valid, digits, stars } => s!"ScanState \{ valid := {valid}, digits := {digits}, stars := {stars.toArray} }"

def solve(grid : Array (Array Char)) : Int × Int := Id.run do
  let mut part_numbers_total : Int := 0
  let mut star_neighbors : Std.HashMap (Nat × Nat) (Array Int):= Std.mkHashMap
  for y in [:grid.size] do
    let mut state : ScanState := {}
    for x in [:grid[y]!.size] do
      if grid[y]![x]!.isDigit then
        state := { state with digits := state.digits.push grid[y]![x]!.toInt! }
        for i in ([-1, 0, 1] : List Int) do
          for j in ([-1, 0, 1] : List Int) do
            if 0 <= y + i && y + i < grid.size && 0 <= x + j && x + j < grid[y]!.size then
              let c := grid[(y+i).toNat]![(x+j).toNat]!
              if !c.isDigit && c != '.' then
                state := { state with valid := true }
              if c == '*' then
                state := { state with stars := state.stars.insert ((y+i).toNat, (x+j).toNat) () }
        -- dbg_trace state
      else
        if state.valid then
          part_numbers_total := part_numbers_total + Int.ofDigits state.digits
          for (pos, ()) in state.stars.toArray do
            if star_neighbors.contains pos then
              star_neighbors := star_neighbors.modify pos fun _k v => v.push (Int.ofDigits state.digits)
            else
              star_neighbors := star_neighbors.insert pos #[Int.ofDigits state.digits]
        state := {}

    -- end of row
    if state.valid then
      part_numbers_total := part_numbers_total + Int.ofDigits state.digits
      for (pos, ()) in state.stars.toArray do
        if star_neighbors.contains pos then
          star_neighbors := star_neighbors.modify pos fun _k v => v.push (Int.ofDigits state.digits)
        else
          star_neighbors := star_neighbors.insert pos #[Int.ofDigits state.digits]
  let gear_ratios_total := star_neighbors.fold (fun acc _pos neighbors =>
    if h : neighbors.size = 2 then
      have h0 : 0 < neighbors.size := by rw [h]; simp
      have h1 : 1 < neighbors.size := by rw [h]; simp
      acc + (neighbors[0] * neighbors[1])
    else
      acc) 0
  return (part_numbers_total, gear_ratios_total)

def main : IO Unit := do
  let inputHandle <- IO.FS.Handle.mk "inputs/aoc2023/day03.txt" IO.FS.Mode.read
  let inputStream := IO.FS.Stream.ofHandle inputHandle
  let lines <- inputStream.readlines
  let grid : Array (Array Char) :=
    (lines.map fun line => line.data.toArray).toArray
  IO.println s!"{solve grid}"
