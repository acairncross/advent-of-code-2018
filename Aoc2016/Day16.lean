import AocLib
import Std.Data.HashMap

def step (s : String) : String := s ++ "0"

def invert (c : Char) : Char :=
  match c with
  | '1' => '0'
  | '0' => '1'
  | _ => c

def mkChecksum (s : Array Char) : Array Char := Id.run do
  let mut current := s
  while current.size % 2 == 0 do
    let mut next : Array Char := Array.empty
    for chunk in current.toChunks 2 do
      if chunk[0]! == chunk[1]! then
        next := next.push '1'
      else
        next := next.push '0'
    current := next

  return current

def solveParts (seed : String) (diskSize : Nat) : String := Id.run do
  let mut curve := Array.mk seed.data

  while curve.size < diskSize do
    curve := curve.push '0'
    -- Subarray doesn't have a reverse method so reverse the Array first
    for c in curve.reverse.toSubarray (start := 1) do
      curve := curve.push (invert c)

  let checksum := mkChecksum (curve.toSubarray (stop := diskSize)).toArray

  return String.mk checksum.toList

def main : IO Unit := do
  let inputHandle <- IO.FS.Handle.mk "inputs/aoc2016/day16.txt" IO.FS.Mode.read
  let inputStream := IO.FS.Stream.ofHandle inputHandle
  let seed := (<- inputStream.getLine).dropRight 1 -- drop the newline
  IO.println s!"{solveParts seed 272}"
  IO.println s!"{solveParts seed 35651584}"
