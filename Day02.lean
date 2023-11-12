import Aoc2018
import Std.Data.HashMap

structure TwoersAndThreers where
  twoers : Int
  threers : Int

def solvePart1 (boxIds : List String) : Int := Id.run do
  let mut twoersAndThreers := TwoersAndThreers.mk 0 0

  for boxId in boxIds do
    let mut counts : Std.HashMap Char Int := Std.mkHashMap
    for letter in boxId.data do
      if counts.contains letter then
        counts := counts.modify letter fun _k v => v+1
      else
        counts := counts.insert letter 1
    if not (counts.filter fun _k v => v = 2).isEmpty then
      twoersAndThreers :=
        { twoersAndThreers with twoers := twoersAndThreers.twoers + 1 }
    if not (counts.filter fun _k v => v = 3).isEmpty then
      twoersAndThreers :=
        { twoersAndThreers with threers := twoersAndThreers.threers + 1 }

  return twoersAndThreers.twoers * twoersAndThreers.threers

def intersect (s1 : String) (s2 : String) : String :=
  (List.bind (List.zip s1.data s2.data) fun (c1, c2) =>
    if c1 = c2 then [c1] else []).asString

def solvePart2 (boxIds : List String) : String := Option.get! do
  for boxId1 in boxIds do
    for boxId2 in boxIds do
      let intersection := intersect boxId1 boxId2
      if intersection.length + 1 = boxId1.length then
        return intersection
  failure

def main : IO Unit := do
  let inputHandle <- IO.FS.Handle.mk "inputs/day02.txt" IO.FS.Mode.read
  let inputStream := IO.FS.Stream.ofHandle inputHandle
  let boxIds <- inputStream.readlines
  IO.println s!"{solvePart1 boxIds}"
  IO.println s!"{solvePart2 boxIds}"
