import AocLib
import Std.Data.HashMap

structure Card where
  ownNumbers : Array Int := Array.empty
  winningNumbers : Array Int := Array.empty
deriving Inhabited

instance : ToString Card where
  toString
  | { ownNumbers, winningNumbers } =>
    s!"Card \{ ownNumbers := {ownNumbers}, winningNumbers := {winningNumbers} }"

def parseNumerals (numerals : String) : Array Int :=
  let numeralList := numerals.splitOn |>.filter fun x => x.length > 0
  dbg_trace numeralList
  dbg_trace (numeralList.toArray.map fun numeral => numeral |>.strip #[' '] |>.toInt!)
  (numeralList.toArray.map fun numeral => numeral |>.strip #[' '] |>.toInt!)

def parseLine (line : String) : Card :=
  match line.splitOn ": " with
  | [_, numbers] => match numbers.splitOn " | " with
    | [ownNumerals, winningNumerals] =>
      { ownNumbers := parseNumerals ownNumerals
      , winningNumbers := parseNumerals winningNumerals
      }
    | _ => panic "failed to parse line"
  | _ => panic "failed to parse line"

def scoreCard (card : Card) : Int := Id.run do
  let mut score := 0
  for own in card.ownNumbers do
    for winning in card.winningNumbers do
      if own = winning then
        if score = 0 then
          score := 1
        else
          score := 2 * score
  return score

def main : IO Unit := do
  IO.println "hello"
  let inputHandle <- IO.FS.Handle.mk "inputs/aoc2023/day04.txt" IO.FS.Mode.read
  let inputStream := IO.FS.Stream.ofHandle inputHandle
  let lines <- inputStream.readlines
  for line in lines do
    dbg_trace (parseLine line)
    dbg_trace (scoreCard (parseLine line))
  let total := Int.sum (lines.map (scoreCard ∘ parseLine))
  IO.println s!"{total}"
  IO.println "finished"
