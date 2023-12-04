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

def preScoreCard (card : Card) : Int := Id.run do
  let mut preScore := 0
  for own in card.ownNumbers do
    for winning in card.winningNumbers do
      if own = winning then
        preScore :=  preScore + 1
  return preScore

def solvePart1 (cards : List Card) : Int :=
  Int.sum (cards.map ((fun x => if x < 2 then x else (2 : Int) ^ (x - 1).toNat) ∘ preScoreCard))

def solvePart2 (cards : List Card) : Int := Id.run do
  let mut cardCounts : Std.HashMap Int Int := Std.mkHashMap
  for i in [:cards.length] do
    let cardId := i + 1
    cardCounts := cardCounts.insert cardId 1
  for (i, card) in cards.enum do
    let cardId := i + 1
    let preScore := preScoreCard card
    for wonCardId in [cardId+1:cardId+preScore.toNat+1] do
      if cardCounts.contains wonCardId then
        cardCounts := cardCounts.modify wonCardId fun _ count => count + cardCounts.find! cardId
  return cardCounts.fold (fun acc _ count => acc + count) 0

def main : IO Unit := do
  let inputHandle <- IO.FS.Handle.mk "inputs/aoc2023/day04.txt" IO.FS.Mode.read
  let inputStream := IO.FS.Stream.ofHandle inputHandle
  let lines <- inputStream.readlines
  let cards := lines.map parseLine
  IO.println s!"{solvePart1 cards}"
  IO.println s!"{solvePart2 cards}"
