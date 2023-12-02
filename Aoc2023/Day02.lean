import AocLib
import Lean.Data.Parsec

structure Rgb where
  red : Int := 0
  green : Int := 0
  blue : Int := 0
deriving Repr

instance : ToString Rgb where
  toString
  | { red, green, blue } => s!"Rgb \{ red := {red}, green := {green}, blue := {blue} }"

instance : Add Rgb where
  add x y := { red := x.red + y.red, green := x.green + y.green, blue := x.blue + y.blue }

instance : Preorder Rgb where
  le x y := x.red <= y.red && x.green <= y.green && x.blue <= y.blue
  le_refl := by simp
  le_trans := sorry

def le (x y : Rgb) : Bool :=
  x.red <= y.red && x.green <= y.green && x.blue <= y.blue

structure Game where
  id : Int
  draws : Array Rgb
deriving Inhabited

namespace Lean.Parsec

def singleColorDraw : Parsec Rgb := do
  let count <- (Int.ofDigits ∘ Array.map Char.toInt!) <$> Parsec.many Parsec.digit
  Lean.Parsec.skipChar ' '
  (   (Lean.Parsec.skipString "red" *> return { red := count })
  <|> (Lean.Parsec.skipString "green" *> return { green := count })
  <|> (Lean.Parsec.skipString "blue" *> return { blue := count })
  )

def draw : Parsec Rgb :=
  Array.foldl (·+·) {} <$> sepBy1 singleColorDraw (Lean.Parsec.skipString ", ")

def game : Parsec Game := do
  Parsec.skipString "Game "
  let gameId <- (Int.ofDigits ∘ Array.map Char.toInt!) <$> Parsec.many Parsec.digit
  Parsec.skipString ": "
  let draws <- sepBy1 draw (Lean.Parsec.skipString "; ")
  return { id := gameId, draws := draws }

end Lean.Parsec

def parseLine (line : String) : Game :=
  match Lean.Parsec.run Lean.Parsec.game line with
  | .error e => panic e
  | .ok game => game

def target : Rgb := { red := 12, green := 13, blue := 14 }

def possibleGame (game : Game) : Bool := Id.run do
  for draw in game.draws do
    -- how do you use the le defined in the Preorder instance? it returns Prop not Bool
    if draw.red > target.red || draw.green > target.green || draw.blue > target.blue then
      return false
  return true

def solvePart1 (games : List Game) : Int := Id.run do
  let mut total := 0
  for game in games do
    if possibleGame game then do
      total := total + game.id
  return total

def minimalBag (game : Game) : Rgb := Id.run do
  let mut minimum: Rgb := {}
  for draw in game.draws do
    minimum := { minimum with
      red := max minimum.red draw.red,
      green := max minimum.green draw.green,
      blue := max minimum.blue draw.blue
    }
  return minimum

def power (cubes : Rgb) : Int := cubes.red * cubes.green * cubes.blue

def solvePart2 (games : List Game) : Int := Id.run do
  let mut total := 0
  for game in games do
    total := total + power (minimalBag game)
  return total

def main : IO Unit := do
  let inputHandle <- IO.FS.Handle.mk "inputs/aoc2023/day02.txt" IO.FS.Mode.read
  let inputStream := IO.FS.Stream.ofHandle inputHandle
  let lines <- inputStream.readlines
  let games := lines.map parseLine
  -- for game in games do
  --   IO.println s!"{game.draws}"
  IO.println s!"{solvePart1 games}"
  IO.println s!"{solvePart2 games}"
