import Std.Data.Array.Init.Lemmas
import Mathlib.Data.Vector
import Lean.Data.Parsec

namespace IO.FS.Stream

partial def readlines (stream : IO.FS.Stream) : IO (List String) := do
  let line := (<- stream.getLine).dropRight 1 -- drop the newline
  if line = ""
    then
      return []
    else
      let lines <- stream.readlines
      return List.cons line lines

end IO.FS.Stream

-- There isn't a Hashable instance for Char in the stdlib currently
instance : Hashable Char where
  hash c := c.val.toUInt64

namespace Char

def toInt! (c : Char) : Int :=
  if c.isDigit then
    Int.ofNat (c.toNat - '0'.toNat)
  else
    panic "Expected digit"

end Char

namespace Int

def sum (l : List Int) : Int := l.foldr (·+·) 0

def ofDigits (digits : Array Int) : Int :=
  Array.foldl (fun acc x => 10 * acc + x) 0 digits

end Int

namespace Lean.Parsec

def sepBy1 (p : Parsec α) (sep : Parsec sep) : Parsec (Array α) := do
  let mut acc : Array α := Array.empty
  acc := acc.push (<- p)
  acc <- manyCore (sep *> p) acc
  return acc

end Lean.Parsec

namespace Array

theorem size_append (xs : Array α) (ys : Array α) :
  (xs ++ ys).size = xs.size + ys.size := by
    unfold size
    rw [Array.append_data]
    rw [List.length_append]

def toChunks (n : Nat) (xs : Array α) : Array (Subarray α) := Id.run do
  let mut chunks: Array (Subarray α) := Array.empty
  for i in [:xs.size:n] do
    if i+n <= xs.size then
      chunks := chunks.push (xs.toSubarray (start := i) (stop := i+n))
  return chunks

end Array

namespace Nat

-- There's a Nat.sum in the std so mirror that
def product (l : List Nat) : Nat := l.foldr (·*·) 1

end Nat

namespace Vector

-- def foldr

end Vector

namespace RankNArray

structure RankNArray (α : Type) (n : Nat) where
  data : Array α
  shape : Vector Nat n
  property : data.size = Nat.product shape.val

def inBounds (xs : RankNArray α n) (idx : Vector Nat n) : Prop :=
  (xs.shape.toList.zip idx.toList).all fun (x, i) => i < x

instance : GetElem (RankNArray α n) (Vector Nat n) α inBounds where
  getElem (xs : RankNArray α n) (idx : Vector Nat n) ok := sorry

end RankNArray
