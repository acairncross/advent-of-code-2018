import Std.Data.Array.Init.Lemmas
import Mathlib.Data.Vector

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

namespace Array

theorem size_append (xs : Array α) (ys : Array α) :
  (xs ++ ys).size = xs.size + ys.size := by
    unfold size
    rw [Array.append_data]
    rw [List.length_append]

def toChunks (n : Nat) (xs : Array α) : Array (Subarray α) := Id.run do
  let mut chunks: Array (Subarray α) := Array.empty
  for i in [:xs.size:n] do
    chunks := chunks.push (xs.toSubarray (start := i) (stop := i+n))
  return chunks

end Array

namespace Nat

-- There's a Nat.sum in the std so mirror that
def product (l : List Nat) : Nat := l.foldr (·*·) 1

end Nat

namespace RankNArray

structure RankNArray (α : Type) (n : Nat) where
  data : Array α
  shape : Vector Nat n
  property : data.size = Nat.product shape.val

end RankNArray
