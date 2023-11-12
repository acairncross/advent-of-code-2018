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
