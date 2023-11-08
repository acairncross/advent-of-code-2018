import Lake
open Lake DSL

require std from git
  "https://github.com/leanprover/std4/" @ "v4.2.0"

package «aoc-2018» where
  -- add package configuration options here

lean_lib Aoc2018 where
  -- add library configuration options here

lean_exe day01 where
  root := `Day01
