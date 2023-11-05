import Lake
open Lake DSL

package «aoc-2018» where
  -- add package configuration options here

lean_lib «Aoc2018» where
  -- add library configuration options here

@[default_target]
lean_exe «aoc-2018» where
  root := `Main
