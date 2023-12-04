import Lake
open Lake DSL

require std from git
  "https://github.com/leanprover/std4/" @ "v4.2.0"

require mathlib from git
  "https://github.com/leanprover-community/mathlib4" @ "v4.2.0"

package «advent-of-lean» where
  -- add package configuration options here

lean_lib AocLib where
  -- add library configuration options here

lean_exe aoc16day16 where
  root := `Aoc2016.Day16

lean_exe aoc18day01 where
  root := `Aoc2018.Day01

lean_exe aoc18day02 where
  root := `Aoc2018.Day02

lean_exe aoc23day01 where
  root := `Aoc2023.Day01

lean_exe aoc23day02 where
  root := `Aoc2023.Day02

lean_exe aoc23day03 where
  root := `Aoc2023.Day03

lean_exe aoc23day04 where
  root := `Aoc2023.Day04
