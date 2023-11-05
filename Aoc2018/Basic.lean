def hello := "world"

def day01_1 := do
  let txt <- IO.FS.readFile "inputs/day01.txt"

  let stdout <- IO.getStdout

  -- let (_, res) := (List.forM (m := StateM Int) (txt.splitOn "\n") fun line => do
  --   let n := line.toInt!
  --   let acc <- get
  --   set (acc + n)) 0
  let (_, res) := (List.forM (m := StateM Int) (txt.splitOn "\n" |>.reverse |>.dropWhile String.isEmpty |>.reverse) fun line => do
    let sign := match String.front line with
    | '+' => 1
    | _ => -1

    let magnitude := (String.drop line 1).toInt!

    let n := sign * magnitude

    let acc <- get
    set (acc + n)) 0
  (<- IO.getStdout).putStrLn (toString res)
