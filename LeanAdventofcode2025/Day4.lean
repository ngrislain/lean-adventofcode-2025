import Init.Data.String.Basic
import Init.Data.Char.Basic

namespace Day4

open List

def parseGrid (lines : List String) : Array Bool × Nat × Nat :=
  let lines := lines.filter (fun l => !l.isEmpty)
  let rows := lines.length
  let cols := lines.head?.map (fun l => l.length) |>.getD 0
  let bools := lines.foldl (fun acc line =>
    acc ++ line.toList.map (fun c => c == '@')
  ) []
  (bools.toArray, rows, cols)

def readInput : IO (Array Bool × Nat × Nat) := do
  let content <- IO.FS.readFile "Data/Day4/input.txt"
  let lines := content.splitOn "\n"
  return parseGrid lines

def response1 : IO Unit := do
  let (grid, rows, cols) <- readInput
  IO.println s!"Grid size: {rows}x{cols}"
  IO.println s!"Response: {grid}"

def response2 : IO Unit := do
  let (_grid, _rows, _cols) <- readInput
  IO.println s!"Response: {0}"

end Day4
