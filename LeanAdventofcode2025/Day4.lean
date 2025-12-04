import Init.Data.String.Basic
import Init.Data.Char.Basic

namespace Day4

open List

structure BoolGrid : Type where
  rows : Nat
  cols : Nat
  values : Array Bool
  deriving Repr, BEq

def BoolGrid.get (g : BoolGrid) (row : Int) (col : Int) : Bool :=
  if row >= 0 && row < g.rows && col >= 0 && col < g.cols then
    (g.values.getD ((row * g.cols + col).toNat) false)
  else
    false

instance : ToString BoolGrid where
  toString g := s!"BoolGrid {g.rows}x{g.cols}"

def parseGrid (lines : List String) : BoolGrid :=
  let lines := lines.filter (fun l => !l.isEmpty)
  let rows := lines.length
  let cols := lines.head?.map (fun l => l.length) |>.getD 0
  let bools := lines.foldl (fun acc line =>
    acc ++ line.toList.map (fun c => c == '@')
  ) []
  { rows := rows, cols := cols, values := bools.toArray }

def countNeighbors (g : BoolGrid) (row : Int) (col : Int) : Nat :=
  let directions := [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
  directions.filter (fun (dr, dc) => g.get (row + dr) (col + dc)) |>.length

def countSlots (g : BoolGrid) : Nat :=
  let slotCounts := (List.range g.rows).flatMap fun row =>
    (List.range g.cols).map fun col =>
    if g.get (Int.ofNat row) (Int.ofNat col) && countNeighbors g (Int.ofNat row) (Int.ofNat col) < 4 then 1 else 0
  slotCounts.sum

def BoolGrid.set (g : BoolGrid) (row : Int) (col : Int) (value : Bool) : BoolGrid :=
  if row >= 0 && row < g.rows && col >= 0 && col < g.cols then
    let idx := (row * g.cols + col).toNat
    { g with values := g.values.set! idx value }
  else
    g

def removeAccessible (g : BoolGrid) : BoolGrid × Nat :=
  let toRemove := (List.range g.rows).flatMap fun row =>
    (List.range g.cols).filterMap fun col =>
      let r := Int.ofNat row
      let c := Int.ofNat col
      if g.get r c && countNeighbors g r c < 4 then some (r, c) else none
  let newGrid := toRemove.foldl (fun grid (r, c) => grid.set r c false) g
  (newGrid, toRemove.length)

def removeAllAccessible (g : BoolGrid) (maxIters : Nat := 1000) : Nat :=
  let rec loop (grid : BoolGrid) (total : Nat) (fuel : Nat) : Nat :=
    match fuel with
    | 0 => total
    | fuel' + 1 =>
      let (newGrid, removed) := removeAccessible grid
      if removed == 0 then
        total
      else
        loop newGrid (total + removed) fuel'
  loop g 0 maxIters

def readInput : IO BoolGrid := do
  let content <- IO.FS.readFile "Data/Day4/input.txt"
  let lines := content.splitOn "\n"
  return parseGrid lines

def response1 : IO Unit := do
  let grid <- readInput
  IO.println s!"Response: {countSlots grid}"

def response2 : IO Unit := do
  let grid <- readInput
  IO.println s!"Response: {removeAllAccessible grid}"

end Day4
