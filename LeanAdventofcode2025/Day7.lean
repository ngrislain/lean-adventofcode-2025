namespace Day7

open System.FilePath
open IO.FS

def enum {α : Type} (l : List α) : List (Nat × α) :=
  (List.range l.length).zip l

def getAt {α} : List α -> Nat -> Option α
| [], _ => none
| a :: _, 0 => some a
| _ :: as, n+1 => getAt as n

def sortAndDedup (l : List Nat) : List Nat :=
  let sorted := l.mergeSort (· ≤ ·)
  sorted.eraseDups

def parseInput (lines : List String) : (List (List Char) × Option (Nat × Nat)) :=
  let grid := lines.map String.toList
  let sPos := enum grid |>.findSome? fun (r, row) =>
    enum row |>.findSome? fun (c, char) =>
      if char == 'S' then some (r, c) else none
  (grid, sPos)

def step (row : List Char) (activeCols : List Nat) (width : Nat) : (List Nat × Nat) :=
  let (nextCols, splits) := activeCols.foldl (fun (acc : List Nat × Nat) c =>
    let (cols, count) := acc
    if c >= row.length then acc
    else
      let char := (getAt row c).getD '.'
      if char == '^' then
        let cols1 := if c > 0 then (c - 1) :: cols else cols
        let cols2 := if c + 1 < width then (c + 1) :: cols1 else cols1
        (cols2, count + 1)
      else
        (c :: cols, count)
  ) ([], 0)
  (sortAndDedup nextCols, splits)

def consolidate (l : List (Nat × Nat)) : List (Nat × Nat) :=
  let sorted := l.mergeSort (fun a b => a.1 <= b.1)
  sorted.foldl (fun acc (c, n) =>
    match acc with
    | [] => [(c, n)]
    | (lc, ln) :: rest =>
      if lc == c then (lc, ln + n) :: rest
      else (c, n) :: acc
  ) []

def step2 (row : List Char) (activeCols : List (Nat × Nat)) (width : Nat) : List (Nat × Nat) :=
  let nextCols := activeCols.foldl (fun acc (c, count) =>
    if c >= row.length then acc
    else
      let char := (getAt row c).getD '.'
      if char == '^' then
        let acc1 := if c > 0 then (c - 1, count) :: acc else acc
        let acc2 := if c + 1 < width then (c + 1, count) :: acc1 else acc1
        acc2
      else
        (c, count) :: acc
  ) []
  consolidate nextCols

def solvePart1 (lines : List String) : Nat :=
  let (grid, sPos) := parseInput lines
  match sPos with
  | none => 0
  | some (startRow, startCol) =>
    let width := match grid.head? with | some r => r.length | none => 0
    let active := [startCol]
    let relevantRows := grid.drop startRow
    let (_, totalSplits) := relevantRows.foldl (fun (acc : List Nat × Nat) row =>
      let (currentActive, currentSplits) := acc
      let (nextActive, rowSplits) := step row currentActive width
      (nextActive, currentSplits + rowSplits)
    ) (active, 0)
    totalSplits

def solvePart2 (lines : List String) : Nat :=
  let (grid, sPos) := parseInput lines
  match sPos with
  | none => 0
  | some (startRow, startCol) =>
    let width := match grid.head? with | some r => r.length | none => 0
    let active := [(startCol, 1)]
    let relevantRows := grid.drop startRow
    let finalActive := relevantRows.foldl (fun acc row =>
      step2 row acc width
    ) active
    finalActive.foldl (fun sum (_, count) => sum + count) 0

def solve : IO Unit := do
  let lines <- IO.FS.lines "Data/Day7/input.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart1 lines
  IO.println s!"Response: {result}"

def solve2 : IO Unit := do
  let lines <- IO.FS.lines "Data/Day7/input.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart2 lines
  IO.println s!"Response: {result}"

def solveTest2 : IO Unit := do
  let lines <- IO.FS.lines "Data/Day7/test.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart2 lines
  IO.println s!"Test Response: {result}"

def response1 : IO Unit := do
  -- solveTest
  solve

def response2 : IO Unit := do
  -- solveTest2
  solve2

end Day7
