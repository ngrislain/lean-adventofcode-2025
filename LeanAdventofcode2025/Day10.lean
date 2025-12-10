import Lean
import Init.Data.Int.Basic

namespace Day10

open IO.FS

structure Machine where
  target : List Bool
  buttons : List (List Nat)
  joltages : List Nat
  deriving Repr, Inhabited

def parseLights (s : String) : List Bool :=
  s.toList.map fun c => c == '#'

def parseButton (s : String) : List Nat :=
  let content := s.drop 1 |>.dropRight 1
  if content.trim.isEmpty then []
  else content.splitOn "," |>.map String.trim |>.filterMap String.toNat?

def parseJoltages (s : String) : List Nat :=
  let content := s.drop 1 |>.dropRight 1
  if content.trim.isEmpty then []
  else content.splitOn "," |>.map String.trim |>.filterMap String.toNat?

def parseLine (line : String) : Option Machine := do
  let parts := line.splitOn " " |>.filter (· != "")
  if parts.isEmpty then failure
  let targetStr := parts.head!
  if !targetStr.startsWith "[" || !targetStr.endsWith "]" then failure
  let target := parseLights (targetStr.drop 1 |>.dropRight 1)

  let buttonParts := parts.tail!.filter (fun s => s.startsWith "(" && s.endsWith ")")
  let buttons := buttonParts.map parseButton

  let joltageParts := parts.tail!.filter (fun s => s.startsWith "{" && s.endsWith "}")
  let joltages := match joltageParts.head? with
    | some s => parseJoltages s
    | none => []

  some { target, buttons, joltages }

-- Part 1 Logic --

def boolListToNat (l : List Bool) : Nat :=
  let indexed := l.zip (List.range l.length)
  indexed.foldl (fun acc (b, i) => if b then acc ||| (1 <<< (l.length - 1 - i)) else acc) 0

def buttonToNat (l : Nat) (btn : List Nat) : Option Nat :=
  if btn.any (· >= l) then none
  else some (btn.foldl (fun acc idx => acc ^^^ (1 <<< (l - 1 - idx))) 0)

def solveMachinePart1 (m : Machine) : Option Nat := Id.run do
  let l := m.target.length
  let validButtons := m.buttons.filterMap (buttonToNat l) |>.toArray
  let n := validButtons.size
  let targetMask := boolListToNat m.target
  let limit := 1 <<< n
  let mut minPresses : Option Nat := none

  for mask in [0:limit] do
    let mut current := 0
    let mut pressCount := 0
    for i in [0:n] do
      if (mask >>> i) &&& 1 == 1 then
        pressCount := pressCount + 1
        current := current ^^^ validButtons[i]!

    if current == targetMask then
      match minPresses with
      | none => minPresses := some pressCount
      | some min => if pressCount < min then minPresses := some pressCount

  minPresses

def solvePart1 (lines : List String) : String :=
  let machines := lines.filterMap parseLine
  let results := machines.map solveMachinePart1
  let failures := results.filter (Option.isNone) |>.length
  let validResults := results.filterMap id
  let total := validResults.foldl (fun acc count => acc + count) 0
  if failures > 0 then
    s!"Total: {total}, Failures: {failures}"
  else
    s!"{total}"

-- Part 2 Logic --

def solveMachinePart2 (m : Machine) : Option Nat := Id.run do
  let l := m.target.length
  let n := m.buttons.length
  let buttons := m.buttons.toArray
  let joltages := m.joltages.toArray

  let zeroRat : Rat := Rat.ofInt 0
  let oneRat : Rat := Rat.ofInt 1

  let mut mat : Array (Array Rat) := (List.range l).toArray.map (fun _ => Array.replicate n zeroRat)

  for j in [0:n] do
    let btn := buttons[j]!
    for idx in btn do
      if idx < l then
        mat := mat.set! idx (mat[idx]!.set! j oneRat)

  for i in [0:l] do
    let jolt := if i < joltages.size then joltages[i]! else 0
    let joltRat : Rat := Rat.ofInt (Int.ofNat jolt)
    mat := mat.set! i (mat[i]!.push joltRat)

  let numRows := l
  let numCols := n -- vars
  let mut pivotRow := 0
  let mut pivots : List (Nat × Nat) := [] -- (col, row)

  for j in [0:numCols] do
    if pivotRow < numRows then
      let mut pivotIdx := -1
      for i in [pivotRow:numRows] do
        if mat[i]![j]! != 0 then
          pivotIdx := i
          break

      if pivotIdx != -1 then
        let rowP := mat[pivotIdx]!
        mat := mat.set! pivotIdx mat[pivotRow]!
        mat := mat.set! pivotRow rowP

        let pivotVal : Rat := mat[pivotRow]![j]!
        for k in [j:numCols+1] do
           let val : Rat := mat[pivotRow]![k]!
           mat := mat.set! pivotRow (mat[pivotRow]!.set! k (val / pivotVal))

        for i in [0:numRows] do
          if i != pivotRow then
            let factor : Rat := mat[i]![j]!
            if factor != 0 then
              for k in [j:numCols+1] do
                let rVal : Rat := mat[pivotRow]![k]!
                let curVal : Rat := mat[i]![k]!
                let val : Rat := curVal - factor * rVal
                mat := mat.set! i (mat[i]!.set! k val)

        pivots := (j, pivotRow) :: pivots
        pivotRow := pivotRow + 1

  for i in [pivotRow:numRows] do
    if mat[i]![numCols]! != 0 then
      return none

  let pivotCols := pivots.map (·.1)
  let freeCols := (List.range n).filter (fun c => !pivotCols.contains c)

  let rec search (freeVars : List Nat) (assignments : List (Nat × Nat)) : Id (Option Nat) := do
    match freeVars with
    | [] =>
      let currentSum := assignments.foldl (fun acc (_, val) => acc + val) 0

      let pivotSumOpt := pivots.foldl (fun (acc : Option Nat) (p : Nat × Nat) =>
        match acc with
        | none => none
        | some s =>
          let (pCol, pRow) := p
          let val : Rat := mat[pRow]![numCols]!
          let val' := assignments.foldl (fun v (fCol, fVal) =>
             v - mat[pRow]![fCol]! * (Rat.ofInt (Int.ofNat fVal))
          ) val

          if val'.den == 1 && val'.num >= 0 then
             some (s + val'.num.toNat)
          else
             none
      ) (some 0)

      match pivotSumOpt with
      | some pSum => some (currentSum + pSum)
      | none => none

    | f :: fs =>
      let mut best : Option Nat := none
      for val in [0:101] do
        let res <- search fs ((f, val) :: assignments)
        match res, best with
        | some r, none => best := some r
        | some r, some b => if r < b then best := some r
        | _, _ => ()
      best

  search freeCols []

def solvePart2 (lines : List String) : String :=
  let machines := lines.filterMap parseLine
  let results := machines.map solveMachinePart2
  let failures := results.filter (Option.isNone) |>.length
  let validResults := results.filterMap id
  let total := validResults.foldl (fun acc count => acc + count) 0
  if failures > 0 then
    s!"Total: {total}, Failures: {failures}"
  else
    s!"{total}"

def solveTest : IO Unit := do
  let lines <- IO.FS.lines "Data/Day10/test.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart1 lines
  IO.println s!"Part 1 Test: {result}"

def response1 : IO Unit := do
  let lines <- IO.FS.lines "Data/Day10/input.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart1 lines
  IO.println s!"Part 1: {result}"

def response2 : IO Unit := do
  let lines <- IO.FS.lines "Data/Day10/input.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart2 lines
  IO.println s!"Part 2: {result}"

end Day10
