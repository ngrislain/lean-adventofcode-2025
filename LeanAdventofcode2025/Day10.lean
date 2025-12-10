namespace Day10

open IO.FS

structure Machine where
  target : List Bool
  buttons : List (List Nat)
  deriving Repr, Inhabited

def parseLights (s : String) : List Bool :=
  s.toList.map fun c => c == '#'

def parseButton (s : String) : List Nat :=
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
  some { target, buttons }

def boolListToNat (l : List Bool) : Nat :=
  let indexed := l.zip (List.range l.length)
  indexed.foldl (fun acc (b, i) => if b then acc ||| (1 <<< (l.length - 1 - i)) else acc) 0

def buttonToNat (l : Nat) (btn : List Nat) : Option Nat :=
  -- Strict: if button has invalid index, discard it.
  -- Loose: ignore invalid index.
  -- Based on 0 failures with strict, strict is fine (and cleaner).
  if btn.any (· >= l) then none
  else some (btn.foldl (fun acc idx => acc ^^^ (1 <<< (l - 1 - idx))) 0)

def solveMachine (m : Machine) : Option Nat := Id.run do
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
  let _ := if machines.length != lines.length then
     dbg_trace s!"WARNING: Parsed {machines.length} machines from {lines.length} lines!"
     ()
  else ()

  let results := machines.map solveMachine
  let failures := results.filter (Option.isNone) |>.length
  let validResults := results.filterMap id
  let total := validResults.foldl (fun acc count => acc + count) 0

  if failures > 0 then
    s!"Total: {total}, Failures: {failures}, Machines: {machines.length}"
  else
    s!"{total}"

def solveTest : IO Unit := do
  let lines <- IO.FS.lines "Data/Day10/test.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart1 lines
  IO.println s!"Test Response: {result} (expected 7)"

def response1 : IO Unit := do
  solveTest
  let lines <- IO.FS.lines "Data/Day10/input.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart1 lines
  IO.println s!"Response: {result}"

def response2 : IO Unit := do
  IO.println "Part 2 not implemented"

end Day10
