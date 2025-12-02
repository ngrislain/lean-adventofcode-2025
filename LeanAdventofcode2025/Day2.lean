namespace Day2

open List

structure SplitNumber : Type where
  left : Int
  right : Int
  size : Int
  deriving Repr, BEq

instance : ToString SplitNumber where
  toString sn := s!"{sn.left}|{sn.right} ({sn.size})"

def parseSplitNumberAbove (splitNumberStr : String) : Option SplitNumber :=
  let splitNumberStr := "0" ++ splitNumberStr
  let len := splitNumberStr.length
  let size := len/2
  let left := splitNumberStr.take (len-size)
  let right := splitNumberStr.drop (len-size)
  match left.toInt?, right.toInt? with
  | some left, some right => some { left := left, right := right, size := size }
  | _, _ =>
    dbg_trace "Cannot parse split number above";
    none

def parseSplitNumberBelow (splitNumberStr : String) : Option SplitNumber :=
  let len := splitNumberStr.length
  let size := len/2
  let left := splitNumberStr.take (len-size)
  let right := splitNumberStr.drop (len-size)
  match left.toInt?, right.toInt? with
  | some left, some right => some { left := left, right := right, size := size }
  | _, _ =>
    dbg_trace "Cannot parse split number below";
    none

structure Range : Type where
  min : SplitNumber
  max : SplitNumber
  deriving Repr, BEq

instance : ToString Range where
  toString r := s!"{r.min}-{r.max}"

def parseRange (rangeStr : String) : Option Range :=
  let parts := rangeStr.splitOn "-"
  match parts with
  | [minStr, maxStr] =>
    match parseSplitNumberAbove minStr, parseSplitNumberBelow maxStr with
    | some min, some max => some { min := min, max := max }
    | _, _ => none
  | _ => none

def parseLine (line : String) : Option (List Range) :=
  let rangeStrs := line.splitOn ","
  dbg_trace rangeStrs
  let ranges := rangeStrs.filterMap parseRange
  return ranges

def readInput : IO (List Range) := do
  let content <- IO.FS.readFile "Data/Day2/input.txt"
  let lines := content.splitOn "\n"
  let ranges := (lines.filterMap parseLine).flatten
  return ranges

def response1 (input : List Range) : IO Unit := do
  let mut count := 0
  for r in input do {
    IO.println s!"{r}"
  }
  IO.println s!"Count: {count}"

def response2 (input : List Range) : IO Unit := do
  let mut count := 0
  -- for r in input do {
  --   if r.min.left <= r.max.left and r.min.right <= r.max.right then
  --     count := count + 1
  -- }
  IO.println s!"Count: {count}"
