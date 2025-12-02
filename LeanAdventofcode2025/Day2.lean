import Std.Data.HashSet

namespace Day2

open List

open Nat

structure SplitNumber : Type where
  half : Nat
  size : Nat
  deriving Repr, BEq

instance : ToString SplitNumber where
  toString sn := s!"{sn.half}{sn.half} ({sn.size})"

def parseSplitNumberAbove (splitNumberStr : String) : Option SplitNumber :=
  let splitNumberStr := "0" ++ splitNumberStr
  let len := splitNumberStr.length
  let size := len/2
  let left := splitNumberStr.take (len-size)
  let right := splitNumberStr.drop (len-size)
  let minHalf := 10^(size-1)
  match left.toNat?, right.toNat? with
  | some left, some right =>
    if left < minHalf then
      some { half := minHalf, size := size }
    else if left < right then
      some { half := left+1, size := size }
    else
      some { half := left, size := size }
  | _, _ =>
    dbg_trace "Cannot parse split number above";
    none

def parseSplitNumberBelow (splitNumberStr : String) : Option SplitNumber :=
  let len := splitNumberStr.length
  let size := len/2
  let left := splitNumberStr.take (len-size)
  let right := splitNumberStr.drop (len-size)
  let maxHalf := 10^size - 1
  match left.toNat?, right.toNat? with
  | some left, some right =>
    if left > maxHalf then
      some { half := maxHalf, size := size }
    else if left > right then
      some { half := left-1, size := size }
    else
      some { half := left, size := size }
  | _, _ =>
    dbg_trace "Cannot parse split number below";
    none

structure Range : Type where
  splitMin : SplitNumber
  splitMax : SplitNumber
  deriving Repr, BEq

instance : ToString Range where
  toString r := s!"{r.splitMin}-{r.splitMax}"

def numInRange (range : Range) : Int :=
  if range.splitMax.half < range.splitMin.half then
    0
  else
    (range.splitMax.half - range.splitMin.half + 1)

def sumInRange (range : Range) : Int :=
  let minVal := range.splitMin.half
  let maxVal := range.splitMax.half
  if maxVal < minVal then 0 else
  let count := maxVal + 1 - minVal
  let halves := (List.range count).map (fun i =>
    let valStr := toString (minVal + i : Nat);
    (valStr ++ valStr).toNat!
  )
  (halves.foldl (fun acc x => acc + x) 0 : Nat)

def parseRange1 (rangeStr : String) : Option Range :=
  -- dbg_trace s!"rangeStr {rangeStr}"
  let parts := rangeStr.splitOn "-"
  match parts with
  | [minStr, maxStr] =>
    match parseSplitNumberAbove minStr, parseSplitNumberBelow maxStr with
    | some min, some max =>
      -- dbg_trace s!"min {min} max {max} {numInRange { splitMin := min, splitMax := max }}";
      some { splitMin := min, splitMax := max }
    | _, _ => none
  | _ => none

def parseLine1 (line : String) : Option (List Range) :=
  let rangeStrs := line.splitOn ","
  let ranges := rangeStrs.filterMap parseRange1
  return ranges

def readInput1 : IO (List Range) := do
  let content <- IO.FS.readFile "Data/Day2/input.txt"
  let lines := content.splitOn "\n"
  let ranges := (lines.filterMap parseLine1).flatten
  return ranges

def response1 : IO Unit := do
  let input <- readInput1
  let count := input.foldl (fun acc r => acc + sumInRange r) 0
  IO.println s!"Count: {count}"

-- Part 2

structure Bounds where
  min : Nat
  max : Nat
  deriving Repr, BEq, Inhabited

def parseBounds (s : String) : Option Bounds :=
  match s.splitOn "-" with
  | [minS, maxS] =>
    match minS.toNat?, maxS.toNat? with
    | some min, some max => some { min := min, max := max }
    | _, _ => none
  | _ => none

def parseLine2 (line : String) : List Bounds :=
  (line.splitOn ",").filterMap parseBounds

def readInput2 : IO (List Bounds) := do
  let content <- IO.FS.readFile "Data/Day2/input.txt"
  let lines := content.splitOn "\n"
  return (lines.map parseLine2).flatten

-- Generate all repeated numbers up to maxVal
def generateRepeatedNumbers (maxVal : Nat) : IO (Std.HashSet Nat) := do
  let maxDigits := (toString maxVal).length
  let mut result : Std.HashSet Nat := Std.HashSet.emptyWithCapacity 1000
  for lenS in List.range maxDigits do
    let lenS := lenS + 1 -- range starts at 0
    let maxK := maxDigits / lenS
    if maxK >= 2 then
      let start := if lenS == 1 then 1 else 10^(lenS - 1)
      let endVal := 10^lenS
      for s in List.range (endVal - start) do
        let s := start + s
        let sStr := toString s
        for k in List.range (maxK - 1) do
          let k := k + 2 -- start at k=2
          let repeated := String.intercalate "" (List.replicate k sStr)
          match repeated.toNat? with
          | some x =>
            if x <= maxVal then
              result := result.insert x
          | none => pure ()
  return result

def response2 : IO Unit := do
  let input <- readInput2
  let maxEnd := input.foldl (fun acc b => max acc b.max) 0
  let repeatedNumbers <- generateRepeatedNumbers maxEnd
  let mut total := 0
  for x in repeatedNumbers.toList do
    let mut found := false
    for b in input do
      if b.min <= x && x <= b.max then
        total := total + x
        found := true
        break
    if found then continue
  IO.println s!"Count: {total}"
