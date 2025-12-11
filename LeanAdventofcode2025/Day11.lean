import Lean
import Init.Data.String
import Std.Data.HashMap

namespace Day11

open IO.FS
open Lean
open Std

-- Adjacency list
abbrev Graph := HashMap String (List String)

def parseLine (line : String) : Option (String × List String) :=
  let parts := line.splitOn ":"
  match parts with
  | [src, dsts] =>
    let src := src.trim
    let dsts := dsts.trim.splitOn " " |>.filter (· != "")
    some (src, dsts)
  | _ => none

def parseGraph (lines : List String) : Graph :=
  lines.foldl (fun g line =>
    match parseLine line with
    | some (src, dsts) => g.insert src dsts
    | none => g
  ) (HashMap.emptyWithCapacity 0)

-- Memoization map
abbrev Memo := HashMap String Nat

partial def countPathsMemo (g : Graph) (current : String) (target : String) (memo : Memo) : Nat × Memo :=
  if current == target then
    (1, memo)
  else
    match memo.get? current with
    | some count => (count, memo)
    | none =>
      match g.get? current with
      | some neighbors =>
        let (count, memo) := neighbors.foldl (fun (acc, m) n =>
          let (res, m') := countPathsMemo g n target m
          (acc + res, m')
        ) (0, memo)
        (count, memo.insert current count)
      | none => (0, memo.insert current 0)

def countPaths (g : Graph) (start : String) (target : String) : Nat :=
  (countPathsMemo g start target (HashMap.emptyWithCapacity 0)).1

def solvePart1 (lines : List String) : Nat :=
  let g := parseGraph lines
  countPaths g "you" "out"

def solvePart2 (lines : List String) : Nat :=
  let g := parseGraph lines
  let start := "svr"
  let target := "out"
  let mid1 := "dac"
  let mid2 := "fft"

  -- Path 1: start -> mid1 -> mid2 -> target
  let p1_1 := countPaths g start mid1
  let p1_2 := countPaths g mid1 mid2
  let p1_3 := countPaths g mid2 target
  let path1 := p1_1 * p1_2 * p1_3

  -- Path 2: start -> mid2 -> mid1 -> target
  let p2_1 := countPaths g start mid2
  let p2_2 := countPaths g mid2 mid1
  let p2_3 := countPaths g mid1 target
  let path2 := p2_1 * p2_2 * p2_3

  path1 + path2

def response1 : IO Unit := do
  let lines <- IO.FS.lines "Data/Day11/input.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart1 lines
  IO.println s!"Part 1: {result}"

def response2 : IO Unit := do
  -- let testLines <- IO.FS.lines "Data/Day11/test2.txt"
  -- let testLines := testLines.toList.filter (· != "")
  -- let testResult := solvePart2 testLines
  -- IO.println s!"Test 2: {testResult}"

  let lines <- IO.FS.lines "Data/Day11/input.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart2 lines
  IO.println s!"Part 2: {result}"

end Day11
