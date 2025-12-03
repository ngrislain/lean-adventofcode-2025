import Init.Data.String.Basic
import Init.Data.Char.Basic

namespace Day3

open List

open Nat

def maxInterval (line : Array Nat) (minIndex : Nat) (maxIndex : Nat) : Nat :=
  let count := maxIndex + 1 - minIndex
  (List.range count).foldl (fun bestIdx i =>
    let currIdx := minIndex + i
    let currVal := line.getD currIdx 0
    let bestVal := line.getD bestIdx 0
    if currVal > bestVal then currIdx else bestIdx
  ) minIndex

def parseLine (line : String) : Array Nat :=
  line.toList.filterMap (fun c =>
    if c.isDigit then some (c.toNat - '0'.toNat) else none) |> toArray

def maxPair (line : Array Nat) : Nat :=
  let maxIdx0 := maxInterval line 0 (line.size - 2)
  let maxIdx1 := maxInterval line (maxIdx0 + 1) (line.size - 1)
  let val0 := line.getD maxIdx0 0
  let val1 := line.getD maxIdx1 0
  val0*10 + val1

def readInput : IO (List String) := do
  let content <- IO.FS.readFile "Data/Day3/input.txt"
  let lines := content.splitOn "\n"
  return lines

def response1 : IO Unit := do
  let input <- readInput
  let sum := input.map (fun line => maxPair $ parseLine line) |> sum
  IO.println s!"Response: {sum}"

def maxTwelve (line : Array Nat) : Nat :=
  let n := 12
  let (indices, _) := (List.range n).foldl (fun (acc, lastIdx) i =>
    let remaining := n - 1 - i
    let startIdx := if i == 0 then 0 else lastIdx + 1
    let endIdx := line.size - 1 - remaining
    let bestIdx := maxInterval line startIdx endIdx
    (acc ++ [bestIdx], bestIdx)
  ) ([], 0)
  let values := indices.map (line.getD · 0)
  values.foldl (fun acc val => acc * 10 + val) 0

def response2 : IO Unit := do
  let input <- readInput
  let sum := input.map (fun line => maxTwelve $ parseLine line) |> sum
  IO.println s!"Response: {sum}"

end Day3
