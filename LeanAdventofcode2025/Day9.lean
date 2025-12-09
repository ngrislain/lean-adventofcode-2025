import Std.Data.HashSet

namespace Day9

open System.FilePath
open IO.FS

structure Point where
  x : Nat
  y : Nat
deriving Repr, BEq, Inhabited, Hashable

def parseLine (line : String) : Option Point := do
  let parts := line.splitOn ","
  match parts with
  | [x, y] =>
    let x' <- x.trim.toNat?
    let y' <- y.trim.toNat?
    return { x := x', y := y' }
  | _ => none

def rectangleArea (p1 p2 : Point) : Nat :=
  let width := if p1.x > p2.x then p1.x - p2.x + 1 else p2.x - p1.x + 1
  let height := if p1.y > p2.y then p1.y - p2.y + 1 else p2.y - p1.y + 1
  width * height

def maxRectangle (points : List Point) : Nat :=
  let pairs := points.foldl (fun acc p1 =>
    points.foldl (fun acc2 p2 =>
      if p1 != p2 then
        let area := rectangleArea p1 p2
        if area > acc2 then area else acc2
      else acc2
    ) acc
  ) 0
  pairs

def solvePart1 (lines : List String) : Nat :=
  let points := lines.filterMap parseLine
  maxRectangle points

def isInside (poly : Array Point) (minX maxX minY maxY : Nat) : Bool :=
  let n := poly.size
  let edgesIntersect := (List.range n).any (fun i =>
    let p1 := poly.getD i default
    let p2 := poly.getD ((i + 1) % n) default
    if p1.x == p2.x then
      let ex := p1.x
      let eyMin := min p1.y p2.y
      let eyMax := max p1.y p2.y
      if minX < ex && ex < maxX then
        let overlapMin := max minY eyMin
        let overlapMax := min maxY eyMax
        decide (overlapMin < overlapMax)
      else false
    else
      let ey := p1.y
      let exMin := min p1.x p2.x
      let exMax := max p1.x p2.x
      if minY < ey && ey < maxY then
        let overlapMin := max minX exMin
        let overlapMax := min maxX exMax
        decide (overlapMin < overlapMax)
      else false
  )

  if edgesIntersect then false
  else
    let intersections := (List.range n).foldl (fun count i =>
      let p1 := poly.getD i default
      let p2 := poly.getD ((i + 1) % n) default
      if p1.x == p2.x then
        let ex := p1.x
        let eyMin := min p1.y p2.y
        let eyMax := max p1.y p2.y
        if ex <= minX && eyMin <= minY && minY < eyMax then
          count + 1
        else count
      else count
    ) 0
    intersections % 2 == 1

def maxValidRectangle (points : Array Point) : Nat :=
  Id.run do
    let n := points.size
    let mut maxArea := 0
    for i in [0:n] do
      for j in [0:n] do
        if i != j then
          let p1 := points.getD i default
          let p2 := points.getD j default
          let currentArea := rectangleArea p1 p2
          if currentArea > maxArea then
            let minX := min p1.x p2.x
            let maxX := max p1.x p2.x
            let minY := min p1.y p2.y
            let maxY := max p1.y p2.y
            if isInside points minX maxX minY maxY then
              maxArea := currentArea
    return maxArea

def solvePart2 (lines : List String) : Nat :=
  let points := lines.filterMap parseLine |> List.toArray
  maxValidRectangle points

def solveTest : IO Unit := do
  let lines <- IO.FS.lines "Data/Day9/test.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart1 lines
  IO.println s!"Test Response: {result} (expected 50)"

def response1 : IO Unit := do
  solveTest
  let lines <- IO.FS.lines "Data/Day9/input.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart1 lines
  IO.println s!"Response: {result}"

def solveTest2 : IO Unit := do
  let lines <- IO.FS.lines "Data/Day9/test.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart2 lines
  IO.println s!"Test 2 Response: {result} (expected 24)"

def response2 : IO Unit := do
  solveTest2
  let lines <- IO.FS.lines "Data/Day9/input.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart2 lines
  IO.println s!"Response 2: {result}"

end Day9
