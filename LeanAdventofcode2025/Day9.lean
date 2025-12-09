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

def lineBetween (p1 p2 : Point) : List Point :=
  if p1.x == p2.x then
    -- Vertical line
    let minY := min p1.y p2.y
    let maxY := max p1.y p2.y
    (List.range (maxY - minY + 1)).map (fun i => { x := p1.x, y := minY + i })
  else if p1.y == p2.y then
    -- Horizontal line
    let minX := min p1.x p2.x
    let maxX := max p1.x p2.x
    (List.range (maxX - minX + 1)).map (fun i => { x := minX + i, y := p1.y })
  else
    []

def enum {α : Type} (l : List α) : List (Nat × α) :=
  (List.range l.length).zip l

def isInsidePolygon (point : Point) (vertices : List Point) : Bool :=
  if vertices.length < 3 then false
  else
    let rec countCrossings (i : Nat) (count : Nat) : Nat :=
      if i >= vertices.length then count
      else
        let v1 := vertices[i]!
        let v2 := vertices[(i + 1) % vertices.length]!
        -- Check if ray from point going right crosses edge v1-v2
        let crosses :=
          ((v1.y > point.y) != (v2.y > point.y)) &&
          (point.x < (v2.x - v1.x) * (point.y - v1.y) / (v2.y - v1.y) + v1.x)
        countCrossings (i + 1) (if crosses then count + 1 else count)
    (countCrossings 0 0) % 2 == 1

def solvePart2 (lines : List String) : Nat :=
  let redTiles := lines.filterMap parseLine
  if redTiles.length < 2 then 0
  else
    -- Get unique sorted coordinates for compression
    let sortedXs := (redTiles.map (·.x)).toArray.qsort (· < ·) |>.toList.eraseDups.toArray
    let sortedYs := (redTiles.map (·.y)).toArray.qsort (· < ·) |>.toList.eraseDups.toArray

    -- Maps for coordinate to index
    let xMap : Std.HashMap Nat Nat := sortedXs.foldl (fun m x => m.insert x (m.size)) (Std.HashMap.emptyWithCapacity sortedXs.size)
    let yMap : Std.HashMap Nat Nat := sortedYs.foldl (fun m y => m.insert y (m.size)) (Std.HashMap.emptyWithCapacity sortedYs.size)

    -- Grid dimensions (2N - 1 to represent lines and intervals)
    let gridW := 2 * sortedXs.size - 1
    let gridH := 2 * sortedYs.size - 1
    let gridSize := gridW * gridH

    let maxArea := Id.run do
      -- flattened grid: 0 = unknown, 1 = edge, 2 = outside
      let mut grid := Array.replicate gridSize 0

      -- 1. Mark edges on the grid
      let markEdge (g : Array Nat) (p1 p2 : Point) : Array Nat :=
        let idx1 := xMap.get! p1.x
        let idy1 := yMap.get! p1.y
        let idx2 := xMap.get! p2.x
        let idy2 := yMap.get! p2.y

        let c1 := 2 * idx1
        let r1 := 2 * idy1
        let c2 := 2 * idx2
        let r2 := 2 * idy2

        if c1 == c2 then -- Vertical
          let minR := min r1 r2
          let maxR := max r1 r2
          (List.range (maxR - minR + 1)).foldl (fun arr dr =>
            arr.set! ((minR + dr) * gridW + c1) 1
          ) g
        else -- Horizontal
          let minC := min c1 c2
          let maxC := max c1 c2
          (List.range (maxC - minC + 1)).foldl (fun arr dc =>
            arr.set! (r1 * gridW + (minC + dc)) 1
          ) g

      grid := (enum redTiles).foldl (fun g (i, p1) =>
        let p2 := redTiles[(i + 1) % redTiles.length]!
        markEdge g p1 p2
      ) grid

      -- 2. Flood fill from outside to mark outside regions (2)
      let mut changed := true
      while changed do
        changed := false
        for r in [0:gridH] do
          for c in [0:gridW] do
            let idx := r * gridW + c
            if grid[idx]! == 0 then
               -- Check if connected to outside (2) or boundary
               let connected :=
                 (r == 0 || r == gridH - 1 || c == 0 || c == gridW - 1) ||
                 (r > 0 && grid[(r-1)*gridW + c]! == 2) ||
                 (r < gridH-1 && grid[(r+1)*gridW + c]! == 2) ||
                 (c > 0 && grid[r*gridW + c-1]! == 2) ||
                 (c < gridW-1 && grid[r*gridW + c+1]! == 2)

               if connected then
                 grid := grid.set! idx 2
                 changed := true

      -- DEBUG: Print Grid
      -- Id.run do
      --  IO.println s!"Grid ({gridW}x{gridH}):"
      --  for r in [0:gridH] do
      --    let mut line := ""
      --    for c in [0:gridW] do
      --      line := line ++ (toString grid[r*gridW + c]!)
      --    IO.println line
      -- This requires lifting IO into Id which is impossible.
      -- I will return the grid for debugging if needed, but for now let's check logic.

      -- 3. Prepare dimensions and histogram
      let mut colWidths := Array.replicate gridW 0
      for c in [0:gridW] do
        let w := if c % 2 == 0 then 1 else sortedXs[c/2 + 1]! - sortedXs[c/2]! - 1
        colWidths := colWidths.set! c w

      let mut rowHeights := Array.replicate gridH 0
      for r in [0:gridH] do
        let h := if r % 2 == 0 then 1 else sortedYs[r/2 + 1]! - sortedYs[r/2]! - 1
        rowHeights := rowHeights.set! r h

      -- Prefix sums of widths for O(1) width calculation in histogram
      let mut widthPrefix := Array.replicate (gridW + 1) 0
      for c in [0:gridW] do
        widthPrefix := widthPrefix.set! (c + 1) (widthPrefix[c]! + colWidths[c]!)

      let getWidth (startIdx endIdx : Nat) : Nat :=
        widthPrefix[endIdx + 1]! - widthPrefix[startIdx]!

      -- 4. Scan rows and solve Largest Rectangle in Histogram
      let mut currentHist := Array.replicate gridW 0
      let mut overallMaxArea := 0

      for r in [0:gridH] do
        -- Update histogram: accum height if valid (0 or 1), reset if outside (2)
        let rHeight := rowHeights[r]!
        for c in [0:gridW] do
          if grid[r * gridW + c]! != 2 then
            currentHist := currentHist.set! c (currentHist[c]! + rHeight)
          else
            currentHist := currentHist.set! c 0

        -- Solve Max Rect for currentHist using weights
        -- Stack stores indices
        let mut stack : List Nat := []
        for c in [0:gridW] do
          let h := currentHist[c]!
          while !stack.isEmpty && currentHist[stack.head!]! >= h do
            let top := stack.head!
            stack := stack.tail!
            let height := currentHist[top]!
            let left := if stack.isEmpty then 0 else stack.head! + 1
            let width := getWidth left (c - 1)
            let area := height * width
            if area > overallMaxArea then overallMaxArea := area
          stack := c :: stack

        -- Flush stack
        while !stack.isEmpty do
          let top := stack.head!
          stack := stack.tail!
          let height := currentHist[top]!
          let left := if stack.isEmpty then 0 else stack.head! + 1
          let width := getWidth left (gridW - 1)
          let area := height * width
          if area > overallMaxArea then overallMaxArea := area

      overallMaxArea

    maxArea

def solveTest2 : IO Unit := do
  let lines <- IO.FS.lines "Data/Day9/test.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart2 lines
  IO.println s!"Test Response Part 2: {result} (expected 24)"

def response2 : IO Unit := do
  solveTest2
  let lines <- IO.FS.lines "Data/Day9/input.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart2 lines
  IO.println s!"Response: {result}"

end Day9
