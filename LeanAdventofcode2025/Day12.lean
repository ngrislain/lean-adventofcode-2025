import Lean
import Std.Data.HashMap

namespace Day12

open Lean
open Std

structure Point where
  r : Int
  c : Int
deriving Repr, BEq, Hashable, Inhabited

instance : ToString Point where
  toString p := s!"({p.r}, {p.c})"

abbrev Shape := List Point

def parseShape (lines : List String) : Shape :=
  lines.zip (List.range lines.length) |>.flatMap fun (line, r) =>
    line.toList.zip (List.range line.length) |>.filterMap fun (char, c) =>
      if char == '#' then some { r := r, c := c } else none

def normalize (s : Shape) : Shape :=
  if s.isEmpty then [] else
  let minR := s.foldl (fun m p => min m p.r) s.head!.r
  let minC := s.foldl (fun m p => min m p.c) s.head!.c
  let shifted := s.map fun p => { r := p.r - minR, c := p.c - minC }
  -- Sort lexicographically
  shifted.mergeSort (fun a b => if a.r != b.r then a.r < b.r else a.c < b.c)

def rotate90 (s : Shape) : Shape :=
  s.map fun p => { r := p.c, c := -p.r }

def flip (s : Shape) : Shape :=
  s.map fun p => { r := p.r, c := -p.c }

def generateVariations (s : Shape) : List Shape :=
  let r0 := s
  let r1 := rotate90 r0
  let r2 := rotate90 r1
  let r3 := rotate90 r2
  let f0 := flip r0
  let f1 := rotate90 f0
  let f2 := rotate90 f1
  let f3 := rotate90 f2
  let all := [r0, r1, r2, r3, f0, f1, f2, f3]
  (all.map normalize).eraseDups

structure RegionSpec where
  w : Nat
  h : Nat
  counts : Array Nat
deriving Repr, Inhabited

def parseRegions (line : String) : Option RegionSpec :=
  let parts := line.splitOn ":"
  match parts with
  | [dims, countsStr] =>
    let dimParts := dims.splitOn "x"
    match dimParts with
    | [wStr, hStr] =>
      let w := wStr.trim.toNat!
      let h := hStr.trim.toNat!
      let counts := countsStr.trim.splitOn " " |>.filter (· != "") |>.map String.toNat! |>.toArray
      some { w, h, counts }
    | _ => none
  | _ => none

def parseShapesAndRegions (lines : List String) : (Array Shape × Array RegionSpec) :=
  let grouped := lines.foldl (fun (acc : Array (Array String)) line =>
    if line.trim.isEmpty then acc.push #[]
    else if acc.isEmpty then #[ #[line] ]
    else
      let lastIdx := acc.size - 1
      let lastGroup := acc[lastIdx]!
      acc.set! lastIdx (lastGroup.push line)
  ) #[]
  let groups := grouped.filter (fun g => !g.isEmpty)

  let (shapeGroups, regionGroups) := groups.partition (fun g =>
    let first := g[0]!
    first.contains ':' && !first.contains 'x'
  )

  let shapesMap := shapeGroups.foldl (fun acc g =>
    let first := g[0]!
    let parts := first.splitOn ":"
    let id := parts.head!.trim.toNat!
    let gridLines := g.toList.tail!
    let shape := parseShape gridLines
    acc.insert id shape
  ) (HashMap.emptyWithCapacity 8)

  let maxId := shapesMap.toList.foldl (fun m (k, _) => max m k) 0
  let shapesArray := (Array.range (maxId + 1)).map (fun i => shapesMap.getD i [])

  let regions := regionGroups.flatMap (fun g =>
    g.toList.filterMap parseRegions |>.toArray
  )

  (shapesArray, regions)

structure LinearShape where
  w : Nat
  h : Nat
  area : Nat
  mask : Nat
deriving Repr, Inhabited

def toLinearShape (s : Shape) (W : Nat) : LinearShape :=
  if s.isEmpty then { w := 0, h := 0, area := 0, mask := 0 } else
  let maxR := s.foldl (fun m p => max m p.r) 0
  let maxC := s.foldl (fun m p => max m p.c) 0
  let w := (maxC + 1).toNat
  let h := (maxR + 1).toNat
  let mask := s.foldl (fun m p =>
    let idx := p.r.toNat * W + p.c.toNat
    m ||| (1 <<< idx)
  ) 0
  { w, h, area := s.length, mask }

partial def solveRecursive (w h : Nat) (grid : Nat) (varCache : Array (List LinearShape)) (presents : List Nat) (remainingArea : Nat) (minPos : Nat) (occupied : Nat) : Bool :=
  if (w * h) - occupied < remainingArea then false
  else match presents with
  | [] => true
  | pIdx :: rest =>
    let variations := varCache[pIdx]!
    let currentShapeArea := variations.head!.area
    let nextRemainingArea := remainingArea - currentShapeArea
    let maxPos := w * h - 1

    let rec tryPos (pos : Nat) : Bool :=
      if pos > maxPos then false
      else
        let r := pos / w
        let c := pos % w

        let rec tryVariations (vars : List LinearShape) : Bool :=
          match vars with
          | [] => false
          | v :: vs =>
            if c + v.w <= w && r + v.h <= h then
              let shiftedMask := v.mask <<< pos
              if (grid &&& shiftedMask) == 0 then
                let newGrid := grid ||| shiftedMask
                let nextMinPos := if !rest.isEmpty && rest.head! == pIdx then pos else 0
                if solveRecursive w h newGrid varCache rest nextRemainingArea nextMinPos (occupied + currentShapeArea) then
                  true
                else
                  tryVariations vs
              else
                tryVariations vs
            else
              tryVariations vs

        if tryVariations variations then true
        else tryPos (pos + 1)

    tryPos minPos

def solveRegion (shapes : Array Shape) (region : RegionSpec) : Bool :=
  let w := region.w
  let h := region.h

  let varCache := shapes.map fun s =>
    (generateVariations s).map fun v => toLinearShape v w

  let area (s : Shape) := s.length

  let indices := (List.range shapes.size).flatMap (fun i => List.replicate (region.counts.getD i 0) i)

  let sortedIndices := indices.mergeSort (fun i j =>
    let ai := area (shapes[i]!)
    let aj := area (shapes[j]!)
    if ai != aj then ai > aj else i < j
  )

  let totalArea := sortedIndices.foldl (fun sum i => sum + area (shapes[i]!)) 0

  solveRecursive w h 0 varCache sortedIndices totalArea 0 0

def response1 : IO Unit := do
  let file := "Data/Day12/input.txt"
  let lines <- IO.FS.lines file
  let lines := lines.toList
  let (shapes, regions) := parseShapesAndRegions lines

  let tasks := regions.map fun r =>
    Task.spawn fun _ => if solveRegion shapes r then 1 else 0

  let count := tasks.toList.map Task.get |>.sum
  IO.println s!"Part 1: {count}"

end Day12
