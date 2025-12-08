namespace Day8

open System.FilePath
open IO.FS

structure Point3D where
  x : Int
  y : Int
  z : Int
deriving Repr, BEq, Inhabited

def parseLine (line : String) : Option Point3D := do
  let parts := line.splitOn ","
  match parts with
  | [x, y, z] =>
    let x' <- x.trim.toInt?
    let y' <- y.trim.toInt?
    let z' <- z.trim.toInt?
    return { x := x', y := y', z := z' }
  | _ => none

def distanceSquared (p1 p2 : Point3D) : Int :=
  let dx := p1.x - p2.x
  let dy := p1.y - p2.y
  let dz := p1.z - p2.z
  dx * dx + dy * dy + dz * dz

structure UnionFind where
  parent : Array Nat
  rank : Array Nat
deriving Repr

def UnionFind.new (n : Nat) : UnionFind :=
  { parent := Array.range n, rank := Array.replicate n 0 }

partial def UnionFind.find (uf : UnionFind) (x : Nat) : Nat :=
  if x >= uf.parent.size then x
  else
    let px := uf.parent[x]!
    if px == x then x
    else uf.find px

def UnionFind.union (uf : UnionFind) (x y : Nat) : UnionFind :=
  if x >= uf.parent.size || y >= uf.parent.size then uf
  else
    let rx := uf.find x
    let ry := uf.find y
    if rx == ry then uf
    else
      let rankX := uf.rank[rx]!
      let rankY := uf.rank[ry]!
      if rankX < rankY then
        { uf with parent := uf.parent.set! rx ry }
      else if rankX > rankY then
        { uf with parent := uf.parent.set! ry rx }
      else
        { parent := uf.parent.set! ry rx, rank := uf.rank.set! rx (rankX + 1) }

def countCircuits (uf : UnionFind) : List (Nat × Nat) :=
  let roots := (List.range uf.parent.size).map (fun i => (uf.find i, i))
  let grouped := roots.foldl (fun acc (root, _) =>
    if acc.any (fun (r, _) => r == root) then acc
    else
      let count := roots.filter (fun (r, _) => r == root) |>.length
      (root, count) :: acc
  ) []
  grouped

def UnionFind.circuitSizes (uf : UnionFind) : List Nat :=
  (countCircuits uf).map (·.2)

def enum {α : Type} (l : List α) : List (Nat × α) :=
  (List.range l.length).zip l

def generatePairs (points : List Point3D) : List (Int × Nat × Nat) :=
  let indexed := enum points
  indexed.foldl (fun acc (i, p1) =>
    indexed.foldl (fun acc2 (j, p2) =>
      if i < j then
        let dist := distanceSquared p1 p2
        (dist, i, j) :: acc2
      else acc2
    ) acc
  ) []

def solvePart1 (lines : List String) (numConnections : Nat) : Nat :=
  let points := lines.filterMap parseLine
  let n := points.length
  let pairs := generatePairs points
  let sortedPairs := pairs.mergeSort (fun (d1, _, _) (d2, _, _) => d1 <= d2)
  let uf := sortedPairs.take numConnections |>.foldl (fun uf (_, i, j) =>
    uf.union i j
  ) (UnionFind.new n)
  let sizes := uf.circuitSizes
  let topSizes := sizes.mergeSort (fun a b => a >= b) |>.take 3
  topSizes.foldl (· * ·) 1

def solve : IO Unit := do
  let lines <- IO.FS.lines "Data/Day8/input.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart1 lines 1000
  IO.println s!"Response: {result}"

def solveTest : IO Unit := do
  let lines <- IO.FS.lines "Data/Day8/test.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart1 lines 10
  IO.println s!"Test Response: {result}"

def response1 : IO Unit := do
  -- solveTest
  solve

def UnionFind.numCircuits (uf : UnionFind) : Nat :=
  let roots := (List.range uf.parent.size).map uf.find
  roots.foldl (fun acc root =>
    if acc.contains root then acc else root :: acc
  ) [] |>.length

def solvePart2 (lines : List String) : Int :=
  let points := lines.filterMap parseLine
  let pointsArray := points.toArray
  let n := points.length
  let pairs := generatePairs points
  let sortedPairs := pairs.mergeSort (fun (d1, _, _) (d2, _, _) => d1 <= d2)

  let rec connectUntilOne (uf : UnionFind) (remaining : List (Int × Nat × Nat)) : Int :=
    match remaining with
    | [] => 0
    | (_, i, j) :: rest =>
      let uf' := uf.union i j
      if uf'.numCircuits == 1 then
        let p1 := pointsArray[i]!
        let p2 := pointsArray[j]!
        p1.x * p2.x
      else
        connectUntilOne uf' rest

  connectUntilOne (UnionFind.new n) sortedPairs

def solveTest2 : IO Unit := do
  let lines <- IO.FS.lines "Data/Day8/test.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart2 lines
  IO.println s!"Test Response Part 2: {result} (expected 25272 = 216 * 117)"

def response2 : IO Unit := do
  -- solveTest2
  let lines <- IO.FS.lines "Data/Day8/input.txt"
  let lines := lines.toList.filter (· != "")
  let result := solvePart2 lines
  IO.println s!"Response: {result}"

end Day8
