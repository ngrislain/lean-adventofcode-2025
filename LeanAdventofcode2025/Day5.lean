import Init.Data.String.Basic
import Init.Data.Char.Basic

namespace Day5

open List

structure Range : Type where
  min : Nat
  max : Nat
  deriving Repr, BEq

structure DataBase : Type where
  ranges : List Range
  entries : List Nat
  deriving Repr, BEq

def readInput : IO DataBase := do
  let content <- IO.FS.readFile "Data/Day5/input.txt"
  let lines := content.splitOn "\n"
  let rangesOrEntries: List (Range ⊕ Nat) := lines.filterMap fun line =>
    match line.splitOn "-" with
    | [minStr, maxStr] =>
      match minStr.toNat?, maxStr.toNat? with
      | some min, some max => some (Sum.inl { min := min, max := max })
      | _, _ => none
    | [entryStr] =>
      match entryStr.toNat? with
      | some entry => some (Sum.inr entry)
      | _ => none
    | _ => none
  let ranges := rangesOrEntries.filterMap fun
    | Sum.inl r => some r
    | Sum.inr _ => none
  let entries := rangesOrEntries.filterMap fun
    | Sum.inl _ => none
    | Sum.inr e => some e
  return { ranges := ranges, entries := entries }

def inRange (n : Nat) (r : Range) : Bool :=
  r.min <= n && n <= r.max

def isFresh (n : Nat) (ranges : List Range) : Bool :=
  ranges.any (inRange n)

def countFresh (database : DataBase) : Nat :=
  database.entries.filter (fun entry => isFresh entry database.ranges) |>.length

def response1 : IO Unit := do
  let database <- readInput
  let freshCount := countFresh database
  IO.println s!"Response: {freshCount}"

structure Bound : Type where
  pos : Nat
  isStart : Bool
  deriving Repr, BEq

def Bound.le (e1 e2 : Bound) : Bool :=
  if e1.pos < e2.pos then true
  else if e1.pos > e2.pos then false
  else e1.isStart && !e2.isStart

def Range.toBounds (r : Range) : List Bound :=
  [{ pos := r.min, isStart := true }, { pos := r.max + 1, isStart := false }]

def insertBound (e : Bound) : List Bound -> List Bound
  | [] => [e]
  | x :: xs => if Bound.le e x then e :: x :: xs else x :: insertBound e xs

def sortBounds (events : List Bound) : List Bound :=
  events.foldl (fun acc e => insertBound e acc) []

def countTotalFresh (ranges : List Range) : Nat :=
  let events := ranges.flatMap Range.toBounds
  let sortedEvents := sortBounds events
  let (_, _, total) := sortedEvents.foldl
    (fun (state : Nat × Nat × Nat) event =>
      let (count, startPos, total) := state
      match event.isStart with
      | true =>
        if count == 0 then
          (count + 1, event.pos, total)
        else
          (count + 1, startPos, total)
      | false =>
        let newCount := count - 1
        if newCount == 0 then
          (newCount, 0, total + (event.pos - startPos))
        else
          (newCount, startPos, total)
    ) (0, 0, 0)
  total

def response2 : IO Unit := do
  let database <- readInput
  let totalFresh := countTotalFresh database.ranges
  IO.println s!"Response: {totalFresh}"

end Day5
