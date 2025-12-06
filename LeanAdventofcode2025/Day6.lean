
namespace Day6

open List

inductive Op : Type
  | Add
  | Mul
  deriving Repr, BEq

def parseOp (c : Char) : Option Op :=
  match c with
  | '+' => some Op.Add
  | '*' => some Op.Mul
  | _ => none

def splitWhitespace (s : String) : List String :=
  let rec helper (chars : List Char) (acc : String) (result : List String) : List String :=
    match chars with
    | [] => if acc.isEmpty then result.reverse else (acc :: result).reverse
    | c :: rest =>
      if c.isWhitespace then
        if acc.isEmpty then
          helper rest acc result
        else
          helper rest "" (acc :: result)
      else
        helper rest (acc.push c) result
  helper s.toList "" []

def parseGrid (content : String) : List (List Nat × Op) :=
  let lines := content.splitOn "\n" |>.filter (fun s => s != "")
  if lines.length < 2 then []
  else
    let numRows := (lines.take (lines.length - 1)).map splitWhitespace
    let opTokens := splitWhitespace (lines[lines.length - 1]!)

    let maxProblems := numRows.foldl (fun acc row => max acc row.length) 0

    List.range maxProblems |>.filterMap fun colIdx =>
      let opOpt := if colIdx < opTokens.length then
        match opTokens[colIdx]? with
        | some token =>
          if token.length == 1 then
            parseOp (token.toList.head!)
          else
            none
        | none => none
      else
        none

      match opOpt with
      | some op =>
        let numbers := numRows.filterMap fun row =>
          match row[colIdx]? with
          | some numStr => numStr.toNat?
          | none => none
        some (numbers, op)
      | none => none

def parseGridCephalopod (content : String) : List (List Nat × Op) :=
  let lines := content.splitOn "\n" |>.filter (fun s => s != "")
  if lines.length < 2 then []
  else
    let numRows := lines.take (lines.length - 1)
    let opRow := lines[lines.length - 1]!

    let maxLen := max (numRows.foldl (fun acc line => max acc line.length) 0) opRow.length
    let paddedRows := numRows.map fun row =>
      let chars := row.toList
      chars ++ List.replicate (maxLen - chars.length) ' '
    let opChars := opRow.toList ++ List.replicate (maxLen - opRow.length) ' '

    let columns := List.range maxLen |>.map fun colIdx =>
      (paddedRows.map fun row => row[colIdx]!, opChars[colIdx]!)

    let rec groupProblems (cols : List (List Char × Char)) (acc : List (List Char × Char)) (result : List (List (List Char × Char))) : List (List (List Char × Char)) :=
      match cols with
      | [] => if acc.isEmpty then result.reverse else (acc.reverse :: result).reverse
      | (chars, opChar) :: rest =>
        if chars.all (· == ' ') && opChar == ' ' then
          if acc.isEmpty then
            groupProblems rest acc result
          else
            groupProblems rest [] (acc.reverse :: result)
        else
          groupProblems rest ((chars, opChar) :: acc) result

    let problemGroups := groupProblems columns [] []

    problemGroups.filterMap fun group =>
      let opOpt := group.reverse.findSome? fun (_, opChar) =>
        parseOp opChar

      match opOpt with
      | some op =>
        let numbers := group.reverse.filterMap fun (chars, _) =>
          let numStr := String.mk (chars.filter (fun c => c != ' ' && !(parseOp c |>.isSome)))
          if numStr.isEmpty then none else numStr.toNat?
        some (numbers, op)
      | none => none

def readInput : IO (List (List Nat × Op)) := do
  let content <- IO.FS.readFile "Data/Day6/input.txt"
  return parseGrid content

def evalOp (op : Op) (args : List Nat) : Nat :=
  match op with
  | Op.Add => args.foldl (· + ·) 0
  | Op.Mul => if args.isEmpty then 0 else args.foldl (· * ·) 1

def solve : IO Nat := do
  let problems <- readInput
  let results := problems.map fun (nums, op) => evalOp op nums
  return results.foldl (· + ·) 0

def readInputCephalopod : IO (List (List Nat × Op)) := do
  let content <- IO.FS.readFile "Data/Day6/input.txt"
  return parseGridCephalopod content

def solve2 : IO Nat := do
  let problems <- readInputCephalopod
  let results := problems.map fun (nums, op) => evalOp op nums
  return results.foldl (· + ·) 0

def response1 : IO Unit := do
  let result <- solve
  IO.println s!"Response: {result}"

def response2 : IO Unit := do
  let result <- solve2
  IO.println s!"Response: {result}"

end Day6
