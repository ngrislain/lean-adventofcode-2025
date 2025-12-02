namespace Day2

inductive Direction : Type
  | Left
  | Right
  deriving Repr, BEq

def Direction.toChar : Direction -> Char
  | Left => 'L'
  | Right => 'R'

instance : ToString Direction where
  toString d := d.toChar.toString

def toDirection (c : Char) : Option Direction :=
  match c with
  | 'L' => some Direction.Left
  | 'R' => some Direction.Right
  | _ => none

def parseLine (line : String) : Option (Direction × Int) :=
  if line.isEmpty then none
  else
    let dirOpt := toDirection line.front
    let distStr := line.drop 1
    match dirOpt, distStr.toInt? with
    | some dir, some dist => some (dir, dist)
    | _, _ => none

def readInput : IO (List (Direction × Int)) := do
  let content <- IO.FS.readFile "Data/Day1/input.txt"
  let lines := content.splitOn "\n"
  let parsed := lines.filterMap parseLine
  return parsed

def readTestInput : IO (List (Direction × Int)) := do
  let content <- IO.FS.readFile "Data/Day1/test.txt"
  let lines := content.splitOn "\n"
  let parsed := lines.filterMap parseLine
  return parsed

structure State : Type where
  position : Int
  zero_count : Int
  deriving Repr, BEq

instance : ToString State where
  toString s := s!"(position: {s.position}, zero_count: {s.zero_count})"

def initialState : State :=
  { position := 50, zero_count := 0 }

def updateState1 (state : State) (dir : Direction) (dist : Int) : State :=
  let newPosition := match dir with
    | Direction.Left => (state.position - dist) % 100
    | Direction.Right => (state.position + dist) % 100
  { state with position := newPosition, zero_count := if newPosition = 0 then state.zero_count + 1 else state.zero_count }

def response1 (input : List (Direction × Int)) : IO Unit := do

  let mut state := initialState
  for (dir, dist) in input do {
    state := updateState1 state dir dist;
  }
  IO.println s!"Zero count: {state.zero_count}"

def updateState2 (state : State) (dir : Direction) (dist : Int) : State :=
  match dir with
  | Direction.Left =>
    let new_position := (state.position - dist) % 100
    let new_zero_count := (((-state.position) % 100) + dist) / 100
    { state with position := new_position, zero_count := state.zero_count + new_zero_count }
  | Direction.Right =>
    let new_position := (state.position + dist) % 100
    let new_zero_count := (state.position + dist) / 100
    { state with position := new_position, zero_count := state.zero_count + new_zero_count }

def response2 (input : List (Direction × Int)) : IO Unit := do
  let mut state := initialState
  for (dir, dist) in input do {
    state := updateState2 state dir dist;
  }
  IO.println s!"Zero count: {state.zero_count}"

end Day2
