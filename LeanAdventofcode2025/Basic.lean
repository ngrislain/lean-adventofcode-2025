import LeanAdventofcode2025.Day1

namespace Basic

def listTestRepsonses: IO Unit := do
  let input <- Day1.readTestInput
  Day1.response1 input;
  Day1.response2 input;
  return Unit.unit

def listRepsonses: IO Unit := do
  let input <- Day1.readInput
  Day1.response1 input;
  Day1.response2 input;
  return Unit.unit
