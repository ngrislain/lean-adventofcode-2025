import LeanAdventofcode2025.Day1

namespace Basic

def listTestRepsonses: IO Unit := do
  let input <- Day1.readTestInput
  Day1.response1 input;
  Day1.response2 input;
  return Unit.unit

def listRepsonses: IO Unit := do
  let input <- Day1.readInput
  IO.println "  ____              _ \n |  _ \\  __ _ _   _/ |\n | | | |/ _` | | | | |\n | |_| | (_| | |_| | |\n |____/ \\__,_|\\__, |_|\n              |___/   "
  IO.println "Part 1:"
  Day1.response1 input;
  IO.println "Part 2:"
  Day1.response2 input;
  return Unit.unit
