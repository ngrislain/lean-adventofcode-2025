import LeanAdventofcode2025.Day1
import LeanAdventofcode2025.Day2

namespace Basic

-- Use https://www.asciiart.eu/text-to-ascii-art to generate the ASCII art.

def listTestRepsonses: IO Unit := do
  let input <- Day1.readTestInput
  Day1.response1 input;
  Day1.response2 input;
  return Unit.unit

def listRepsonses: IO Unit := do
  IO.println "  ____              _ \n |  _ \\  __ _ _   _/ |\n | | | |/ _` | | | | |\n | |_| | (_| | |_| | |\n |____/ \\__,_|\\__, |_|\n              |___/   "
  let input <- Day1.readInput
  IO.println "Part 1:"
  Day1.response1 input;
  IO.println "Part 2:"
  Day1.response2 input;
  IO.println ""
  IO.println " ____              ____  \n|  _ \\  __ _ _   _|___ \\ \n| | | |/ _` | | | | __) |\n| |_| | (_| | |_| |/ __/ \n|____/ \\__,_|\\__, |_____|\n             |___/       "
  let input <- Day2.readInput
  IO.println "Part 1:"
  Day2.response1 input;
  IO.println "Part 2:"
  Day2.response2 input;
  return Unit.unit
