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
  IO.println r" ____              _
|  _ \  __ _ _   _/ |
| | | |/ _` | | | | |
| |_| | (_| | |_| | |
|____/ \__,_|\__, |_|
             |___/   "
  let input <- Day1.readInput
  IO.println "Part 1:"
  Day1.response1 input;
  IO.println "Part 2:"
  Day1.response2 input;
  IO.println r" ____              ____
|  _ \  __ _ _   _|___ \
| | | |/ _` | | | | __) |
| |_| | (_| | |_| |/ __/
|____/ \__,_|\__, |_____|
             |___/       "
  let input <- Day2.readInput
  IO.println "Part 1:"
  Day2.response1 input;
  IO.println "Part 2:"
  Day2.response2 input;
  return Unit.unit
