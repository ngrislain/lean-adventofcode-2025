import LeanAdventofcode2025.Day1
import LeanAdventofcode2025.Day2
import LeanAdventofcode2025.Day3

namespace Basic

-- Use https://www.asciiart.eu/text-to-ascii-art to generate the ASCII art.


def listRepsonses: IO Unit := do
  IO.println "  ____              _ \n |  _ \\  __ _ _   _/ |\n | | | |/ _` | | | | |\n | |_| | (_| | |_| | |\n |____/ \\__,_|\\__, |_|\n              |___/   "
  IO.println "Part 1:"
  Day1.response1;
  IO.println "Part 2:"
  Day1.response2;
  IO.println ""
  IO.println " ____              ____  \n|  _ \\  __ _ _   _|___ \\ \n| | | |/ _` | | | | __) |\n| |_| | (_| | |_| |/ __/ \n|____/ \\__,_|\\__, |_____|\n             |___/       "
  IO.println "Part 1:"
  Day2.response1;
  IO.println "Part 2:"
  Day2.response2;
  IO.println ""
  IO.println ""
  IO.println "Part 1:"
  Day3.response1;
  IO.println "Part 2:"
  Day3.response2;
  return Unit.unit
