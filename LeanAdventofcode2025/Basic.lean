import LeanAdventofcode2025.Day1
import LeanAdventofcode2025.Day2
import LeanAdventofcode2025.Day3
import LeanAdventofcode2025.Day4

namespace Basic

-- Use https://www.asciiart.eu/text-to-ascii-art to generate the ASCII art.


def listRepsonses: IO Unit := do
  IO.println r" ____              _
|  _ \  __ _ _   _/ |
| | | |/ _` | | | | |
| |_| | (_| | |_| | |
|____/ \__,_|\__, |_|
             |___/   "
  IO.println "Part 1:"
  Day1.response1;
  IO.println "Part 2:"
  Day1.response2;
  IO.println ""
  IO.println r" ____              ____
|  _ \  __ _ _   _|___ \
| | | |/ _` | | | | __) |
| |_| | (_| | |_| |/ __/
|____/ \__,_|\__, |_____|
             |___/       "
  IO.println "Part 1:"
  Day2.response1;
  IO.println "Part 2:"
  Day2.response2;
  IO.println ""
  IO.println r" ____              _____
|  _ \  __ _ _   _|___ /
| | | |/ _` | | | | |_ \
| |_| | (_| | |_| |___) |
|____/ \__,_|\__, |____/
             |___/       "
  IO.println "Part 1:"
  Day3.response1;
  IO.println "Part 2:"
  Day3.response2;
  IO.println ""
  IO.println r" ____              _  _
|  _ \  __ _ _   _| || |
| | | |/ _` | | | | || |_
| |_| | (_| | |_| |__   _|
|____/ \__,_|\__, |  |_|
             |___/        "
  IO.println "Part 1:"
  Day4.response1;
  IO.println "Part 2:"
  Day4.response2;
  return Unit.unit
