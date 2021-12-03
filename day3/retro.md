# Day 3: [Binary Diagnostic](https://adventofcode.com/2021/day/3)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day3/Part1.hs) (00:16:25, rank 5806), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day3/Part2.hs) (00:43:40, rank 4523)*

## Part 1
Pretty smooth; a bit early in the morning to wrap my head around navigating the list nicely and efficiently, but it didn't matter. The gamma and epsilon values are basically the complement of each other, but in unsigned N-bit arithmetic, so my attempt to be clever with `Data.Bits.complement` didn't work and I just manually flipped each bit instead.

## Part 2
Mega-wall of text for this one. I'm getting a sense that it's more productive to just jump straight to the worked example rather than trying to comprehend the initial explanation.

Anyway, after a while of reading, I decided to keep track of the column index and use the same function to whittle it down, taking a comparison predicate which decides when to go with ones or zeroes. When it came to the CO2 scrubber rating, I was running out of items in the list and thought my logic with the comparison predicate was wrong, but I'd just failed to read the instruction to stop the filtering when a single element is reached.
