# Day 2: [Dive!](https://adventofcode.com/2021/day/2)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day2/Part1.hs) (00:14:07, rank 8523), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day2/Part2.hs) (00:25:14, rank 9462)*

## Part 1
Argh, I forgot how to use Parsec properly. I originally parsed the directions and kept them as strings, but refactored a bit after submission and parsed them as a `Dir` sum type, although I didn't bother storing the numeric values with them since it was more convenient to pull them out of a tuple later.

## Part 2
I suspected I would mix up `foldl` and `foldr`'s evaluation semantics and I did, so for a while I was processing the list in reverse. This didn't matter in part 1, but for part 2 it was multiplying by the wrong aim values. D'oh.
