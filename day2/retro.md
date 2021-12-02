# Day 2: [Dive!](https://adventofcode.com/2021/day/2)
*Haskell: [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day2/Part1.hs) (00:xx:xx, rank xxxx), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day2/Part2.hs) (00:xx:xx, rank xxxx)*

## Part 1
Argh, I forgot how to use Parsec properly. Will have to refactor this a bit.

## Part 2
I suspected I would mix up `foldl` and `foldr`'s evaluation semantics and I did, so for a while I was processing the list in reverse. This didn't matter in part 1, but for part 2 it was multiplying by the wrong aim values. D'oh.
