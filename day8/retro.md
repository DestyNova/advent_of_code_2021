# Day 8: [Seven Segment Search](https://adventofcode.com/2021/day/8)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day8/Part1.hs) (00:21:38, rank 6191), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day8/Part2.hs) (01:44:13, rank 5065))*

Another cool puzzle, this time involving a jumbled 7-segment display we had to decode. Reminiscent of the monster jigsaw puzzle from last year but not quite as mindmelting.

## Part 1
This part was pretty easy but I got stuck in parsing for a while.

## Part 2
Here's where it got really interesting. I think there were clever ways of doing this with constraint programming (probably a good use for Picat), but I chose the brute force approach and just generated all 5040 permutations of the letters `abcdefg`, then checked each code in the current input line against a template for each character to see if there was a match. I used sets for the matching since indices `[2,5]` produce the same digit as `[5,2]`.

This took me a really long time, and I got stuck at some point trying to lift `[Just a]` up to `[a]`, before discovering `catMaybes` in the `Data.Maybe` package. Another problem where it might be beneficial to sketch the solution out on paper before proceeding, although whenever I do that, the initial sketch is invalidated quite quickly...
