# Day 20: [Trench Map](https://adventofcode.com/2021/day/20)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day20/Part1.hs) (01:15:49, rank 2702), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day20/Part2.hs) (01:41:28, rank 3097)*

## Part 1

This one seemed straightforward enough and a very typical Advent of Code sort of puzzle. After dealing with the usual off-by-one sort of error, I soon ran into the classic problem where the sample input passes and the full input fails.

After a fair bit of debugging, I didn't know how to proceed since there's no other example data that might help narrow down the issue, although if I'd looked closely at what was going on (especially at the border of `#` characters appearing around the resulting grid), I cheated and checked the Reddit discussion thread for solutions and spoilers. Within about 10 seconds I had the scoop; a clue that the infinite grid of initially dark pixels around the defined region was flashing on every step. Looking at the "algorithm" confirmed what I should have realised myself: bit zero is a 1, meaning that a 9x9 window of fully dark pixels will produce a lit pixel in the middle. Apply this to the infinite grid and you get the effect that every pixel around your defined area will alternate between light and dark on each step.
Rather than account for this properly, I was able to grow the reference frame by an extra 2 cells in each dimension instead of 1, then shrink it back when counting the `#` pixels in the result. Messy but it worked.

## Part 2

This is where my "grow extra + shrink" tactic failed and I had to actually model the infinite grid behaviour, which turned out to be much simpler than the grow/shrink logic: when deciding the state of an out-of-bounds (OOB) cell in the sliding window calculation, keep a boolean flag indicating whether we're on an odd or even step, and use that to derive the OOB default value.

For the first step, I started with the flag set to false, producing `.` as the OOB value. On the next step (with the centre grid expanded by 2 on each axis), we toggle the flag and produce `#` as the OOB values.

This time computation time wasn't really an issue, even though I used good old nested linked lists to store the grid at each step. This produced the final result in 6.5 seconds, so I imagine an array implementation would be much quicker.

## Reflections

* I was disappointed not to have figured out the "catch" to this one and had to look for a hint, but after yesterday's nearly 11 hour fiasco I didn't have it in me to repeat that kind of slog today.
* Be more suspicious of phrases like "being careful to account for the infinite size of the images". This was one of a couple of clues that there was some important behaviour that needed to be thought about and modelled.
