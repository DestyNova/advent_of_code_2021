# Day 14: [Extended Polymerization](https://adventofcode.com/2021/day/14)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day14/Part1.hs) (00:30:02, rank 5210), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day13/Part2.hs) (01:39:46, rank 4873)*

## Part 1

This was a pretty straightforward puzzle, albeit rife for off-by-one errors and so on. After parsing, the most confusing part was probably the de-duplication of the in-between pairs. Not a big deal but it was enough to cost me a few more minutes.

## Part 2

I sort of sensed what was coming while working on part 1, but I didn't feel confident enough to make a gamble and try to implement a non-exponentially-growing state representation until I got to part two and confirmed that it was needed.

The idea of tracking pair counts came to me quickly, and I simulated it manually for one step before implementing it with a foldr over `Data.Map` to generate the new map of counts at each step.

I ran into two difficulties:

### Problem 1. Frequency counting within pairs

It wasn't entirely obvious to me how the frequencies should be ultimately calculated, since `["AB", "BB", "BC"]` need to be handled slightly differently from the perspective of `'B'`. I ended up empirically establishing that taking the maximum of the counts where a character X is in the first position, and where it's in the second position, which seemed to give the correct result. I still don't have a great mental model about it, but it worked.

### Problem 2. Losing information about input pair frequency

This really hurt -- the solution worked fine for the sample input, but failed on the full input. I thought it was an off-by-one error with the aforementioned frequency counting issue, and tried a few +1/+2/etc variations which resulted in a 5 minute ban from submitting more answers.

With 5 minutes to take a breather, I tried comparing the results against part 1's output for N=10, and they were wrong. Then I tried N=1 and that was wrong too. WAT!

A closer look at the initial state quickly revealed that I was inserting each pair in the input into the frequency map, but failing to handle collisions. In my test input, the pair "BO" appeared 3 times, but all pairs received an initial count of 1.

The fix was as simple as:

```
- M.insert pair 1
+ M.insertWith (+) pair 1
```

Apart from this, I probably would have been done at least 10 or 20 minutes quicker. D'oh!

## Reflections

* For simulations of the "run this thing that requires exponential storage for 10 steps" variety, if there's an obvious way to do it without exponentially growing storage, maybe just do that immediately for part 1. Unless it's mad complicated.
* If part 2 is just a "now do this for a much bigger N" challenge, make sure to test the solution with smaller values of N that can be compared with the part 1 solution.
