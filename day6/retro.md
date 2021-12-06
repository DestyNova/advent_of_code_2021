# Day 5: [Lanternfish](https://adventofcode.com/2021/day/6)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day6/Part1.hs) (00:10:47, rank 3435), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day6/Part2.hs) (01:19:00, rank 8227)*

Oh, this was a bit clever. I really messed up in part 2 but still enjoyed it. Well, not quite so much until I figured it out.

## Part 1
This part was relatively straightforward -- very simple parse, then just make a recursive function to step through the states and update the fish population.

## Part 2
Here, I really went off the rails. It was clear that I couldn't actually store a number for every fish, since the growth is exponential.
At this point it might have been good to sit down and think carefully for a few minutes, but instead I jumped to the conclusion that there would be a simple exponential function to produce the result in one step.

So I jumped into Google Sheets and regression calculators to try to narrow it down, but while the results were close, they were always off a bit. For starters, there's always an integer number of fish.

After a while I went to the fridge to get some strawberry cheesecake -- known for its mentally restorative properties and general healthiness -- and some dials started to click in my head. Then it occurred to me that we only have to deal with the *counts* of fish in each state.

* A lanternfish can only have a timer in one of the following states: 0, 1, 2, 3, 4, 5, 6, 7, 8.
* At each step, *N* fish have a timer whose value is 0, which is then updated to 6, and *N* new fish are inserted with a timer value of 8.

Instead of tracking individual fish, we can just track the timer values in a map from `Time -> Count` (I used `Data.IntMap`). For each step:
  * Get the count `N` of fish for timer value `0`.
  * Update the key `0` to `9`.
  * Subtract 1 from every key.
  * Get the count `K` of fish for timer value `6`.
  * Update the count of fish for timer value `6` to `N + K`.

That's not exactly what I did, in the exact order, but it's basically the idea. It was a bit awkward doing it with immutable maps, and I got lost for a while dealing with intermediate maps with names like `m'`, `m''` and `m'''`. Eventually I renamed each intermediate map to give a clue what was being done (`mDec`, `mNew`, `mReset`) whichrevealed that I'd mixed them up. It's probably a good idea to avoid more than one "m-prime" name and be more explicit and verbose in those cases.

Anyway, good fun but a reminder that you need to stop and think sometimes before trying to force your way through.
