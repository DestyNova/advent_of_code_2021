# Day 4: [The Treachery of Whales](https://adventofcode.com/2021/day/7)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day7/Part1.hs) (xx:xx:xx, rank ???? while the stats are disabled), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day7/Part2.hs) (xx:xx:xx, rank ????)*

I'm really enjoying the ludicrous storyline that goes along with these puzzles. This time, we had to help a swarm of submarine-piloting crabs get to the optimal location to detonate a hole in the seafloor above a hidden cave, taking us to safety from a whale that's about to eat us. Yesssss!

Today's puzzle was a bit simpler than previous days, in fact deceptively so...

## Part 1
Parsing was trivial since it's just comma-separated numbers. I took one look at the problem and went "aha, there's going to be some trick like yesterday where you do a naieve version in part 1, then have to rewrite it in part 2 because the problem is scaled up". So I searched for a trick to find the minimum value in a list of absolute differences, and found that the median works. Easy.

## Part 2
This time we did scale up the problem, but also changed it so that the fast method I used in part 1 would not work, and I'd have to just implement the slow way:

* Walk through all possible locations, call it `loc`.
* For each `loc`, calculate the total cost for all crabs to move there from their current position.
  * Instead of summing the sequence of step costs for each crab `sum [1..steps]` to get to `loc`, you can use the series sum formula `steps*(steps+1) / 2`.
* find the minimum total fuel cost we calculated.

This wasn't difficult, but things would have been quicker if I'd just done it that way in part 1.
