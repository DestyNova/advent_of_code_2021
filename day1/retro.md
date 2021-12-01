# Day 1: [Sonar Sweep](https://adventofcode.com/2021/day/1)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day1/Part1.hs) (00:05:22, rank 3082), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day1/Part2.hs) (00:09:03, rank 2071)*

## Part 1
Quite an easy opener for day 1. I started writing a recursive solution before realising `zip` was more appropriate to the task. I'm probably a little bit rusty, and also running on about 4 hours of sleep. I fell asleep around 10:30pm, woke up at 2:30am and gave up trying to get back to sleep at 4am, opting instead to prepare for the first coding puzzle by watching videos of someone repairing cars...

As usual, a nontrivial amount of time was also taken up by reading the problem text. For some reason I always feel compelled to go through it slowly, where a lot of experienced puzzlers just seem to hone in on the important details and jump straight to work. That's probably a useful skill to work on... I can always come back and read the flavour text later.

## Part 2
Going from part 1 to part 2 was very trivial since it just required the use of `zipWith3` with the tail and the tailtail of the input list to sum over the sliding windows. Definitely the kind of situation that is well suited to Haskell.
