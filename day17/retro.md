# Day 17: [Trick Shot](https://adventofcode.com/2021/day/17)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day17/Part1.hs) (02:15:58, rank 6580)), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day17/Part2.hs) (03:02:25, rank 6672)*

Not sure what happened here. Both parts were pretty simple in retrospect, but I got really confused at the beginning and went down the wrong path.

## Part 1

I basically built most of part 2 in my solution for part 1, then realised it didn't make sense and simplified it. I was trying to find the number of steps where the target area is reached on the x-axis, but only later realised that that's really an infinite number of steps when the x velocity reaches zero. I should have started with y, since I initially thought it could be arbitrarily high, but then realised (by looking at the output of `scanl` on the console) that the probe when fired upward will always pass back through y=0, at which time its velocity will be the negative of its initial velocity. This means that the maximum value is constrained to be smaller than the negation of the deepest point in the target range.

## Part 2

This part went ok after all the previous hassle: for each value of y and the number of steps that produced it, find the values of x that can reach the target area in the same number of steps.

Sounds simple when you say it.

## Reflections

* Again, thinking things through more carefully at the beginning and maybe drawing something on paper would have revealed that I needed to just figure out the y situation first.
