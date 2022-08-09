# Day 5: [Hydrothermal Venture](https://adventofcode.com/2021/day/5)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day5/Part1.hs) (00:28:39, rank 3916), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day5/Part2.hs) (00:52:45, rank 4469)*

This was a fun problem that seems well-suited to good old mutation, but I decided to stick with plain old immutable `Data.Map` which turned out to be computationally okay.

## Part 1
This time parsing was super easy, and it just took me a while to wrap my head around how to generate the individual points from the segments. I missed the part about considering only straight lines but it didn't take too long to fix that.

## Part 2
Today's part 2 seemed like a tiny step beyond what I was already doing -- in fact I expected it to work just by removing the straight line filtering step from part 1, but it turned out I was generating the points incorrectly.

All I wanted was to say "give me all points (x,y) where x is in the range (x1..x2) and y is in the range (y1..y2)", but in Haskell you need to specify a range in the form `[x1, next number on the way to x2 .. x2]`, and they have to be different numbers or you get an infinite list.
So you have to check if `x1 == x2 && y1 == y2` and draw the steps from `[0]` in that case; otherwise the steps are from zero to the maximum absolute change in either dimension, which then gets multiplied by the direction we're moving in (i.e. x could be going down and y could be going up).

I'm certain that there's a really simple, elegant way to express all of that, but with 4 hours' sleep it hasn't occurred to me. If you're reading this and can see a nicer way, I'd love it if you would open an issue (or PR adding an improved version). In the meantime I'll scour a few other repos and see how other people did it.
