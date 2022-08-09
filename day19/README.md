# Day 19: [Beacon Scanner](https://adventofcode.com/2021/day/19)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day19/Part1.hs) (10:32:56, rank 4356), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day19/Part2.hs) (10:51:54, rank 4218)*

Ah, the true descent into madness begins.

## Part 1

By far the hardest part 1 so far for me. Really, I actually did spend 10 and a half hours on part 1. What I was doing seemed like it should work, but I messed up my `repeatF` function for repeatedly applying cube rotations to the scanner.

Even after finding that, it still wouldn't work even for the sample input. I was still not sure that the rotations were correct, right up until I made a seemingly small change that solved the problem. At this point I'm not even sure what fixed it, but it was something to do with how I returned candidate lists of a scanner's points transformed to be from scanner 0's frame of reference.

This was a great puzzle but quite an unpleasant experience which had me questioning my basic intelligence and ability to solve problems like this.

## Part 2

Part 2 was relatively trivial after part 1 was working -- I was already passing back the offset of each scanner after it had been rotated to its correct orientation, but was discarding them somewhere. So part 2 just involved keeping those offsets (which I called `loc`) in the state returned back up to the `match'` function and pushed onto the accumulator. The Manhattan distance comparison itself is easy, just the maximum distance calculator between all pairs of scanner locations.

## Reflections

I don't even know. The program grew and ended up with debugging expressions everywhere which didn't help much. Guess I can't draw too much from this, but I'm pretty disappointed in how it went. It was hard but I don't think it's really "get stuck for 11 hours" hard. Maybe I needed to take more breaks and look at it all again piece by piece.
