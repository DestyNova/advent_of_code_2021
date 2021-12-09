# Day 9: [Smoke Basin](https://adventofcode.com/2021/day/9)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day9/Part1.hs) (00:22:29, rank 5275)), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day9/Part2.hs) (00:58:06, rank 4761)*

Last night was another late one, so I started this after 3 hours of sleep but actually felt fine. I must be acclimatising to this routine, when usually I'll sleep a bit longer if I'm tired. It'll probably hit me soon...

My scores were very mediocre here but I was fairly happy with the result.

## Part 1
This was the first proper 2D grid-based puzzle, and I was waiting for it since there were quite a few last year. Because of this, enumerating the valid neighbours was pretty quick. It would have been a good idea to use `Data.Array`, but I was too lazy and went with much more inefficient lists. Repeatedly doing `grid !! y !! x` should really slow things down, but even in GHCi it wasn't a problem.


## Part 2
At first I wasn't sure how to do this, but then I realised it's basically like paint bucket filling and could be solved with a recursive function. Again, I wondered if sticking with the built-in linked lists and generating so many intermediate lists was going to make this solution incredibly slow, but the compiled version runs in less than 0.1 seconds for the complete input.

Early on, I ran into an infinite recursion while exploring the basin, and had to add an extra check for heights of 8 or above, which can't have any more neighbours since heights of 9 aren't included, and the height must increase with each step away from the source. This last point wasn't very clear in the text, but I assume it makes sense, and it worked on my input.

Just when I thought I was done, another problem presented itself: I was enumerating some of the same points twice, since it could have been a neighbour of a previously explored point. Instead of being clever here, I just zipped up the height values in the basin with their coordinates, and used `nub` later to take only the unique set of points in each basin. Actually there's probably no reason to have included the height values when zipping them, but anyway.

This was a fun one, and I look forward to seeing other people's clever approaches.
