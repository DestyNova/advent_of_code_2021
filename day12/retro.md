# Day 12: [Passage Pathing](https://adventofcode.com/2021/day/12)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day12/Part1.hs) (00:33:03, rank 2992)), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day12/Part2.hs) (01:06:17, rank 3733)*

A nice graph-based puzzle that was solvable with DFS.

## Part 1
The first part was pretty straightforward, so I just had to decide how to do DFS. It was more an issue of figuring out how to track the state, what to accumulate etc. I quickly ruled out Haskell's graph implementation because it just felt like a lot of work when I used it before, and it just didn't seem worth the cost. It might make more sense with weighted paths.

Instead, I just assembled a map from vertices to their edges. Predictably, I forgot to add the reverse of each edge, accidentally creating a directed graph. Apart from that, this was alright.

## Part 2
It seemed like what I had to do here was clear-cut, but I failed at reading and somehow took the description to mean that any small cave can be visited twice, when they actually said that any ONE small cave in a path can be visited twice, but all others in that path can only be visited at most once.

After a lot of debugging (one of the pain points of Haskell with its lazy and pure execution model), I got past this, then got stuck for a while on how I was handling the start node. Eventually I rewrote my `cannotVisit` function and broke it down into some simpler predicates, which fixed the problem.

Yet again, I relied on `nub` to tell me if any cave had already been visited twice. This is a bit stinky and probably contributed to the slow runtime (1.4 seconds after compiling part 2). A better solution would probably be to keep another map of vertex to its count within the path, but this was simpler and works.
