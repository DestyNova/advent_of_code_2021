# Day 23: [Amphipod](https://adventofcode.com/2021/day/23)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day23/Part1.hs) (06:05:52, rank 4527)), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day23/Part2.hs) (15:25:39, rank 5111)*

A new record in exasperation and desparation.

## Part 1

I was really stumped by this for a little while, and decided to try tackling it as a game tree search problem using [iterative deepening search](https://en.wikipedia.org/wiki/Iterative_deepening_depth-first_search). It took a really, really long time to just express the constraints correctly, so for several hours I was getting either not enough neighbouring nodes, or too many.

Painful, but eventually it got the job done, just about.

## Part 2

This is where the panic and confusion started to set in, and as I got more and more tired it became even harder to see the wood for the trees.

At first I thought that it might be possible to use the trick of mapping the state to a fixed size integer, which could then be used as an index into an array or table for running Dijkstra's algorithm (which I'd already used on day 15). However this method would require a lot of storage if placed in a flat array -- there are 27 slots that can hold an amphipod, and each slot can have one of 5 states (A, B, C, D or empty). This means we need 27*3 == 2^81 bits to represent the game state, which equates to 2417851639229258349412352 different "slots". I abandoned the idea early on, but after hours, and I mean __many hours__ of slogging with IDS and trying to come up with clever heuristics for pruning the state space, it was clearly not going to work that way. At some point I killed the program while searching depth 22 on the sample input, and it had been running for just over an hour.

Eventually I checked the Reddit solutions thread (for the second time this year) and saw that people were actually succeeding with Dijkstra. After thinking about it again, I figured that Dijkstra visits "useful" parts of the game tree first, so it won't get stuck exploring crappy dead-ends like DFS will. So I tried re-implementing Dijkstra with a mutable distance array based on an encoding of bit-shifted values in each legal cell (you can't stop in the spaces outside the rooms, so I skipped them in the encoding), modulo a fixed value (I tried 2^24 up to 2^28 or so) to keep it in fixed bounds.

This was a total failure for some reason, and eventually I tried again with an immutable `Data.Map Grid Int` which allows querying the distance table directly with each discovered game state. I had almost no hope at this point, but was amazed when it completed on the full input in about 10 seconds.

## Reflections

Not sure what to say really. I felt a mixture of elation, relief and annoyance at having overlooked the right solution. You could say that I mentally did a depth-first search when I should have stepped back sooner and thought more about the bigger picture. After several puzzles where I learned lessons that surprised me about the performance benefits of mutability, I guess a blind spot developed where I forgot how significant the branching factor is for search algorithms like IDS/DFS, and failed to appreciate the benefits of a smarter search strategy. All I could think about was "but I can't store an unbounded amount of distance values", and it took many hours before I realised what should have been obvious:

1. Due to the nature of Dijkstra's algorithm, I can expect it to find a solution in a reasonable number of steps.
2. Since it looks at a fixed number of "neighbouring nodes" -- game states that are reachable from the current state -- and completes in a reasonable number of steps, it stands to reason that the storage requirements are bounded if you store them lazily.

A great lesson, but will I remember this next time? There's only one way to find out...
