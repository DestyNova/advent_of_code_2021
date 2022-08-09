# Day 25: [Sea Cucumbers](https://adventofcode.com/2021/day/25)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day25/Part1.hs) (01:45:19, rank 3238), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day25/Part2.hs) (01:46:09, rank 2248)*

An easy one to close another thoroughly enjoyable and mind-expanding Advent of Code!

## Part 1

This time I made the opposite mistake to my usual approach, and started writing a mutable array-based version in the `ST` monad, assuming that there would be many expensive updates. Once I got it running though, I realised that mutating the array was causing infinite loops when cucumbers wrap around the edge of the toroidal map. This could be fixed by remembering the first element in every row and column and checking against them at the end, but I decided to throw out `ST` and just generate a new array on every step.

This turned out to be perfectly cromulent, and the full version completed in about 200ms and was easier to understand. Not that this prevented me from making some silly mistakes, like:

1. Applying both types of moves to both types of cucumber
2. Assuming the grid was square when it wasn't (I was using the same dimension of length to represent both dimensions)

Other than those hiccups, the puzzle was pretty straightforward and a very Advent-of-Code-ish sort of puzzle, and one that will probably be solved by [smart APL programmers](https://github.com/pitr/aoc/tree/main/2021) in just a few lines.

## Part 2

No part 2, instead, a well earned cup of tea.

## Reflections

This year felt harder than the 2020 round, and I'm not sure if that's me being a year older or sleep deprived, or if there was a difficulty boost. Last year I was almost defeated by the [Jurassic Jigsaw](https://github.com/DestyNova/advent_of_code_2020/blob/main/day20/retro.md) puzzle, and this year the [Beacon Scanner](https://github.com/DestyNova/advent_of_code_2021/blob/main/day19/retro.md), [Amphipod](https://github.com/DestyNova/advent_of_code_2021/blob/main/day23/retro.md) and [Arithmetic Logic Unit](https://github.com/DestyNova/advent_of_code_2021/blob/main/day24/retro.md) puzzles came even closer.

Apart from generally improving my problem-solving skills (I hope), I got much more familiar with some important techniques and tactics, such as:

* Dijkstra's algorithm. This came in really handy in the [Chiton](https://github.com/DestyNova/advent_of_code_2021/blob/main/day15/retro.md) puzzle for traversing the map, and for [Amphipod](https://github.com/DestyNova/advent_of_code_2021/blob/main/day23/retro.md) for finding an optimal plan in a relatively large state space.
* Dynamic programming in general, and a few ways to apply it in situations where it's awkward. For example, in Haskell:
    * If the index is an integer, you can use arrays and maybe mutable `STUArray` for a large speed boost.
    * If not, you might be able to map the state into an integer index (I think this is what [Zobrist hashing](https://en.wikipedia.org/wiki/Zobrist_hashing) is about, although surely collisions would be a problem since you're not going to actually store 2^64 values -- in that case the array might need to store a hashmap or list of key/value pairs, i.e. linear chaining)
    * If not, you can either use 3rd party packages (e.g. [hashtables](https://hackage.haskell.org/package/hashtables)), or stick with immutable updates to a `Data.Map`. This is still much faster than not using dynamic programming, but of course slower than mutable arrays if there's a huge number of updates to do (e.g. if you frequently encounter new states or discover lower costs).
* When to use mutability and when not to. There's a bit of a mental tax involved in using mutable variables and arrays in Haskell, so I tend to start with immutable structures before committing to an `ST` monad rewrite which generally makes things uglier. Sometimes there's an absolutely enormous speedup when switching to mutable arrays, notably in the graph search case where you're making a huge number of updates to a very large table of distances.
* When faced with a really difficult combinatorial decision or optimisation problem, it might be time to reach for a specialised solver for SAT/SMT/MIP problems rather than trying to analyse it for hours looking for a key that unlocks everything. This happened with the [ALU](https://github.com/DestyNova/advent_of_code_2021/blob/main/day24/retro.md) problem and I went pretty deep down the rabbit hole trying to find an elegant solution before switching to Picat's SAT solver.
* Picat is amazing and I'll probably revisit some of the earlier puzzles and solve them again, or maybe pick up the old AoC rounds I didn't participate in at the time.
* Sometimes it's probably a good idea to pause and work stuff out with a pen and paper. This would have helped with the [Reactor Reboot](https://github.com/DestyNova/advent_of_code_2021/blob/main/day22/retro.md) problem and several others where I couldn't really hold everything in my head and my typed notes weren't enough.

Perhaps the most general learning for me has been to try to frame a high-level solution in mind before jumping into code. When I watch recordings of streams from really good programmers, they seem to just immediately know what to do and get going, and they're usually right. In my case my first instincts have been wrong many times and I've had to backtrack. This happened with the ALU puzzle -- I spent time modelling the ALU fully in Haskell before realising that it wasn't necessary (or at least, wasn't sufficient) for solving the puzzle, and instead I needed to reason carefully about the specific program that had been given, and ultimately reduce and transform it into a constraint problem. Afterwards, I watched [Joshua Wise](https://www.youtube.com/watch?v=NEyxCfeBfKs) solving the puzzle and within about 2 minutes reading the description he said "it's a VM puzzle... oh wait, it's NOT a VM puzzle!" and pretty much immediately switched to encoding the program so that it could be passed to the [z3 SMT solver](https://github.com/Z3Prover/z3).

This is probably something that gets better with practice, so I'm looking forward to solving more problems, and to next year's Advent of Code. Happy Christmas!
