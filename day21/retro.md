# Day 21: [Dirac Dice](https://adventofcode.com/2021/day/21)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day21/Part1.hs) (00:19:31, rank 2023), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day21/Part2.hs) (03:22:13, rank 3933)*

Dynamic programming? Dynamic programming.

## Part 1
This part was easy enough that I quickly got suspicious of what would come in part 2...

## Part 2
This part was fairly simple once I wrapped my head around how the rolls should play out, taking 3 rolls at a time (so, 1 way of making 3, 3 ways of making 4, 1 way of making 9 etc).

The awkward part was doing DP in Haskell with maps. AFAIK there's no mutable maps/dicts in the standard library which is a bit of a bummer, so I had to just pass the maps around and union them together. This was pretty wasteful so the solution took about 6 minutes on the full input. This meant testing the program on the sample input was extremely costly -- in fact the sample input required about a minute longer to run.

Out of curiosity, I reimplemented it in Python, which was actually a bit awkward to write in slightly more imperative form, but unsurprisingly it was much faster using mutable dicts -- it produced the correct answer in 145 ms, about 2400 times faster than the Haskell version using immutable maps. Pity there's no mutable `ST` maps in the standard library.

__Several hours later...__

Another bout of curiosity took hold and I ported the Python version to Rust, and found that the `--release` compiled version needs about 25 ms to produce the same answer. I'm feeling like Rust might be an alright general purpose language for problems that are well suited towards mutation, but it still feels very verbose and doing even simple things requires a sequence of debates with the compiler.

__The next evening...__

Clever Haskeller and fellow AoC nerd [@ethercrow](https://github.com/ethercrow) gave me a hot tip: since we know the maximum sizes of each part of the game state, it can be encoded in a fixed number of bits, and indeed it fits into even a 32-bit integer. This means we can use `STArray s Int (Int,Int)` as a mutable table for dynamic programming.

I defined a conversion function using binary or `(.|.)` and `shiftL` operations from `Data.Bits` to convert each game state to a numeric index in the array as follows:

* Player 1 position (1-10): 4 bits
* Player 2 position (1-10): 4 bits
* Player 1 score (0-21): 5 bits
* Player 2 score (0-21): 5 bits
* Player 1 turn? (boolean): 1 bit

After rewriting the program to stop passing around updated versions of the DP table and use `readArray` and `writeArray` instead, it now completes in about 85 ms, which puts it halfway between the Rust and Python versions. I also switched from `Integer` to `Int` types because the results fit in 64 bits (as evidenced by the Rust version), but this didn't shave much noticeable time off.

All in all I'm pretty happy with this, even though it won't always be possible to map any problem's state into an integer index like this. I was pleased to notice that the conversion to use the `ST` monad and mutable updates was much quicker and smoother than in previous puzzles, which makes sense now but I had some struggles before where it seemed like it would never get easier. Guess my neural net is still switched to learning mode -- good to know.

## Reflections

* Mutability is king again. Also, learn to love dynamic programming and recognise when it'll be needed.
* Maybe there's a better way to do DP with a non-integer key in Haskell, but I'm not sure what it is. Tikhon Jelvis has a cool article about a [lazy dynamic programming technique](https://jelv.is/blog/Lazy-Dynamic-Programming/) which I've used before, but it only really works where you can enumerate different values of the argument (e.g. "the nth Fibonacci number"). In this case I'm storing a tuple containing the complete game state (player 1 position, player 2 position, player 1 score, player 2 score and a flag indicating whether it's currently player 1's turn).
    * Update: someone gave me a hint that allowed me to achieve this in Haskell with good results; see the "The next evening" section above. Thanks again [@ethercrow](https://github.com/ethercrow)!
