# Day 18: [Snailfish](https://adventofcode.com/2021/day/18)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day18/Part1.hs) (02:32:58, rank 2057)), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day18/Part2.hs) (02:55:42, rank 2198)*

## Part 1

This felt like the toughest part 1 so far, especially using immutable data representations. Parsing was fine, and I represented the resulting expression as a tree in the `SnailNum` datatype, which can be either a `RegNum Int` (for "regular numbers") or a `Pair SnailNum SnailNum` for a pair of two snailnums.

Where I got really stuck was the explode operation. Traversing the tree and replacing a pair with an atomic value `0` is fine, but how do you propagate the original values outward to the "next leftmost/rightmost" regular number and add them?

I went down a blind alley several times trying to define a single recursive operation that would do it, but just couldn't figure it out. Eventually, I wrote a function `explode'` which tracks an index of leaf nodes (i.e. `RegNum`) while doing a depth-first traversal of the tree up to the point where an explosion needed to happen. Then I'd produce a replacement `RegNum 0` at that point in the tree and pass the leaf index pointing to the newly-inserted `RegNum 0` back up the call stack.

After the top-level call to `explode'` returns, we get a tuple with a leaf index and either the pair of numbers to be added left/rightward or `Nothing`, if there was no explosion.

If there was no explosion, we're done and can try applying a split (this was trivial at least).
If there was an explosion, then we pass the leaf index, a temporary counter, the pair of numbers to be added to the left/right, and the new expression tree to the `increment` function. This function then does another depth-first traversal of the tree, incrementing the counter `i` until it finds a `RegNum` where the condition `(c-1) == i` is true, meaning the first regular number to the left of the explosion point. It then adds the left number of the exploded pair to the current number and moves on until it finds another `RegNum` where the condition `(c+1) == i` is true, corresponding to the next rightmost number after the explosion site, so it can add the right number of the exploded pair.

Even writing about this is taking ages... I really want to see how people managed to do both parts of this in less than 20 minutes.

## Part 2

Part 2 was much simpler than part 1. I was caught out by a couple of things though:

1. At first I thought the combinatorial approach would take too long and maybe there was a way to memoise it, but it didn't seem likely.
2. When combining all pairs, I forgot to compare the reverse of each pair, so only half the combinations were evaluated at first.

My worries about combinatorial complexity turned out to be unfounded when the compiled program finished on the full input in about 0.35 seconds.

## Reflections

* I'm not sure how to best to solve problems like this. I spent more time than usual reading and thinking about the problem before I jumped in to write code, but the "add this to the first leftmost leaf" thing really threw me for a loop.
