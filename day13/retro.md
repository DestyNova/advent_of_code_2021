# Day 13: [Transparent Origami](https://adventofcode.com/2021/day/13)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day13/Part1.hs) (00:57:42, rank 6230), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day13/Part2.hs) (00:59:20, rank 5071)*

Another poor performance from me, but I really enjoyed the backstory and the puzzle itself.

## Part 1

Once I'd parsed the input into a sparse list of coordinates and a list of `Fold` actions, I needed to decide how to represent the grid in memory. As usual, this is where in a language like Python, you'd just use a two-dimensional mutable array, maintain a current width/height and update the array to overwrite folded positions with a `#`.
In Haskell, working with arrays isn't super straightforward, and I was scared of touching the `ST` monad again after a previous challenge where it backfired spectacularly.

Instead, I opted for my good old friend, the list of lists (that's `[[Char]]` to you). Rather than folding a million times over the coordinates, I built the grid as follows:

* sorted them by y-rank first, then by x,
* determined the grid bounds (maximum x and maximum y in the points list),
* and constructed the grid with a list comprehension.

Now that I think of it, it would have made more sense to fold the coordinates into a Map, for quicker querying during grid construction. Oh well.

Later, I got stuck for a long time because I specified the combination function to `zipWith` in the form `(\(a,b) -> ...)`, forgetting that `zipWith` curries its arguments, so `(\a b -> ...)` is what you need. The error messages here threw me for a loop and I went back through the program, adding type annotations to narrow down the problem with mixed success. Eventually I looked at the signature of `zipWith` again in GHCi and realised my folly. Ugh.

Eventually I got the sample input producing the correct result, but the full input failed. I added a debugging function to render the grid visually, and compared it with the worked example (which could have done with one or two more intermediate folds). It quickly became apparent that I had an off-by-one error in my folding logic, so horizontal and vertical folds were losing one or two lines. D'oh.

## Part 2

Thanks to the debugging step at the end of part 1, this was a trivial addition -- I was already truncating the output with `(take 1 folds)` in part 1, so this just needed to be replaced with `folds` and the final output became visible. This little visual twist in part 2 was a lovely way to end today's challenge -- hopefully we can get some more tricks like that in future puzzles.

## Reflections

* Have to decide early on how best to represent the grid:
  * Pay the performance tax on using lists?
  * Use `Map (Int,Int) Char`?
    * Probably more suitable for sparse grids, as in this case.
      * But would have to manually update indices during a fold, rather than (ab)using `reverse`.
  * Use `UArray`?
    * Feels cumbersome in Haskell, but probably a lot more efficient than lists, and maps too depending on sparsity.
  * Use `STUArray`?
    * I literally couldn't figure out how to work with these last time. Like, I could work with it, but introspecting the grid for debugging proved beyond my brain capacity, producing output like `<ST action>`.
* It's probably better to pause for a minute or two and think, rather than rushing into a decision just because the clock is ticking.
  * This seems to be a general thing that I've failed to learn from the previous challenges where it was also true.
