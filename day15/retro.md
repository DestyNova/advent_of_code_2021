# Day 14: [Chiton](https://adventofcode.com/2021/day/14)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day14/Part1.hs) (?)), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day14/Part2.hs) (00:59:20, rank 5071)*

aka Djikstra's Cave

## Part 1

So I implemented this yet again with `Data.Map` holding weights and distances at each coordinate. This is probably why it's a bit slow...

Also, this is really annoying:

```
Part1.hs: Prelude.foldl1: empty list
```

The hell, Haskell? I had to compile the program with profiling options and run it with special `+RTS` arguments to get a partial stacktrace. Not very impressed with that UX; it harks back to the old Commodore 64 days where you'd type in a 500 line BASIC program and then just get `?SYNTAX ERROR` when you tried to run it.

Anyway in this case it turned out to be the call to `minimumBy`, which does not like to be given an empty list. The list was empty because I was enumerating the neighbours incorrectly (`(i,i+1)` instead of `(i,j+1)` kind of thing). Ugh.

## Part 2

I didn't expect another "now scale it way up" challenge, but it was cool expanding the graph out with that increment/wrap function.

However... the `Data.Map` Djikstra solution is pretty slow... in fact it's been running for at least 10 minutes now. It would have been smarter to use a `UArray` here. Might rewrite it later, but surely I have to let it finish now... surely... it will halt...

## Reflections

Don't be a coward. Use an array or vector for storage even if it's awkward in Haskell.
