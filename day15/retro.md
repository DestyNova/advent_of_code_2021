# Day 15: [Chiton](https://adventofcode.com/2021/day/15)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day15/Part1.hs) (01:41:05, rank 5572), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day15/Part2.hs) (05:42:08, rank 9008)*

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

__Some time later__

Okay that wasn't really working. I rewrote it using `Data.Array` and... it was still slow. Really, really slow. The main slowness was the `getClosest` function, which walks over the entire weight array. I let it run and cook the room while I worked on an implementation of [leftist heaps|https://en.wikipedia.org/wiki/Leftist_tree] which would allow for a much more efficient priority queue (the slowest part of my Djikstra implementation). Eventually it produced the correct answer after almost 2 hours, literally not cool.

After submitting the answer, I debugged the leftist heap implementation and accepted that I'd just generate a ton of garbage by not removing old distance values when inserting new lower ones, because removal of an arbitrary node seems to be __O(n)__ for leftist heaps, and heaps in general, which sucks. Some of the material I could find discussing trees mentions a "decrease-key" operation, but they all start with the sneaky caveat "assuming you have a pointer to the node" -- but of course I don't. I only have the key, which is to say, the distance, and getting a reference to the node probably requires a complete traversal of the tree. It might be possible to do this more efficiently with Fibonacci or binomial heaps, but my brain battery is empty already.

In any case, running Djikstra with arrays and a leftist heap priority queue produced an identical result after about 9 minutes, which is a pretty decent speedup of about 12x.

__Some time even later__

Out of curiosity I tried implementing the Bellman-Ford algorithm, which seems like a less appropriate tool for the job of finding the shortest path between two nodes (Bellman-Ford finds the costs of the shortest paths from a source node to EVERY node). Indeed, when I ran it on the small sample input, it took about 4.5 seconds to complete, about 10 times slower than the Djikstra version with a leftist heap priority queue.

So I abandoned that thought there and went shopping. After I returned, the spirit of madness took me and I altered this version to use mutable unboxed arrays in the `ST` monad. This took a while and was very ugly and very difficult to debug. However, it produced the right answer for the sample data in about 0.008 seconds, about 500 times faster than the immutable array updating version.

When I ran it on the full input data, it produced the correct result again, in a delightful time of 3.3 seconds. That's 175 times faster than my Djikstra implementation with leftist heaps. Wow.

## Addendum, 2 days later

A very good Haskell coder in the "Dublin Haskell Meetup" Discord suggested that `Data.Set` has some very efficient operations, including `minView` which could easily serve as a priority queue. This was a great idea, so I implemented it, but found that it was still slow on the full input set. This seemed like a good time to introduce `ST` to the mix, replacing the `UArray` of distances with a `STUArray` and mutating it rather than creating and throwing away new array copies on every iteration.

This sped the program up to a ridiculous 1 second runtime, about 3.3x faster than the Bellman-Ford ST solution. Now I want to try reintroducing leftist heaps to see if it's the same or what.

__Several minutes later__

It's a tiny bit faster, maybe 100ms shaved off -- but it's so close that I'm measuring noise at this point.

## Performance of the different implementations

| Algorithm    | Data representations                                                  | Runtime      |
|--------------|-----------------------------------------------------------------------|--------------|
| Djikstra     | immutable Data.Map for distance table + vanilla lists as vertex queue | 2 hours      |
| Djikstra     | immutable array for distance table + binary heap as vertex queue      | 10 minutes   |
| Bellman-Ford | mutable arrays for everything                                         | 3.3 seconds  |
| Djikstra     | mutable array for distance table + sets as vertex queue               | 1 second     |
| Djikstra     | mutable array for distance table + binary heap as vertex queue        | 0.95 seconds |

## Reflections

* Once again, if you expect to do an absolute ton of updates, get comfortable with the `ST` monad. It might literally be tens of thousands of times faster than generating and throwing away copies of huge arrays in a tight loop.
* Sometimes the dumb algorithm is the right algorithm. Sometimes the right house is the one that's for sale.
* While Haskell may be a beautiful circular beast, it can be an awkward beast when you try to fit it into a square hole.
