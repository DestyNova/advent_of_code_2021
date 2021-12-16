# Day 16: [Packet Decoder](https://adventofcode.com/2021/day/16)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day16/Part1.hs) (01:44:57, rank 3581)), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day16/Part2.hs) (03:19:48, 4879)*

Today's puzzle was reminiscent of variable length encoding protocols, like parsing TCP headers or serialisation formats like MsgPack or Protobufs. Good puzzle but very frustrating to debug in Haskell due to its lazy evaluation, which is usually one of the coolest features of the language, but when stepping through thousands of lines of `Debug.Trace` output it's hard to know what happens when.

Thankfully, this type of problem didn't need any kind of efficient algorithmic tricks or mutable state -- it was more of a thinking puzzle than a pattern-matching one.

## Part 1

This part wasn't too bad -- I had to spend a good bit of time reading the description, and even then only realised sort of late that the input really was hex and not a bunch of binary digits.

## Part 2

This is where things got complicated, and I ended up in the frustrating situation where the examples all work fine but the full input fails. A lot of debugging followed, which consisted mostly of replacing function calls like `bin2dec` with duplicate wrappers (`bin2decPackets`, `bin2decLength` etc) that just call `trace` and proxy through to `bin2dec`.

Then I started doing a binary search on the input file, deleting half, then a quarter, and so on, until I'd reduced it to the first failure point. In this case, it was trying to apply function 7 (`==`) to a list of 5 arguments, when it expects exactly two.

More debugging allowed me to rule out the type `I=1` packets with a fixed count of subpackets, and localised the problem to the `I=0` case, where we don't know how many subpackets there are, only the total length in bits.
As I started to look back at the top level `solve` function, I realised that my handling of the `totalBits` was vague, and my calculation of the bits consumed so far was definitely wrong (probably negative, I think). This forced me to think more clearly about the problem spec and rewrite the function to consume the right amount of bits, which solved the problem.

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
