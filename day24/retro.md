# Day 24: [Arithmetic Logic Unit](https://adventofcode.com/2021/day/24)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day24/Part1.hs) (13:25:16, rank 4399)), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day24/Part2.hs) (13:26:57, rank 4270)*

Another tough one! This time both parts were almost equivalent, unless you found a very specific solution to the maximisation of the 14-digit number in part 1.

## Part 1

I got off to a decent start parsing the input and writing a virtual machine for it in Haskell, assuming that I could use some sort of semi-smart brute force search with pruning again, a bit like the Dijkstra or DFS approaches I used before.

It's a good thing that I was able to implement the parser and simulated ALU quickly because it turned out to be the wrong track. Once I realised that naieve backtracking or graph search was probably not going to work, I started manually going through my program instructions and converting it into a more compact set of expressions that could be reasoned about. This quickly led to some realisations about the structure of the data:

* First, it consists of 14 almost identical blocks of code; one for each digit of the input.
* The first 4 blocks use `z` directly in the last part of the calculation of what I assumed was the penalty for using incorrect digits.
* The other blocks, except for 3 (I didn't notice this until much later, having wasted a lot of time trying to find a solution), divide `z` by 26 at each stage.
    * The number 26 seemed strange; I assumed at first that it would be relevant for converting an input number to a string of letters, but that wasn't the case.
* Shortly before the end of each block, `z` needs to be in the range `[0..25]` so that it's reduced to zero by the truncated division by 26 that happens next.
* Each block has two constants which I called `c1` and `c2`, which modify the value of the last digit read in the modulo step and the calculation of the penalty term.

After a couple of hours I realised I was exhausted, having slept only a few hours last night and the one before, so bailed and took a nap for a couple of hours. In the afternoon I picked it up and kept analysing the data and trying to find a logical way to constrain the search by working backwards from the end of the program and seeing what values of `z` can be valid in relation to the digits encountered at each step.

However, I just couldn't crack it with the analytical approach. Maybe some more sleep would have helped, and the incorrect assumption that only the first 4 blocks do `div z 1` instead of `div z 26` may also have added to my confusion.

After another few hours of fruitless analysis, I decided to try using the slightly mysterious [Picat language|http://picat-lang.org] to frame the puzzle as a constraint satisfaction problem. I wasn't sure how to do this and the documentation is more of a reference manual than a practical guide, but eventually I figured out how to specify the constraints in the right form -- for example, it seems you have to specify basically everything in a sort of functional form that can presumably be compiled into set of SAT or SMT expressions. This means writing things like `cond(X + Y #> Z, 1, 0)` rather than if-statements and assignments.

Once I got the hang of it, I was able to simplify and shrink my huge first specification into a one-page list of expressions that described the ALU program correctly. However my first few attempts to solve the solution using the built-in `cp` solver (which seems to be the preferred one) was a failure, with no results at all generated after about 10 minutes of runtime.

Picat can also talk to the external SMT solvers z3 and CVC4, the MIP/linear solvers CBC and a couple of others, but none of them worked for me.

I was starting to think I'd fundamentally misunderstood something when I gave the built-in SAT solver a go, and was surprised when it started producing results rapidly, and discovered the maximum value after about 80 seconds. Phew!

## Part 2

This is probably my quickest split this year -- I just had to switch the solver goal from `$max(TotalCost)` to `$min(TotalCost)` and wait another minute or so.

I have to say I'm really impressed by how easy and clear it is to express relatively complex constraints and transformations in Picat, which is a sort of Prolog with some built-in functions and constraint solving abilities that make it quite amenable to competitive programming (except that it's an interpreted VM and so can't generate executables or C output which would be needed for competitive programming sites like Codeforces etc).

I'll probably go back and try solving some of the previous problems with Picat -- the best resource I've found so far for example Picat programs (this is what I look for when trying to use a new tool) seems to be [Hakan Kjellerstrand's page](http://hakank.org/picat) (which curiously, just like the Picat homepage, doesn't support HTTPS). The built-in "tabling" support looks really useful for DP problems, and allows specifying table predicates that restrict the kinds of values that are memoised. So that's pretty neat. I suspect that parsing complex input is more of a hassle compared to Parsec in Haskell but hey.

## Reflections

Even though I've been trying to do all these challenges in Haskell, sometimes you should cut your losses and try something else, especially if your health or family is suffering. At least now I have a bit more intuition about when to rely on a dedicated solver rather than trying to brute force the problem or continue analysing it indefinitely.
