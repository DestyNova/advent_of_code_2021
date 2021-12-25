# Day 10: [Syntax Scoring](https://adventofcode.com/2021/day/10)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day10/Part1.hs) (00:16:26, rank 3531), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day10/Part2.hs) (00:25:17, rank 2757))*

Bit of a change of pace here with a puzzle that demanded a straightforward stack solution. This one was easier than previous days.

## Part 1
After a few minutes reading the text, I knew the most straightforward way to achieve this was by pushing valid opening symbols onto a stack, and popping when a matching closer is encountered. If an invalid closer is encountered, then we return the points for that character. If we run out of characters, regardless of the stack's state, we move on.

## Part 2
Having already gone with the stack route, this was pretty simple -- just pass back the remaining stack and convert each symbol to its point value before doing the requisite list manipulation to pull out the middle value. At first I was getting wrong outputs on the sample input because I was reversing the leftover stack before recursing over it to calculate the score, but of course it was already reversed when it was pushed onto the stack.
