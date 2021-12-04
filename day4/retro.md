# Day 4: [Giant Squid](https://adventofcode.com/2021/day/4)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day4/Part1.hs) (01:11:37, rank 7001), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day4/Part2.hs) (01:20:23, rank 5952)*

## Part 1
Another massive struggle mainly due to poor use of Parsec. I only realised after hacking for a long time that I was getting a single board which was the individual boards concatenated. Turned out it was mostly caused by me using `sepEndBy` instead of `sepEndBy1`. So frustrating. Hopefully I'll get more competent at parsing...

## Part 2
After part 1 I was worried this would be a massive step, but nope, I was able to just filter out winning boards as I marked them until there was only one left. I did get tripped up by the fact that multiple boards can win at once, except the final board of course, so I needed to exclude the set of winning boards at each move, and reduce the remaining count accordingly.
