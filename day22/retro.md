# Day 22: [Reactor Reboot](https://adventofcode.com/2021/day/22)
*Haskell: [Part 1](https://github.com/DestyNova/advent_of_code_2021/blob/main/day22/Part1.hs) (00:50:40, rank 3451)), [Part 2](https://github.com/DestyNova/advent_of_code_2021/blob/main/day22/Part2.hs) (03:27:24, rank 2138)*

Very cool puzzle that got me thinking hard about spatial relationships between cubes. A nice throwback to Conway Cubes, although not enough overlap with that puzzle that I could lift the code I wrote last year.

## Part 1

To start, I parsed the input a list of tuples `(Bool, Region)` where `Region` is a range in each axis. Next, I created a big 3D array and just brute force applied all the flips. Not too bad.

## Part 2

It was sort of obvious where this one was going: instead of dealing with a 50x50x50 region of space, deal with unlimited scale. This immediately ruled out the array option, so I had to figure out how to represent a set of possibly intersecting cubes and allow efficient deletes.

What I came up with in the end might not be super elegant but it worked:

1. Walk the list of flips.
    1. For "on" flips, add the region to a list of `(xRange, yRange, zRange)`, where each range is a tuple of `(minVal, maxVal)` on that axis.
        * I think there's a function that does this in Haskell, but I couldn't remember it so just applied min and max separately.
    2. For "off" flips, it gets very interesting. I sat and looked at a Rubik's cube for a while, then wrote down some equations to remove a region `d` from another region `r` by removing `r` from the region list and inserting 6 new regions:
        1. Everything in `r` to the left of `d`.
        2. Everything in `r` to the right of `d`.
        3. Everything in `r` above `d` and within its `x` range.
        4. Everything in `r` below `d` and within its `x` range.
        5. Everything in `r` in front of `d` and within its `x` and `y` ranges.
        6. Everything in `r` behind `d` and within its `x` and `y` ranges.
    3. Note that the delete operation needs to apply to every region in the original list since there may be multiple overlapping regions. So this could generate thousands of new subregions.
2. When all flips are performed, recurse over the remaining region list and count from 0.
    1. If there are no more regions, return the count.
    1. If there's at least a region `r`:
        1. Add its size (product of length in each dimension) to count
        1. Delete `r` from the region list, treating it like another "off" flip.

This worked well except I got mixed up with the recursion step and had done a fold over the original list instead, which isn't what you want, since the deletion step might completely alter the list (e.g. removing all other elements if they're contained within the deleted region). Also, it turned out I hadn't figured out the subregion generation properly in the deletion step, and was accidentally causing some of the newly generated subregions to be larger than they should.

## Reflections

* Thinking in 3 dimensions can be hard.
