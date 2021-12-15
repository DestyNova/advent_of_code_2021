#!/usr/bin/env bash

# Delete intermediate object files
find . -maxdepth 1 -regex '\./Part\(1\|2\).*\.\(o\|hi\)$' -delete
# Delete binaries (i.e. "Part1", "Part2Arr" etc with no extension)
find . -maxdepth 1 -regex '\./Part\(1\|2\)[^\.]*$' -delete
