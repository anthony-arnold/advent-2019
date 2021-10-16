# Advent of Code - 2019

Solutions to AoC 2019 in Fortran 77.

First build all solutions with `make`. You might need to set the fortran compiler with

    FC=gfortran make

or similar.

Next, run the solution you need with:

    ./dayXX < dayXX.txt

Each solution will output the answer to both parts of the day's problem.

Some problems have input which are not easily digestible by fortran, so an awk script is provided to convert the input into something more sane. For these, use

    ./dayXX.awk dayXX.txt | ./dayXX


## Additional considerations

 - Day 4: Substitute a space `' '` for the dash `'-'` in the input.
 - Day 18: This program only outputs one answer. Modify the output manually for part 2.
