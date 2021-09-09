# Advent of Code - 2019

Solutions to AoC 2019 in Fortran 77.
    
Solutions requiring `IntCode` will need to first compile that module:

    gfortran -x f77 -c intcode.f -o intcode.o

Most solutions are compiled with

    gfortran -x f77 dayXX.f intcode.o -o dayXX
    
and then run with

    ./dayXX < dayXX.txt
