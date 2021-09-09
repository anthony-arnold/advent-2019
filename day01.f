      function fuel(mass)
        integer mass, fuel
        fuel = 0

 98     mass = mass / 3 - 2
        if (mass .lt. 1) go to 99
        fuel = fuel + mass
        go to 98
        
 99     return
      end function
      
      program day01
          integer x, part1, part2, fuel
          part1 = 0
          part2 = 0

 1        read (*,*,END=2) x
          part1 = part1 + x / 3 - 2
          part2 = part2 + fuel(x)
          goto 1

 2        write(*,*) part1, part2
      stop
      end
