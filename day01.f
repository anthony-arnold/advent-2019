      program day01
          integer x, tot
          tot = 0

 1        read (*,*,END=2) x
          tot = tot + x / 3 - 2
          goto 1
 2        write(*,*) tot    
      stop
      end
