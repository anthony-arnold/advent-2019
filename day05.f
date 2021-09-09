      subroutine day05_input(dst)
         integer i
         common/input/ i
         integer dst
         dst = i
         return
      end

      subroutine day05_output(val)
        integer val
        write(*,*) val
      return
      end

      program day05
         integer i
         common/input/ i
         external day05_input, day05_output
         integer mem(1000), mem2(1000), n

        read(*,*,end=1) mem

 1      do n = 1,1000
           mem2(n) = mem(n)
        end do

        i = 1
        call intcode(mem, day05_input, day05_output)

        i = 5
        call intcode(mem2, day05_input, day05_output)
      stop
      end
