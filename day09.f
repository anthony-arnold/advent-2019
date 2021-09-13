      subroutine input(val, *)
        common /part/ mode
        integer*8 val, mode
        val = mode
        return
      end

      subroutine output(val, *)
        integer*8 val
        write(*,*) val
        return
      end

      program day09
        external input, output
        integer*8 mem(100000), mem2(100000), mode
        data mem/100000*0/
        common /part/ mode

        read(*,*,end=1) mem
 1      mem2 = mem

c     Run day one
        mode = 1
        call intcode(mem, input, output)

c     Run day two
        mode = 2
        call intcode(mem2, input, output)
        stop
      end
