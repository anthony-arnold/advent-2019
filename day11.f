      program day11
        integer*8 mem(100000), mem2(100000)
        integer*8 panels(80,80), x, y, dir, iout, n
        character*40 prnt
        external input, output
        common /state/ panels, x, y, dir, iout, n
        data dir/0/, iout/0/, n/0/, mem/100000*0/
        data panels/6 400*-1/
        data x/40/, y/40/

        read(*,*,end=1) mem
 1      mem2 = mem
        call intcode(mem, input, output)
        write(*,*) n

c     reset
        do x = 1,80
           do y = 1,80
              panels(x,y) = 0
           end do
        end do

        panels(40, 40) = 1
        x = 40
        y = 40

        call intcode(mem, input, output)
        do y = 1,80
           do x = 1,80
              if (panels(x,y).eq.1) then
                 prnt(x:x) = '1'
              else
                 prnt(x:x) = ' '
              end if
           end do
           write(*,*) prnt
        end do

      stop
      end

      subroutine input(val, *)
        integer*8 val
        integer*8 panels(80,80), x, y, dir, iout, n
        common /state/ panels, x, y, dir, iout, n

        val = panels(x, y)
        if (val .eq. -1) val = 0
        return
      end

      subroutine output(val, *)
        integer*8 val
        integer*8 panels(80,80), x, y, dir, iout, n
        common /state/ panels, x, y, dir, iout, n

        iout = iout + 1
        if (iout .eq. 1) then
           if (panels(x, y).eq.-1) then
              n = n + 1
           end if
           panels(x,y) = val
        else
           if (val.eq.0) then
              dir = dir - 1
              if (dir .eq. -1) dir = 3
           else
              dir = dir + 1
              if (dir .eq. 4) dir = 0
           end if

           select case (dir)
           case (0)
              y = y - 1
           case (1)
              x = x + 1
           case (2)
              y = y + 1
           case (3)
              x = x - 1
           end select

           iout = 0
        end if
        return
      end
