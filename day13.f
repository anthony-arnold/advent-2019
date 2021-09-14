      program day13
        integer*8 grid(100,100), x(2), ball, pad
        integer*8 i, j, k, blck, seg
        integer*8 mem(10000), cpy(10000)
        external input, output
        common /game/ grid, x, i, seg, ball, pad
        data i/0/, grid/10000*0/, blck/0/, seg/0/

        read(*,*, end=1) mem
 1      cpy = mem

c     Part one; count blocks
        call intcode(mem, input, output)
        do j = 1,100
           do k = 1,100
              if (grid(j, k).eq.2) blck = blck + 1
           end do
        end do
        write(*,*) blck

c     Part two; win
        cpy(1) = 2
        ball = 0
        pad = 0
        i = 0
        call intcode(cpy, input, output)
        write(*,*) seg
      stop
      end

      subroutine input(val, *)
        integer*8 val, diff
        integer*8 grid(100,100), x(2), i, seg, pad, ball
        common /game/ grid, x, i, seg, ball, pad

        diff = ball - pad
        if (diff.lt.0) then
           val = -1
        else if (diff.gt.0) then
           val = 1
        else
           val = 0
        end if
      end

      subroutine output(val, *)
        integer*8 grid(100,100), val, x(2), i, seg, pad, ball
        common /game/ grid, x, i, seg, ball, pad

        i = i+1
        if (i.eq.3) then
           if (x(1).eq.-1.and.x(2).eq.0) then
              seg = val
           else
              if (val.eq.3) then
                 pad = x(1)
              else if (val.eq.4) then
                 ball = x(1)
              end if
              grid(x(1)+1, x(2)+1) = val
           end if
           i = 0
        else
           x(i) = val
        end if
        return
      end
