      program day19
        integer*8 prog(10000), hits, i, j
        logical check
        data prog/10000*0/

        read(*,*,end=100) prog
100     continue        

c   Part one
        hits = 0
        do 102 i = 0,49
          do 101 j = 0,49
            if (check(prog, i, j)) then
                hits = hits + 1
            end if
101       continue
102     continue
        write(*,*) hits

c   Part two
        j = 99
        i = 99
103     if (check(prog, i, j)) then
            if (check(prog, i+99, j)) then
                if (check(prog, i, j+99)) then
                    write(*,*) i*10 000 + j
                    go to 199
                end if
                i = i + 1
            else
                j = j + 1
                i = j
            end if
            go to 103
        else
            i = i + 1
            go to 103
        end if
199     stop
      end

      logical function check(prog, x, y)
        integer*8 mem(10000), prog(10000), x, y, pos(2)
        external input, output
        logical hit
        common /dayx/ pos, hit
        mem = prog
        pos(1) = x
        pos(2) = y
        call intcode(mem, input, output)
        check = hit
        return
      end

      subroutine input(val, *)
        integer*8 val, x(2)
        logical hit
        common /dayx/ x, hit
        val = x(1)
        x(1) = x(2)
        return
      end
      subroutine output(val, *)
        integer*8 val, x(2)
        logical hit
        common /dayx/ x, hit
        hit = val.ne.0
        return
      end
