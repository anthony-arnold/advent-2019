      program day25
        integer*8 mem(10 000)
        character*32 puzzle
        external input, output

        if (iargc().ne.1) then
           write(*,*) 'Usage: day25 <puzzle-input>'
           stop
        end if

        call getarg(1, puzzle)
        open(UNIT=8, FILE=puzzle, ACCESS='SEQUENTIAL', FORM='FORMATTED')
        read(8,*,end=1) mem

 1      continue

        call intcode(mem, input, output)

        stop
      end

      subroutine input(val, *)
        integer i
        integer*8 val
        integer*8 command(80), cp
        character*80 line
        data cp/0/
        save command, cp

        if (cp.eq.0) then
           read(*,'(A)') line
           do 2 i = 1,80
              if (i.gt.1.and.command(i-1).eq.10) then
                 if (line(i:i).eq.char(32)) go to 3
                 command(i-1) = 32
              end if
              if (line(i:i) .eq. char(32)) then
                 command(i) = 10
              else
                 command(i) = ichar(line(i:i))
              end if
 2         continue
 3         cp = 1
        end if

        val = command(cp)
        cp = cp + 1
        if (val.eq.10) then
           cp = 0
        end if
        return
      end

      subroutine output(val, *)
        integer*8 val, n
        character*200 line
        data line/''/, n/0/
        save line, n

        if (val.eq.10) then
           if (n.gt.0) then
              write(*,*) line(1:n)
              line = ''
              n = 0
           end if
        else
           n = n + 1
           line(n:n) = char(val)
        end if
      end
