      program day17
        external day17_input, day17_output
        integer*8 mem(10 000), mem2(10 000), lineno, pos, i, j, algn
        character*80 lines(100)
        common /outside/ lines, lineno, pos
        data lineno/1/, pos/1/, algn/0/

        read(*,*,end=20) mem
 20     mem2 = mem

c Part One
        call intcode(mem, day17_input, day17_output)
        do 21 i = 2,lineno-1
            do 22 j = 2, 99
              if (lines(i)(j:j).eq.'#'.and.
     &              lines(i-1)(j:j).eq.'#' .and.
     &              lines(i+1)(j:j).eq.'#' .and.
     &              lines(i)(j-1:j+1).eq.'###') then
                    algn = algn + (i-1)*(j-1)
              else if(lines(i)(j:j).eq.char(10)) then
                go to 21
              end if
 22         continue
 21     continue
        write(*,*) algn

c Part Two
        mem(1) = 2
      stop
      end

      subroutine day17_input(val, *)
        integer*8 val
        write(*,*) 'Unexpected input'
        stop
      end

      subroutine day17_output(val, *)
        integer*8 val, lineno, pos
        character*80 lines(100)
        common /outside/ lines, lineno, pos

        lines(lineno)(pos:pos) = char(val)
        if (val.eq.10) then
            lineno = lineno + 1
            pos = 1
        else
            pos = pos + 1
        end if  
        return 
      end 
