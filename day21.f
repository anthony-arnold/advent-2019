      program day21
        integer*8 mem(10 000), mem2(10 000)
        character*8 code(16)
        integer*8 prog(125), pp
        external input, output
        common /springcode/ prog, pp
        data mem/10 000*0/

        read(*,*,end=1) mem
 1      mem2 = mem

c     Part one
        code(1) = 'NOT A J'
        code(2) = 'NOT B T'
        code(3) = 'OR T J'
        code(4) = 'NOT C T'
        code(5) = 'OR T J'
        code(6) = 'AND D J'
        code(7) = 'WALK'
        call compile(code, 11, prog)

        cp = 0
        pp = 1
        call intcode(mem, input, output)

c     Part two
        code(1) = 'NOT A J'
        code(2) = 'NOT B T'
        code(3) = 'OR T J'
        code(4) = 'NOT C T'
        code(5) = 'OR T J'
        code(6) = 'AND D J'
        code(7) = 'NOT E T'
        code(8) = 'NOT T T'
        code(9) = 'OR H T'
        code(10) = 'AND T J'
        code(11) = 'RUN'
        call compile(code, 11, prog)

        cp = 0
        pp = 1
        call intcode(mem2, input, output)

        stop
        end

      subroutine compile(code, n, prog)
         character*8 code(n), cmd
         integer*8 prog(125)
         integer i, j, k, p

        p = 0
        do 1 i = 1, n
           j = 8
           do 2 while(code(i)(j:j).eq.' ')
              j = j - 1
 2         continue
           do 3 k = 1,j
              p = p + 1
              prog(p) = ichar(code(i)(k:k))
 3         continue
           p = p + 1
           prog(p) = 10
 1      continue
      end

      subroutine input(val, *)
        integer*8 val
        integer*8 prog(125), pp
        common /springcode/ prog, pp
        val = prog(pp)
        pp = pp + 1
        return
      end

      subroutine output(val, *)
        integer*8 val

        if (val.gt.2**7) then
           write(*,*) val
        end if
        return
      end
