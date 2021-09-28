      program day17
        external day17_input, day17_output
        integer*8 mem(10 000), mem2(10 000), lineno, pos, i, j, k, algn
        character*80 lines(100)
        integer*8 bot(2), instr(256), ip, ori, rot
        logical walk
        character*1 c
        common /outside/ lines, lineno, pos, bot
        common /prog/ instr, ip
        data lineno/1/, pos/1/, algn/0/, ip/1/, instr/256 * 0/

        read(*,*,end=20) mem
 20     mem2 = mem
        mem2(1) = 2

c Part One
        call intcode(mem, day17_input, day17_output)
        do 21 i = 2,lineno-1
            do 22 j = 2, 99
               if (lines(i)(j:j).eq.'#') then
                  if(lines(i-1)(j:j).eq.'#' .and.
     &                 lines(i+1)(j:j).eq.'#' .and.
     &                 lines(i)(j-1:j+1).eq.'###') then
                     algn = algn + (i-1)*(j-1)
                  end if
              else if(lines(i)(j:j).eq.char(10)) then
                go to 21
              end if
 22         continue
 21     continue
        write(*,*) algn

c     Part Two
        select case (lines(bot(2))(bot(1):bot(1)))
        case ('^')
           ori = 0
        case ('>')
           ori = 1
        case ('v')
           ori = 2
        case ('<')
           ori = 3
        end select

 30     call find_next(ori, walk, rot, *31)
        if (walk) then
           instr(ip) = instr(ip) + 1
           call move(ori)
        else
           ori = mod(ori + rot, 4)
           if (ip.ne.1) ip = ip + 1
           instr(ip) = rot
           ip = ip + 1
        end if
        go to 30

 31     call routine(ip, instr)
        lineno = 1
        ip = 1
        call intcode(mem2, day17_input, day17_output)
      stop
      end

      subroutine day17_input(val, *)
        integer*8 val, instr(256), ip
        common /prog/ instr, ip
        val = instr(ip)
        ip = ip + 1
        return
      end

      subroutine day17_output(val, *)
        integer*8 val, lineno, pos, bot(2)
        character*80 lines(100)
        common /outside/ lines, lineno, pos, bot
        if (val.gt.255) then
           write(*,*) val
           return
        end if

        lines(lineno)(pos:pos) = char(val)
        if (val.eq.10) then
            lineno = lineno + 1
            pos = 1
         else
            if (val.ne.35 .and. val.ne.46) then
               bot(1) = pos
               bot(2) = lineno
            end if
            pos = pos + 1
         end if
        return
      end


      subroutine routine(ip, instr)
        integer*8 ip, instr(ip), c, d, h, i, j, k, l
        integer*8 main(100), fn(100, 4)
        logical same
        data main/100*0/, fn/400*0/

c     Encode main
        do 100 i = 1,ip,2
           if (instr(i).eq.1) then
              main(i/2+1) = instr(i) - instr(i+1)
           else
              main(i/2+1) = instr(i) + instr(i+1)
           end if
 100    continue

c     Find repeated sequences
        d = ip/2
        do 110 c = 1,3
           k = 4
 108       do 109 i = 1, d-k*2
              if (same(main(i), main(d-k+1), k)) then
                 do 107 j = 1,c-1
                    if (fn(1,j).eq.k.and.same(main(i),fn(2,j),k)) then
                       d = d - k
                       k = 4
                       go to 108
                    end if
 107             continue

                 fn(1,c) = k
 111             fn(2:k+1,c) = main(d-k+1:d)
                 d = d - k
                 go to 110
              end if
 109       continue
           k = k - 1
           go to 108
 110    continue

c     Rewrite main to call subroutines
        i = 1
        j = 1
        l = 1
 199    do 200 c = 1,3
           k = fn(1,c)
           if (same(fn(2,c), main(i), k)) then
              instr(j) = ichar('A') + c - 1
              instr(j+1) = 44
              j = j + 2
              i = i + k
              if (i.lt.ip/2) go to 199
           end if
 200    continue
        instr(j-1) = 10

c     Decode subroutines
        do 300 c = 1,3
           k = fn(1, c)
           do 299 i = 1,k
              l = fn(i+1, c)
              if (l.lt.1) then
                 h = ichar('R')
                 l = -l + 1
              else
                 h = ichar('L')
                 l = l - 3
              end if
              instr(j) = h
              instr(j+1) = 44
              j = j + 2
              if (l.gt.9) then
                 instr(j) = ichar('0') + l/10
                 l = mod(l, 10)
                 j = j + 1
              end if
              instr(j) = ichar('0') + l
              instr(j+1) = 44
              j = j + 2
 299       continue
           instr(j-1) = 10
 300    continue
        instr(j) = ichar('n')
        instr(j+1) = 10
        ip = j + 1
        return
      end

c     Sequence equality
      function same(seq1, seq2, k)
        integer*8 i, k, seq1(k), seq2(k)
        logical same
        do 60 i = 1,k
           if (seq1(i).ne.seq2(i)) then
              same = .false.
              return
           end if
 60     continue
        same = .true.
        return
      end

c     Walk the bot
      subroutine move(ori)
        integer*8 lineno, pos, bot(2), ori
        character*80 lines(100)
        character*1 dir
        common /outside/ lines, lineno, pos, bot

        select case (ori)
        case (0)
           bot(2) = bot(2) - 1
        case (1)
           bot(1) = bot(1) + 1
        case (2)
           bot(2) = bot(2) + 1
        case (3)
           bot(1) = bot(1) - 1
        end select
      end

c     Determine next move
      subroutine find_next(ori, walk, rot, *)
        integer*8 lineno, pos, bot(2), dir, ori, rot
        logical walk
        character*80 lines(100)
        character*1 next
        common /outside/ lines, lineno, pos, bot

        walk = .false.
        dir = ori
        if (next(dir).eq.'#') then
           walk = .true.
           return
        end if

c     Try turning right
        dir = mod(dir + 1, 4)
        if (next(dir).ne.'#') then
c     Try turning left
           dir = mod(dir + 2, 4)
           if (next(dir).ne.'#') then
c     Finished
              return 1
           end if
           rot = 3
        else
           rot = 1
        end if
      end

c     What is the bot looking at?
      function next(dir)
        integer*8 lineno, pos, bot(2), dir, i, j
        character*80 lines(100)
        character*1 next
        common /outside/ lines, lineno, pos, bot

        select case(dir)
        case(0)
           i = bot(2) - 1
           j = bot(1)
           if (i.lt.1) then
              next = '.'
              return
           end if
        case (1)
           i = bot(2)
           j = bot(1)+1
        case (2)
           i = bot(2)+1
           j = bot(1)
        case (3)
           i = bot(2)
           j = bot(1)-1
        end select

        if (i.lt.1 .or. j.lt.1) then
           next = '.'
        else
           next = lines(i)(j:j)
        end if
        return
      end
