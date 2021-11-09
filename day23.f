      program day23
        integer*8 NATSIZE, SIZE
        parameter(NATSIZE=2, SIZE=2 000)
        integer*8 mem(10 000, 50), ip(2, 50), i
        integer*8 queue(SIZE, 50), qp(2, 50), no(50), dst(50)
        integer*8 nat(4)
        logical idle

        integer*8 addr
        external input_addr, output, input
        common /netaddr/ addr
        common /io/ queue, qp, no, dst, nat, idle

        nat(1) = 0
        nat(2) = 0

        read(*,*,end=1) mem(1:10 000, 1)
 1      continue

        do 2 i = 1,50
           ip(1, i) = 1
           ip(2, i) = 2
           qp(1, i) = 0
           qp(2, i) = 0
           no(i) = 0
           dst(i) = -1
           if (i.gt.1) then
              mem(1:10 000, i) = mem(1:10 000, 1)
           end if
 2      continue

        do 3 i = 1,50
           addr = i
           call intcode_yield(mem(1,i), input_addr, output, ip(1,i), *3)
 3      continue

 5      idle = .true.
        do 4 i = 1,50
           addr = i
           call intcode_yield(mem(1,i), input, output, ip(1,i), *4)
 4      continue
        if (idle) then
           call ring_pop(nat(3), i, NATSIZE, nat)
           call ring_push(queue(1, 1), i, SIZE, qp(1, 1))

           call ring_pop(nat(3), i, NATSIZE, nat)
           call ring_push(queue(1, 1), i, SIZE, qp(1, 1))
           call check(i)
        end if
        go to 5
      end

      subroutine check(val)
        integer*8 val
        integer*8 table(1 000), n, i
        data n/0/
        save table, n

        do 1 i = 1,n
           if (table(i).eq.val) then
              write(*,*) val
              stop
           end if
 1      continue

        n = n + 1
        table(n) = val

        return
      end

      subroutine input_addr(val, *)
        integer*8 val, addr
        common /netaddr/ addr
        val = addr - 1
        return 1
      end

      subroutine input(val, *)
        integer*8 val, SIZE, addr
        parameter(SIZE=2000)
        integer*8 queue(SIZE, 50), qp(2, 50), no(50), dst(50)
        integer*8 nat(4)
        logical idle
        common /netaddr/ addr
        common /io/ queue, qp, no, dst, nat, idle

        if (qp(1, addr)-qp(2, addr).eq.0) then
           val = -1
           return 1
        end if

        idle = .false.
        call ring_pop(queue(1, addr), val, SIZE, qp(1, addr))
        return
      end

      subroutine output(val, *)
        integer*8 val, SIZE, NATSIZE, addr, daddr
        parameter(SIZE=2000, NATSIZE=2)
        integer*8 queue(SIZE, 50), qp(2, 50), no(50), dst(50)
        integer*8 nat(4)
        integer tonat
        data tonat/0/
        save tonat
        logical idle
        common /netaddr/ addr
        common /io/ queue, qp, no, dst, nat, idle

        if (dst(addr).lt.0) then
           dst(addr) = val
           no(addr) = 0
           return
        else
           daddr = dst(addr)+1
           if (daddr.le.50) then
              call ring_push(queue(1, daddr), val, SIZE, qp(1, daddr))
           else if(daddr.eq.256) then
              call ring_push(nat(3), val, NATSIZE, nat)
           end if
           no(addr) = no(addr) + 1

           if (no(addr).eq.2) then
              if (daddr.eq.256) then
                 tonat = tonat + 1
                 if (tonat.eq.1) then
                    write(*,*) val
                 end if
              end if

              dst(addr) = -1
              return 1
           end if
        end if
        return
      end

      subroutine ring_push(buffer, i, n, qp)
        integer*8 n, buffer(n), i, qp(2)

        qp(2) = mod(qp(2) + 1, n)
        buffer(qp(2)+1) = i
        return
      end

      subroutine ring_pop(buffer, i, n, qp)
        integer*8 n, buffer(n), i, qp(2)

        qp(1) = mod(qp(1) + 1, n)
        i = buffer(qp(1)+1)
        return
      end
