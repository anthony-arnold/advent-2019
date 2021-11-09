      program day23
        integer*8 mem(10 000, 50), ip(2, 50), i
        integer*8 queue(2 000, 50), qp(2, 50), no(50), dst(50)
        logical on(50), active

        integer*8 addr
        external input_addr, output, input
        common /netaddr/ addr
        common /io/ queue, qp, no, dst

        addr = 0

        read(*,*,end=1) mem(1:10 000, 1)
 1      continue

        do 2 i = 1,50
           ip(1, i) = 1
           ip(2, i) = 2
           qp(1, i) = 0
           qp(2, i) = 0
           no(i) = 0
           dst(i) = -1
           on(i) = .false.
           if (i.gt.1) then
              mem(1:10 000, i) = mem(1:10 000, 1)
           end if
 2      continue

        do 3 i = 1,50
           addr = i
           on(i) = .true.
           call intcode_yield(mem(1,i), input_addr, output, ip(1,i), *3)
 3      continue

 5      active = .false.
        do 4 i = 1,50
           if (on(i)) then
              active = .true.
              addr = i
              call intcode_yield(mem(1,i), input, output, ip(1,i), *4)
              on(i) = .false.
           end if
 4      continue
        if (active) go to 5

        stop
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
        common /netaddr/ addr
        common /io/ queue, qp, no, dst

        if (qp(1, addr)-qp(2, addr).eq.0) then
           val = -1
           return 1
        end if

        call ring_pop(queue(1, addr), val, SIZE, qp(1, addr))
        return
      end

      subroutine output(val, *)
        integer*8 val, SIZE, addr, daddr
        parameter(SIZE=2000)
        integer*8 queue(SIZE, 50), qp(2, 50), no(50), dst(50)
        common /netaddr/ addr
        common /io/ queue, qp, no, dst

        if (dst(addr).lt.0) then
           dst(addr) = val
           no(addr) = 0
           return
        else
           daddr = dst(addr)+1
           if (daddr.le.50) then
              call ring_push(queue(1, daddr), val, SIZE, qp(1, daddr))
           end if
           no(addr) = no(addr) + 1

           if (no(addr).eq.2) then
              if (daddr.eq.256) then
                 write(*,*) val
                 stop
              end if

              dst(addr) = -1
              return 1
           end if
        end if
        return
      end
