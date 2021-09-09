      subroutine intcode(mem)
        integer mem(*), ip, op, lhs, rhs, dst
        ip = 1

 1      op = mem(ip)

        if (op.eq.99) return

c Intcode positions are 0-based
        lhs = mem(mem(ip + 1) + 1)
        rhs = mem(mem(ip + 2) + 1)
        dst = mem(ip + 3) + 1

        if (op.eq.1) then
          mem(dst) = lhs + rhs
        end if
        if (op.eq.2) then
          mem(dst) = lhs * rhs
        end if

        ip = ip + 4
        go to 1
      end
