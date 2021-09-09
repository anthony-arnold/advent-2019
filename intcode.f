      subroutine intcode(n)
        integer n(*), pp, op, lhs, rhs, dst
        pp = 1

 1      op = n(pp)

        if (op.eq.99) return

c Intcode positions are 0-based
        lhs = n(n(pp + 1) + 1)
        rhs = n(n(pp + 2) + 1)
        dst = n(pp + 3) + 1

        if (op.eq.1) then
          n(dst) = lhs + rhs
        end if
        if (op.eq.2) then
          n(dst) = lhs * rhs
        end if

        pp = pp + 4
        go to 1
      end
