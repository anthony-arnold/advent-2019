      subroutine lcm(i, j, k)
        integer*8 i, j, k, a
        call gcd(i, j, a)
        if (a.ne.0) then
           k = i*j / a
        else
           k = a
        end if
        return
      end
