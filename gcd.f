      subroutine gcd(i, j, k)
        integer*8 i, j, a, b, c
        a = i
        b = j
        do while (b .ne. 0)
           c = b
           b = mod(a, b)
           a = c
        end do
        k = a
        return
      end
