c     Do the shuffling using as little memory as possible.
      program day22
        integer*8 p(2), modulo, inverse, mult
        character*4 command
        integer*8 val, x(2), v(2), i, j, r, n, d
        data x/2*0/, v/2*1/

        p(1) = 10007
        p(2) = 1193157*10**8 + 17514047
        r = 1017415*10**8 + 82076661

 1      read(*,*,end=2) command, val
        do 3 j = 1,2
        if (command .eq. 'cut') then
           x(j) = modulo(x(j) + v(j) * val, p(j))

        else if (command .eq. 'new') then
           v(j) = p(j) - v(j)
           x(j) = modulo(x(j) + v(j), p(j))
        else if (command .eq. 'incr') then
           v(j) = mult(v(j), inverse(val, p(j)), p(j))
        end if
 3      continue
        go to 1

c     Part one - find the index of 2019
 2      v(1) = inverse(v(1), p(1))
        x(1) = mult(-x(1), v(1), p(1))
        n = modulo(2019 * v(1) + x(1), p(1))
        write(*,*) n

c     Part two - find the 2020th value after many iterations
        n = 2020
        call geom(x(2), v(2), r, p(2))
        d = mult(v(2), n, p(2))
        d = modulo(x(2) + d, p(2))
        write(*,*) d
      end

      subroutine geom(x, v, r, p)
        integer*8 x, v, r, p, a, b, mult, inverse, modexp

        a = modexp(v, r, p)
        b = modulo(a-1, p)
        b = mult(b, x, p)
        b = mult(b, inverse(v-1, p), p)

        x = b
        v = a
        return
      end

c     a*b mod p by repeated addition
      integer*8 function mult(a, b, p)
        integer*8 a, b, p, x, y, m
        m = 0
        x = a
        y = b
        do 1 while(y.ne.0)
           if (mod(y, 2).eq.1) then
              m = mod(m + x, p)
           end if
           y = y / 2
           if (x.lt.p-x) then
              x = x * 2
           else
              x = x - (p - x)
           end if
 1      continue
        mult = m
        return
      end

c     A version of mod that handles negatives correctly
      integer*8 function modulo(a, b)
        integer*8 a, b
        modulo = mod(a, b)
        if (modulo.lt.0) then
           modulo = modulo + b
        end if
        return
      end

c     b^e mod m by repeated squaring
      integer*8 function modexp(b, e, m)
        integer*8 b, e, m
        integer*8 c, x, mult, g
        c = mod(b, m)
        x = e
        g = 1
        do 1 while(x.gt.0)
           if (mod(x, 2).eq.1) then
              g = mult(g, c, m)
           end if
           c = mult(c, c, m)
           x = x / 2
 1      continue
        modexp = g
        return
      end

c     Get the modular inverse (1 / a mod m)
      integer*8 function inverse(a, m)
        integer*8 a, m, q, p
        integer*8 t0, t1, r0, r1

        t0 = 0
        t1 = 1
        r0 = m
        r1 = a

        do 1 while (r1 .ne. 0)
           q = r0 / r1

           p = t1
           t1 = t0 - q * t1
           t0 = p

           p = r1
           r1 = r0 - q * r1
           r0 = p
 1      continue

        if (t0 .lt. 0) then
           inverse = t0 + m
        else
           inverse = t0
        end if
        return
      end
