c     Do the shuffling using as little memory as possible.
      program day22
        integer*8 p(2), modulo, inverse, modexp
        character*4 command
        integer*8 val, x(2), v(2), i, r, n, d
        data x/2*0/, v/2*1/

        p(1) = 10007
        p(2) = 1193157*10**8 + 17514047
        r = 1017415*10**8 + 82076661


 1      read(*,*,end=2) command, val
        if (command .eq. 'cut') then
           x(1) = modulo(x(1) + v(1) * val, p(1))
           x(2) = modulo(x(2) + v(2) * val, p(2))

        else if (command .eq. 'new') then
           v(1) = p(1) - v(1)
           v(2) = p(2) - v(2)

           x(1) = modulo(x(1) + v(1), p(1))
           x(2) = modulo(x(2) + v(2), p(2))
        else if (command .eq. 'incr') then
           v(1) = modulo(v(1) * inverse(val, p(1)), p(1))
           v(2) = modulo(v(2) * inverse(val, p(2)), p(2))
        end if
        go to 1


c     Part one - find the index of 2019
 2      i = 0
        do 5 while(x(1).ne.2019)
           i = i + 1
           x(1) = modulo(x(1) + v(1), p(1))
 5      continue
        write(*,*) i

c     Part two - find the 2020th value after a whole lotta iterations
c     x = geometric sum of x*v^r
        n = modexp(v(2), r, p(2))
        n = modulo(1 - n, p(2))
        d = modulo(1 - v(2), p(2))
        d = inverse(d, p(2))
        x(2) = modulo(x(2) * n, p(2))
        x(2) = modulo(x(2) * d, p(2))

c     v = v^r
        v(2) = modexp(v(2), r, p(2))

c     Get the 2020th card
        v(2) = modulo(2020 * v(2), p(2))
        x(2) = modulo(x(2) + v(2), p(2))
        write(*,*) x(2)
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
        integer*16 c, x
        c = mod(b, m)
        x = e
        modexp = 1
        do 1 while(x.gt.0)
           if (mod(x, 2).eq.1) then
              modexp = mod(modexp * c, m)
           end if
           c = mod(c * c, m)
           x = x / 2
 1      continue
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
