      program day22
        integer*8 p, phi, modulo, modexp
        character*4 command
        integer*8 val, x, v, i

        p = 10007
        phi = 10006

        x = 0
        v = 1

 1      read(*,*,end=2) command, val
        if (command .eq. 'cut') then
           x = modulo(x + v*val, p)
        else if (command .eq. 'new') then
           v = p - v
           x = modulo(x + v, p)
        else if (command .eq. 'incr') then
           v = modulo(v * modexp(val, phi-1, p), p)
        end if
        go to 1


 2      i = 0
        do 5 while(x.ne.2019)
           i = i + 1
           x = modulo(x + v, p)
 5      continue
        write(*,*) i
      end

      integer*8 function modulo(a, b)
        integer*8 a, b
        modulo = mod(a, b)
        if (modulo.lt.0) then
           modulo = modulo + b
        end if
        return
      end

      integer*8 function modexp(b, e, m)
        integer*8 b, e, m
        integer*8 c, eprime

        c = 1
        eprime = 0

 2      eprime = eprime + 1
        c = mod(b * c, m)
        if (eprime.lt.e) go to 2
        modexp = c
        return
      end
