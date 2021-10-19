      program day22
        integer p, modulo
        parameter(p=10007)

        character*4 command
        integer val, ptr, dir, i, j
        integer deck(p), swap(p)
        do 3 ptr = 1, p
           deck(ptr) = ptr - 1
 3      continue

        ptr = 0
        dir = 1

 1      read(*,*,end=2) command, val
        if (command .eq. 'cut') then
           ptr = modulo(ptr + dir * val, p)
        else if (command .eq. 'new') then
           dir = dir * (-1)
           ptr = modulo(ptr + dir, p)
        else if (command .eq. 'incr') then
           j = 0
           do 4 i = 1,p
              swap(j+1) = deck(ptr+1)
              j = modulo(j + val, p)
              ptr = modulo(ptr + dir, p)
 4         continue
           deck = swap
           ptr = 0
           dir = 1
        end if
        go to 1

 2      i = 0
        do 5 while(deck(ptr + 1).ne.2019)
           i = i + 1
           ptr = modulo(ptr + dir, p)
 5      continue
        write(*,*) i
      end

      integer function modulo(a, b)
        integer a, b
        modulo = mod(a, b)
        if (modulo.lt.0) then
           modulo = modulo + b
        end if
        return
      end
