      program day24
        character*5 layout(5, 2), ch
        integer i, j, c, k(2), adj, mem(1 000), nm
        integer encode
        nm = 0

        do 1 i = 1,5
           read(*,*) layout(i, 1)
 1      continue

        k(1) = 1
        k(2) = 2
 2      do 4 i = 1, 5
           do 3 j = 1, 5
              ch = layout(j, k(1))(i:i)
              layout(j, k(2))(i:i) = ch

              c = adj(layout(1, k(1)), i, j)
              if (ch .eq. '#' .and. c .ne. 1) then
                 layout(j, k(2))(i:i) = '.'
              else if (ch .ne. '#' .and. c .ge. 1 .and. c .le. 2) then
                 layout(j, k(2))(i:i) = '#'
              end if
 3         continue
 4      continue
        c = encode(layout(1, k(2)))
        do 5 i = 1, nm
           if (mem(i).eq.c) go to 6
 5      continue
        nm = nm + 1
        mem(nm) = c

        i = k(1)
        k(1) = k(2)
        k(2) = i
        go to 2

 6      write(*,*) c

        stop
      end

      integer function encode(layout)
        character*5 layout(5)
        integer i, j
        encode = 0

        do 2 j = 1, 5
           do 1 i = 1, 5
              if (layout(j)(i:i).eq.'#') then
                 encode = encode + int(2**(5*(j-1)+(i-1)))
              end if
 1         continue
 2      continue
        return
      end

      integer function adj(layout, i, j)
        character*5 layout(5)
        integer i, j, x, y

        adj = 0
        if (i.gt.1 .and. layout(j)(i-1:i-1).eq.'#') then
           adj = adj + 1
        end if
        if (i.lt.5 .and. layout(j)(i+1:i+1).eq.'#') then
           adj = adj + 1
        end if
        if (j.gt.1 .and. layout(j-1)(i:i).eq.'#') then
           adj = adj + 1
        end if
        if (j.lt.5 .and. layout(j+1)(i:i).eq.'#') then
           adj = adj + 1
        end if

        return
      end
