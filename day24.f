      program day24
        character*5 layout(5, 2), ch
        character*5 layout2(5, 201, 2)
        integer i, j, c, h, n, k(2), adj, adj2, mem(1 000), nm, b
        integer encode
        nm = 0

        do 10 i = 1, 201
            do 11 j = 1,5
                layout2(j, i, 1) = '.....'
 11         continue
 10     continue

        b = 0
        do 1 i = 1,5
           read(*,*) layout(i, 1)
           layout2(i, 101, 1) = layout(i, 1)
           do 12 j = 1,5
              if(layout(i,1)(j:j).eq.'#') b = b + 1
 12        continue
 1      continue
        write(*,*) b

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

        k(1) = 1
        k(2) = 2
        do 7 n = 1,200
              do 88 h = 1,201
                 do 8 j = 1, 5
                    do 9 i = 1, 5
                       if (i.eq.3.and.j.eq.3) go to 9

                    ch = layout2(j, h, k(1))(i:i)
                    layout2(j, h, k(2))(i:i) = ch

                    c = adj2(layout2(1, 1, k(1)), i, j, h)
                    if (ch .eq. '#' .and. c .ne. 1) then
                       b = b - 1
                       layout2(j, h, k(2))(i:i) = '.'
                    else if (ch.ne.'#' .and. c.ge.1 .and. c.le.2) then
                       b = b + 1
                       layout2(j, h, k(2))(i:i) = '#'
                    end if
 9               continue
 8            continue
 88        continue

           i = k(1)
           k(1) = k(2)
           k(2) = i

 7      continue
        write(*,*) b

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

      integer function adji(layout, j, k)
        character*5 layout(5, 201)
        integer i, j, k

        adji = 0
        do 1 i  = 1,5
           if (layout(j, k)(i:i).eq.'#') then
              adji = adji + 1
           end if
 1      continue
        return
      end


      integer function adjj(layout, i, k)
        character*5 layout(5, 201)
        integer i, j, k

        adjj = 0
        do 1 j = 1,5
           if (layout(j, k)(i:i).eq.'#') then
              adjj = adjj + 1
           end if
 1      continue
        return
      end

      integer function adj2(layout, i, j, k)
        character*5 layout(5, 201)
        integer i, j, k, adji, adjj

        adj2 = 0
        if (i.gt.1 .and. layout(j, k)(i-1:i-1).eq.'#') then
           adj2 = adj2 + 1
        end if

        if (i.lt.5 .and. layout(j, k)(i+1:i+1).eq.'#') then
           adj2 = adj2 + 1
        end if

        if (j.gt.1 .and. layout(j-1, k)(i:i).eq.'#') then
           adj2 = adj2 + 1
        end if
        if (j.lt.5 .and. layout(j+1, k)(i:i).eq.'#') then
           adj2 = adj2 + 1
        end if

        if (k.eq.1) go to 1

        if (i.eq.1 .and. layout(3, k-1)(2:2).eq.'#') then
           adj2 = adj2 + 1
        end if
        if (i.eq.5 .and. layout(3, k-1)(4:4).eq.'#') then
           adj2 = adj2 + 1
        end if
        if (j.eq.1 .and. layout(2, k-1)(3:3).eq.'#') then
           adj2 = adj2 + 1
        end if
        if (j.eq.5 .and. layout(4, k-1)(3:3).eq.'#') then
           adj2 = adj2 + 1
        end if

 1      if (k.eq.201) go to 99
        if (j.eq.3) then
           if (i.eq.2) then
              adj2 = adj2 + adjj(layout, 1, k+1)
           else if (i.eq.4) then
              adj2 = adj2 + adjj(layout, 5, k+1)
           end if
        else if (i.eq.3) then
           if (j.eq.2) then
              adj2 = adj2 + adji(layout, 1, k+1)
           else if (j.eq.4) then
              adj2 = adj2 + adji(layout, 5, k+1)
           end if
        end if

 99     return
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
