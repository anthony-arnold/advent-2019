
      

      function ischar(c) 
        character c
        integer i
        logical ischar
        i = ichar(c)
        ischar = i.ge.ichar('A') .and. i.le.ichar('Z')
        return
      end

      function enc(x, y) 
        integer enc, x, y
        enc = y * 200 + x
        return
      end

      subroutine cmp_dst(a, b, r)
        integer a, b
        integer distance(40 000)
        logical r
        common /path/ distance

        r = distance(a).le.distance(b)
        return
      end

      subroutine dijkstra(cnct, pos, n, src, dst)
        integer cnct(5, 40 000), n, pos(n), src(2), dst(2)
        integer A(40 000), b, d, i, j, k, s
        logical V(40 000)
        integer enc
        external cmp_dst
        integer distance(40 000)
        common /path/ distance
        b = 0

        do 50 i = 1,n
            distance(pos(i)) = 1 000 000
            V(pos(i)) = .false.
50      continue
        s = enc(src(1), src(2))
        d = enc(dst(1), dst(2))

        distance(s) = 0
        call heap_push(A, b, s, cmp_dst)

        do 56 while(b.gt.0) 
            call heap_pop(A, b, i, cmp_dst)
            if (d .eq. i) go to 60

            V(i) = .true.
            do 57 j = 1, cnct(1, i)
                k = cnct(j+1, i)
                distance(k) = min(distance(k), distance(i) + 1)
                if (.not.V(k)) then
                    call heap_push(A, b, k, cmp_dst)
                end if
57          continue
56      continue
60      write(*,*) distance(d)
        return
      end

      program day20
        integer cnct(5, 40 000), w, x, y, z, n, i, j, k
        integer prtls(2, 100), nprtls, enc
        integer src(2), dst(2), pos(40 000), npos
        character*200 lines(200)
        character c, d
        logical ischar
        data cnct/200 000*0/

        n = 0
        nprtls = 0
11       read(*,'(A)',end=12) lines(n+1)
        n = n + 1
        go to 11
12       continue
c   Find all portals
        do 13 y = 1, n
            do 14 x = 1, 199
                c = lines(y)(x:x)
                if (ischar(c)) then
                    if (ischar(lines(y)(x+1:x+1))) then
                        d = lines(y)(x+1:x+1)

                        i = x + 2
                        j = y
                        if (lines(j)(i:i).eq.'.') go to 10

                        i = x - 1
                        j = y
                        if (lines(j)(i:i).eq.'.') go to 10

                    else if (ischar(lines(y+1)(x:x))) then
                        d = lines(y+1)(x:x)

                        i = x
                        j = y + 2
                        if (lines(j)(i:i).eq.'.') go to 10

                        i = x
                        j = y - 1
                        if (lines(j)(i:i).eq.'.') go to 10
                    end if
                    go to 14
10                  if (c.gt.d) then
                        c = d
                        d = lines(y)(x:x)
                    end if
                    
                    if (c.eq.'A' .and. d.eq.'A') then
                        src(1) = i
                        src(2) = j
                    else if (c.eq.'Z' .and. d.eq.'Z') then
                        dst(1) = i
                        dst(2) = j
                    end if

                    nprtls = nprtls + 1
                    prtls(1, nprtls) = enc(i, j)
                    prtls(2, nprtls) = enc(ichar(c), ichar(d))
                end if
14           continue
13       continue

c   Find all edges
        npos = 0
        do 6 y = 1, n
            do 5 x = 1, 200
                c = lines(y)(x:x)
                if (c.eq.'.') then
                    npos = npos + 1
                    pos(npos) = enc(x, y)

                    k = 0
                    if (lines(y-1)(x:x).eq.'.') then
                        k = k + 1
                        cnct(k+1, enc(x, y)) = enc(x, y-1)
                    end if
                    if (lines(y+1)(x:x).eq.'.') then
                        k = k + 1
                        cnct(k+1, enc(x, y)) = enc(x, y+1)
                    end if
                    if (lines(y)(x-1:x-1).eq.'.') then
                        k = k + 1
                        cnct(k+1, enc(x, y)) = enc(x-1, y)
                    end if
                    if (lines(y)(x+1:x+1).eq.'.') then
                        k = k + 1
                        cnct(k+1, enc(x, y)) = enc(x+1, y)
                    end if

                    do 50 z = 1, nprtls
                        if (prtls(1, z).eq.enc(x, y)) then
                            
                            do 51 w = 1, nprtls
                                if (z.ne.w .and.
     &                              prtls(2,z).eq.prtls(2,w)) then
                                    k = k + 1
                                    cnct(k+1, enc(x, y)) = prtls(1, w)
                                end if
51                          continue
                        end if
50                  continue
                    cnct(1, enc(x, y)) = k
                end if
5           continue
6       continue

        call dijkstra(cnct, pos, npos, src, dst)
        stop
      end
