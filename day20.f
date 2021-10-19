


      function ischar(c)
        character c
        integer i
        logical ischar
        i = ichar(c)
        ischar = i.ge.ichar('A') .and. i.le.ichar('Z')
        return
      end

      function enc(x, y)
        integer enc, x, y, i
        enc = y * 200 + x
        return
      end

      subroutine cmp_dst(a, b, r)
        integer a, b, alvl, blvl, ad, bd
        integer distance(1000, 40 000)
        logical r
        common /path/ distance

        alvl = mod(a, 1 000)
        blvl = mod(b, 1 000)

        ad = distance(alvl, a / 1000)
        bd = distance(blvl, b / 1000)

        r = ad.lt.bd
        return
      end

      subroutine dijkstra(cnct, pos, n, src, dst, kcase)
        integer cnct(5, 40 000), n, pos(2, n), src(2), dst(2)
        integer A(40 000), b, d, i, j, k, s, ir
        logical V(1000, 40 000), Q(1000, 40 000)
        integer enc, lvl, kcase, xmax(2), klvl
        external cmp_dst
        integer distance(1000, 40 000), portal(40 000), alt
        common /path/ distance
        b = 0

        do 50 i = 1,n
            do 51 j = 1,1000
               distance(j, pos(1, i)) = 1 000 000
               V(j, pos(1, i)) = .false.
               Q(j, pos(1, i)) = .false.
 51         continue
            portal(pos(1, i)) = pos(2, i)
50      continue
        s = enc(src(1), src(2))
        d = enc(dst(1), dst(2)) * 1000 + 1

        distance(1, s) = 0
        call heap_push(A, b, s*1000+1, cmp_dst)

        do 56 while(b.gt.0)
            call heap_pop(A, b, i, cmp_dst)
            if (d .eq. i) go to 60

            ir = i / 1000
            lvl = mod(i, 1 000)

            if (V(lvl, ir)) go to 56
            V(lvl, ir) = .true.

            do 57 j = 1, cnct(1, ir)
                k = cnct(j+1, ir)

c     Decide next level
                klvl = lvl
                if (kcase .eq. 2) then
                   if (portal(ir).eq.1 .and. portal(k).eq.2) then
c     From outer to inner portal
                      klvl = lvl - 1
                   else if (portal(ir).eq.2. .and. portal(k).eq.1) then
c     From inner to outer potal
                      klvl = lvl + 1
                   end if

c     Outer portals not valid on first level
                   if (klvl.eq.0) go to 57
                end if

                alt = distance(lvl, ir) + 1
                if (distance(klvl, k).gt.alt) then
                   distance(klvl,k) = alt

                   if (.not.Q(klvl, k)) then
                      call heap_push(A, b, k*1000+klvl, cmp_dst)
                   else
c     in place of "decrease priority"
                      call reheap(A, b, cmp_dst)
                   end if
                   Q(klvl, k) = .true.
                end if
57          continue
56      continue
60      write(*,*) distance(1, d/1000)
        return
      end

      subroutine reheap(A, n, c)
        integer A(n), B(n), n, m, i
        external c
        m = 0
        do 1 i = 1, n
           call heap_push(B, m, A(i), c)
 1      continue
        A = B
        return
      end

      program day20
        integer cnct(5, 40 000), w, x, y, z, n, i, j, k
        integer prtls(2, 100), nprtls, enc, xmax(2)
        integer src(2), dst(2), pos(2, 40 000), npos
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
                        go to 14
                    else if (c.eq.'Z' .and. d.eq.'Z') then
                        dst(1) = i
                        dst(2) = j
                        go to 14
                    end if
                    xmax(1) = max(xmax(1), i)
                    xmax(2) = max(xmax(2), j)

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
                    pos(1, npos) = enc(x, y)
                    pos(2, npos) = 0

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
                            if (x.eq.3 .or. y.eq.3 .or.
     &                          x.eq.xmax(1) .or. y.eq.xmax(2)) then
c     Outer portal
                              pos(2, npos) = 1
                            else
c     Inner portal
                              pos(2, npos) = 2
                            end if

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

        call dijkstra(cnct, pos, npos, src, dst, 1)
        call dijkstra(cnct, pos, npos, src, dst, 2)
        stop
      end
