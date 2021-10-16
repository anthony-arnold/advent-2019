      subroutine cmp(a, b, r)
        integer a, b
        logical r
        r = a.le.b
        return
      end

      program day18
        parameter (INF=1 000 000)
        integer posn(2, 30)
        integer distances(26, 30), doors(26, 26, 30)
        integer nlines, i, j, k, nbots, nkeys, pathlen
        character*100 lines(100)
        character c
        data distances/780*INF/, doors/20280*0/

        nbots = 0
        nkeys = 0
        nlines = 0
 10     read(*,*, end=20) lines(nlines + 1)
        nlines = nlines + 1
        do 11 i=1,100
           c = lines(nlines)(i:i)
           if (c.eq.'@') then
              nbots = nbots + 1
              posn(1, nbots + 26) = nlines
              posn(2, nbots + 26) = i
           else
              k = ichar(c) - ichar('a') + 1
              if (k.ge.1 .and. k.le.26) then
                 posn(1, k) = nlines
                 posn(2, k) = i
                 nkeys = max(nkeys, k)
              end if
           end if
 11     continue
        go to 10
 20     continue

        do 30 i = 1,nkeys
           call fill(posn(1,i), distances(1,i), doors(1,1,i), lines)
 30     continue

        do 31 i = 27,nbots+26
           call fill(posn(1,i), distances(1,i), doors(1,1,i), lines)
 31     continue

        write(*,*) pathlen(distances, doors, nbots, nkeys)
      end

      subroutine cmp_pathlen(a, b, r)
        parameter (NSTATE=100 000)
        integer a, b, k1, k2, i
        logical r
        integer state(31, NSTATE), stack(NSTATE), sp
        common /pathlen_state/ state, stack, sp
        if (state(5,a).eq.state(5,b)) then
           k1 = 0
           do 1 i = 6,31
              k1 = k1 + state(i, a)
 1         continue
           k2 = 0
           do 2 i = 6,31
              k2 = k2 + state(i, b)
              if (k2.gt.k1) then
                 r = .true.
                 return
              end if
 2         continue
           r = k1.le.k2
        else
           r = state(5, a) .lt. state(5, b)
        end if
        return
      end

      logical function reachable(doors, keys)
         integer doors(26), keys(26), j
         j = 1
         do 200 while(doors(j).ne.0)
            if (keys(doors(j)).eq.0) then
               reachable = .false.
               return
            end if
            j = j + 1
 200     continue
         reachable = .true.
         return
      end

      function pathlen(distances, doors, nbots, nkeys)
        parameter (NSTATE=100 000, INF=1 000 000)
        integer NSTATE, INF
        integer pathlen
        integer distances(26, 30), doors(26, 26, 30)
        integer nbots, nkeys
        integer i, j, k, z, dist, x, si, s, s2, s3
        external cmp_pathlen, cmpstate
        integer A(30*NSTATE), n
        logical reachable

        integer state(31, NSTATE), stack(NSTATE), sp
        common /pathlen_state/ state, stack, sp
        data state/3 100 000*0/

        n = 0
        sp = 0
        do 100 i = NSTATE,2,-1
           sp = sp + 1
           stack(sp) = i
 100    continue

        s = 1
        do 101 i = 1,nbots
           state(i, s) = i + 26
 101    continue
        state(5, s) = 0
        call heap_push(A, n, s, cmp_pathlen)

        do 80 while (n .ne. 0)
           call heap_pop(A, n, s, cmp_pathlen)

           x = 0
           do 66 j = 6, 31
              x = x + state(j, s)
 66        continue
           if (x.ge.nkeys) then
              pathlen = state(5, s)
              go to 81
           end if

           do 1010 si = 1, nbots
              z = state(si, s)

c     Find all keys that can be reached (no locked doors) from here.
              do 72 i = 1,nkeys
                 if (.not.reachable(doors(1,i,z), state(6, s))) go to 72
                 if (distances(i, z).eq.INF) go to 72
                 if (state(i+5, s).ne.0) go to 72

c     Alternate distance
                 dist = state(5, s) + distances(i, z)

c     New state with previous keys set
                 s2 = stack(sp)
                 sp = sp - 1

                 do 69 j = 1, 31
                    state(j, s2) = state(j, s)
 69              continue
                 state(si, s2) = i
                 state(5, s2) = dist
                 state(i+5, s2) = 1

c     Find equivalent state already on the queue (ugh)

                 do 157 j = 1,n
                    s3 = A(j)
                    do 152 k = 1,nbots
                       if (state(k,s2).ne.state(k,s3)) then
                          go to 157
                       end if
 152                continue
                    do 156 k = 6,nkeys+5
                       if (state(k,s2).ne.state(k,s3)) then
                          go to 157
                       end if
 156                continue

                    sp = sp + 1
                    stack(sp) = s2

                    if (dist.lt.state(5, s3)) then
c     Reuse existing state - reorder heap due to key change
                       state(5, s3) = dist
                       call reheap(A, n, cmp_pathlen)
                    end if
                    go to 72
 157             continue
c     Push new state
                 call heap_push(A, n, s2, cmp_pathlen)
 72           continue
 1010      continue
c     Free the memory
           sp = sp + 1
           stack(sp) = s
 80     continue
 81     return
        end

      subroutine reheap(A, n, c)
        integer A(n), B(n), n, m, i
        external c
        m = 0
        do 99 i = 1,n
           call heap_push(B, m, A(i), c)
 99     continue
        do 990 i = 1,n
           A(i) = B(i)
 990    continue
        return
      end

      subroutine cmp_fill(a, b, r)
        integer a, b, xa, ya, xb, yb
        logical r
        integer when(100, 100)
        common /fill_trace/ when
        call dec(a, xa, ya)
        call dec(b, xb, yb)

        r = when(xa,ya).le.when(xb,yb)
        return
        end

      integer function enc(pos)
        integer pos(2)
        enc = (pos(1)-1) * 100 + pos(2) - 1
        return
      end

      subroutine dec(out, x, y)
        integer out, x, y
        x = mod(out, 100)+1
        y = out / 100 +1
        return
      end

      subroutine fill(posn, distances, doors, lines)
        parameter (INF=1 000 000)
        integer posn(2), distances(26), doors(26, 26)
        integer i, j
        logical vis(100, 100)
        integer when(100, 100), w
        integer trace(100, 100), A(100000), n, out, enc
        integer drs(26, 100, 100), ndrs(100, 100)
        external cmp_fill
        common /fill_trace/ when

        integer x, y, k, l, d, ic
        character c
        character*100 lines(100)
        w = 1

        do 33 i = 1,100
           do 32 j = 1,100
              vis(i,j) = .false.
              trace(i, j) = INF
              ndrs(i, j) = 0
 32        continue
 33     continue
        trace(posn(2), posn(1)) = 0
        n = 0
        when(posn(2), posn(1)) = w
        w = w + 1
        call heap_push(A, n, enc(posn), cmp_fill)

        do 42 while (n.gt.0)
           call heap_pop(A, n, out, cmp_fill)
           call dec(out, x, y)
           d = trace(x, y)

           c = lines(y)(x:x)
           if (c.eq.'#') then
              go to 42
           end if

           if (c.ge.'a' .and. c.le.'z') then
              ic = ichar(c) - ichar('a') + 1
              if (d.lt.distances(ic)) then
                 distances(ic) = d
                 do 420 k = 1, ndrs(x, y)
                    doors(k, ic) = drs(k, x, y)
 420             continue
              end if
           else if (c.ge.'A' .and. c.le.'Z') then
              ic = ichar(c) - ichar('A') + 1
              ndrs(x, y) = ndrs(x, y) + 1
              drs(ndrs(x, y), x, y) = ic
           end if

           if (vis(x, y)) go to 42
           vis(x, y) = .true.

           if (x.gt.1) then
              posn(1) = y
              posn(2) = x - 1
              when(posn(2), posn(1)) = w
              w = w + 1
              call heap_push(A, n, enc(posn), cmp_fill)
              trace(x - 1, y) = d + 1
              call cpydrs(x, y, x - 1, y, drs, ndrs)
           end if
           if (x.lt.100) then
              posn(1) = y
              posn(2) = x + 1
              when(posn(2), posn(1)) = w
              w = w + 1
              call heap_push(A, n, enc(posn), cmp_fill)
              trace(x + 1, y) = d + 1
              call cpydrs(x, y, x + 1, y, drs, ndrs)
           end if
           if (y.gt.1) then
              posn(1) = y - 1
              posn(2) = x
              when(posn(2), posn(1)) = w
              w = w + 1
              call heap_push(A, n, enc(posn), cmp_fill)
              trace(x, y - 1) = d + 1
              call cpydrs(x, y, x, y - 1, drs, ndrs)
           end if
           if (y.lt.100) then
              posn(1) = y + 1
              posn(2) = x
              when(posn(2), posn(1)) = w
              w = w + 1
              call heap_push(A, n, enc(posn), cmp_fill)
              trace(x, y + 1) = d + 1
              call cpydrs(x, y, x, y + 1, drs, ndrs)
           end if
 42     continue
      end

      subroutine cpydrs(x1, y1, x2, y2, drs, ndrs)
        integer x1, y1, x2, y2, drs(26, 100, 100), ndrs(100, 100)
        integer k
        do 60 k = 1,ndrs(x1, y1)
           drs(k, x2, y2) = drs(k, x1, y1)
 60     continue
        ndrs(x2, y2) = ndrs(x1, y1)
        return
      end
