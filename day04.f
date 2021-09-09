      program day04
      integer i, j, k, l, m, n(2), p, q, r
      logical rept
      n = 0

      read(*,*) i, j

c     Reset
 1    k = i
      p = 0
      q = 999999
      r = 0

c     Get the rightmost digit
      l = MOD(k, 10)

c     Get the next digit
 2    m = MOD(k/10, 10)

c     Ensure digits are decreasing
      if(m.le.l) then
c        Check for a group
         if (m.eq.l) then
c           A group of at least 2
            p = p + 1
            r = p
         else
c           Record the size of the smallest group
            if (p.ge.1) q = min(q, p)
            p = 0
         end if

         l = m
         k = k / 10
         if (k.ne.0) go to 2
      end if

 3    if (k.eq.0) then
c        All the digits were in order
         if (r.gt.0) then
c     Had some repeating digits (part one)
            n(1) = n(1) + 1
         end if
         if (q.eq.1) then
c     Had repeating digits of group size exactly 2
            n(2) = n(2) + 1
         end if
      end if

c     Next number in the sequence
      i = i + 1
      if (i.le.j) go to 1

      write(*,*) n
      stop
      end
