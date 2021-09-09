      program day03
      character c
      integer wire, n, x(3), w(3), grid(3,1000), i
      integer ans(2), xsct(2)
      data x/3*0/, i/1/, ans/2*1 000 000 000/, wire/1/, grid/3000 * 0/

c     Read first wire direction
 2    read(*,*) c, n
      if (c.eq.'-') then
c        Go to second wire
         x = 0
         go to 3
      end if

c     Move to new position
      call move(x, c, n)
      i = i + 1
c     Record next position and total distance
      grid(1,i) = x(1)
      grid(2,i) = x(2)
      grid(3,i) = x(3)
      go to 2

c     Read second wire
 3    read(*,*) c, n
      if (c.eq.'-') go to 4

c     Remember previous position and total distance
      w(1) = x(1)
      w(2) = x(2)
      w(3) = x(3)

c     Move to next position
      call move(x, c, n)

c     Find intersection with first wire
      call intersect(grid, w, x, i, xsct)
      ans(1) = min(xsct(1), ans(1))
      ans(2) = min(xsct(2), ans(2))
      go to 3

c     Write answers to part 1 & 2
 4    write(*,*) ans
      end

      function intersection(x, y, z)
      integer x(4), y(4), z(2), x0, x1, x2, y0, y1, y2
      logical intersection

      intersection = .false.
c     A rather verbose way of seeing if two line segments are orthogonal
      if (x(1).eq.x(3)) then
         x0 = min(y(1), y(3))
         x1 = x(1)
         x2 = max(y(1), y(3))

         y0 = min(x(2), x(4))
         y1 = y(2)
         y2 = max(x(2), x(4))
         go to 90
      else
         x0 = min(x(1), x(3))
         x1 = y(1)
         x2 = max(x(1), x(3))

         y0 = min(y(2), y(4))
         y1 = x(2)
         y2 = max(y(2), y(4))
         go to 90
      end if

c     Determine the point of intersection
 90   if ( x0.lt.x1 .and. x1.lt.x2 .and.
     &     y0.lt.y1 .and. y1.lt.y2) then
         intersection = .true.
         z(1) = x1
         z(2) = y1
      end if

      return
      end

      subroutine intersect(grid, x, y, i, out)
      integer i, j, k, x(3), y(3), grid(3,*)
      integer z(4), w(4), v(2), out(2)
      real norm
      logical intersection

c     Set something really high in case there is no intersect
      out = 1 000 000 000

      z(1) = x(1)
      z(2) = x(2)
      z(3) = y(1)
      z(4) = y(2)

      w(1) = grid(1,1)
      w(2) = grid(2,1)

c     Check each length of wire 1
      do k = 2, i
         w(3) = grid(1,k)
         w(4) = grid(2,k)

c        Find any intersection point of wire 1 & 2
         if (intersection(z, w, v)) then
c           For part 1 - record manhattan distance to origin
            out(1) = abs(v(1)) + abs(v(2))

c           For part 2 - record total wire length from intersection point
            norm = float((x(1)-v(1))**2 + (x(2)-v(2))**2)
            out(2) = sqrt(norm) + x(3)

            norm = float((w(1)-v(1))**2 + (w(2)-v(2))**2)
            out(2) = out(2) + sqrt(norm) + grid(3,k-1)
            return
         end if

         w(1) = w(3)
         w(2) = w(4)
      end do
      return
      end

      subroutine move(x, c, n)
      integer x(3), n
      character c
      if (c.eq.'L') x(1) = x(1) - n
      if (c.eq.'R') x(1) = x(1) + n
      if (c.eq.'U') x(2) = x(2) - n
      if (c.eq.'D') x(2) = x(2) + n
      x(3) = x(3) + n
      return
      end
