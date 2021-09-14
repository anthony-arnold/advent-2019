
      subroutine remove(chart, i, j, di, dj)
        character chart(36,36)
        integer i,j,di,dj, x, y, dx, dy, norm

        if (chart(i+di,j+dj).eq.'#') then
c     Remove all occluded ones
           call gcd(di, dj, norm)
           norm = abs(norm)
           dx = di / norm
           dy = dj / norm

           x = i + di + dx
           y = j + dj + dy
           do while(x.eq.min(max(x, 1), 36) .and.
     &              y.eq.min(max(y, 1), 36))

              chart(x, y) = 'X'
              x = x + dx
              y = y + dy
           end do
        end if
        return
        end

      integer function visible(chart, i, j)
        character*36 chart(36)
        character copy(36,36)
        integer i,j,di,dj

        do di = 1,36
           do dj = 1,36
              copy(di,dj) = chart(di)(dj:dj)
           end do
        end do
        copy(i,j) = 'Y'

        do dj = 0,35
           do di = 0,35
              if (di.ne.0 .or. dj.ne.0) then

              if (i+di.le.36) then
                 if (j+dj.le.36) then
                    call remove(copy, i, j, di, dj)
                 end if

                 if (dj.ne.0 .and. j-dj.ge.0) then
                    call remove(copy, i, j, di, -dj)
                 end if
              end if

              if (di.ne.0 .and. i-di.ge.0) then
                 if (j+dj.le.36) then
                    call remove(copy, i, j, -di, dj)
                 end if
                 if (dj.ne.0 .and. j-dj.ge.0) then
                    call remove(copy, i, j, -di, -dj)
                 end if
              end if

              end if
           end do
        end do

        visible = 0
        do di = 1,36
           do dj = 1,36
              if (copy(di,dj).eq.'#') then
                 visible = visible + 1
              end if
           end do
        end do

        return
      end

      subroutine sort(A, n)
        integer A(2,*), n
        call qsort(A, 1, n)
        return
      end

      subroutine qsort(A, lo, hi)
        integer A(2,*), lo, hi
        call quicksort(A, lo, hi)
        return
      end

      subroutine quicksort(A, lo, hi)
        integer A(2,*), lo, hi, p, partition

        if (lo.ge.1 .and. hi.ge.1) then
           if (lo.lt.hi) then
              p = partition(A, lo, hi)
              call qsort(A, lo, p - 1)
              call qsort(A, p + 1, hi)
           end if
        end if
      end

      real*8 function angle(c)
c     Cartesian to polar
        integer c(2)
        real*8 d1, d2
        d1 = float(c(1))
        d2 = float(c(2))
        angle = abs(datan2(d1, d2) - datan2(0.0d0, -1.0d0))
        return
      end

      integer function partition(A, lo, hi)
        integer A(2,*), lo, hi, i, j, k, t
        real*8 pivot, fj, angle

        pivot = angle(A(1,hi))

        i = lo - 1
        do j = lo, hi
           fj = angle(A(1,j))
           if (fj.le.pivot) then
              i = i + 1
              do k = 1,2
                 t = A(k,i)
                 A(k,i) = A(k,j)
                 A(k,j) = t
              end do
           end if
        end do
        partition = i
        return
      end

      subroutine laser(x, y, chart)
        integer x, y, i, j, n, hit
        integer asteroids(2,36*36)
        real*8 theta, phi, mag1, mag2, angle
        character*36 chart(36)
        data hit/0/

        chart(y)(x:x) = 'X'
c     Discover all asteroids
        n = 0
        do i = 1,36
           do j = 1,36
              if (chart(i)(j:j).eq.'#') then
                 n = n + 1
                 asteroids(1, n) = j - x
                 asteroids(2, n) = i - y
              end if
           end do
        end do

c     Sort all coordinates by angle from the origin
        call sort(asteroids, n)

        i = 1
 40     if (i.gt.n) then
           i = 1
        end if
        theta = angle(asteroids(1,i))
        mag1 = asteroids(1,i)**2 + asteroids(2,i)**2
        j = i
c     Linear scan (ew) for closest asteroid with this angle
        i = i + 1

 50     phi = angle(asteroids(1,i))
        if (phi.eq.theta) then
           mag2 = asteroids(1,i)**2 + asteroids(2,i)**2
           if (mag2.lt.mag1) then
              mag1 = mag2
              j = i
           end if
           i = i + 1
           if (i.le.n) then
              go to 50
           end if
        end if

        if (asteroids(1,j).lt. 1 000 000) then
c     Kill this asteroid
           hit = hit + 1
           if (hit.eq.200) then
              x = asteroids(1,j) + x - 1
              y = asteroids(2,j) + y - 1
              write(*,*) 100*x + y
              return
           end if

           asteroids(1,j) = 1 000 000
           asteroids(2,j) = 1 000 000
        end if

c     Check next angle
        go to 40
      end

      program day10
        character*36 chart(36)
        integer i,j,k,visible,vmax,v,x,y
        data vmax/0/

        do i = 1,36
           read(*,*,end=99) chart(i)
        end do
 99     continue

       do j = 1,36
           do i = 1,36
              if (chart(i)(j:j).eq.'#') then
                 v = visible(chart, i, j)

                 if (v.gt.vmax) then
                    vmax = v
                    x = j
                    y = i
                 end if
              end if
           end do
        end do

        write(*,*) vmax
        call laser(x, y, chart)
        stop
      end
