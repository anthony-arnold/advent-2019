      program day12
        integer*8 x(3,4), xdot(3,4), f(3,4), step, i, k, l(3), m, n
        data xdot/12*0/, l/3*0/
        common /param/ x,xdot

        do i = 1,4
           read(*,*) (x(k,i),k=1,3)
        end do

        step = 1
 1      do i = 1,4
           call grav(x(1,i), f(1,i))
        end do
        do i = 1,4
           call euler(xdot(1,i), f(1,i))
           call euler(x(1,i), xdot(1,i))
        end do

        if (step.eq.1000) then
           call energy
        end if

        do k = 1,3
           do i = 1,4
              if (xdot(k,i).ne.0) go to 5
           end do
           l(k) = step
 5         continue
        end do

        step = step + 1
        if (l(1)*l(2)*l(3).eq.0 .or. step.le.1000) go to 1

        call lcm(l(1), l(2), m)
        call lcm(m, l(3), n)
        write(*,*) n

        stop
      end

      subroutine energy
        integer*8 e, kin, pot, x(3,4), xdot(3,4), i, k
        common /param/ x,xdot

        e = 0
        do i = 1,4
           kin = 0
           pot = 0
           do k = 1,3
              pot = pot + abs(x(k,i))
              kin = kin + abs(xdot(k,i))
           end do
           e = e + kin * pot
        end do

        write(*,*) e
      end

      subroutine euler(xi, xidot)
        integer*8 xi(3), xidot(3), k
        do k = 1,3
           xi(k) = xi(k) + xidot(k)
        end do
        return
      end

      subroutine grav(xi, f)
        integer*8 x(3,4), xdot(3,4), j, k, dx, f(3), xi(3)
        common /param/ x,xdot
        do k = 1,3
           f(k) = 0
        end do

        do j = 1,4
           do k = 1,3
              dx = x(k,j) - xi(k)
              if (dx.lt.0) then
                 f(k) = f(k) - 1
              else if (dx.gt.0) then
                 f(k) = f(k) + 1
              end if
           end do
        end do
        return
      end
