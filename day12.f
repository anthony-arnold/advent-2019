      program day12
        integer x(3,4), xdot(3,4), f(3,4), step, i, k
        integer kin, pot, e
        data xdot/12*0/
        common /param/ x,xdot

        do i = 1,4
           read(*,*) (x(k,i),k=1,3)
        end do

        do step = 1,1000
           do i = 1,4
              do k=1,3
                 f(k,i) = 0
              end do
              call grav(x(1,i), f(1,i))
           end do
           do i = 1,4
              call euler(xdot(1,i), f(1,i))
              call euler(x(1,i), xdot(1,i))
           end do
        end do

        e = 0
        do i = 1,4
           kin = 0
           pot = 0
           do k = 1,3
              pot = pot + abs(x(k,i))
              kin = kin + abs(xdot(k,i))
           end do
           write(*,*) kin, pot
           e = e + kin * pot
        end do

        write(*,*) e
      stop
      end

      subroutine euler(xi, xidot)
        integer xi(3), xidot(3), k
        do k = 1,3
           xi(k) = xi(k) + xidot(k)
        end do
        return
      end

      subroutine grav(xi, f)
        integer x(3,4), xdot(3,4), j, k, dx, f(3), xi(3)
        common /param/ x,xdot

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
