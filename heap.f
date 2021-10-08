      subroutine heap_push(A, n, i, c)
        integer A(n), n, i, j
        external c
        logical x

        n = n + 1
        j = n
1       A(j) = i
        if (j.eq.1) go to 2
        call c(A(j/2), i, x)
        if (.not. x) then
            A(j) = A(j/2)
            j = j / 2
            go to 1
        end if
2       return
      end


      subroutine heap_pop(A, n, i, c)
        integer A(n), n, i, j, t
        logical x, y
        external c

        i = A(1)
        A(1) = A(n)
        n = n - 1
        if (n.lt.2) go to 4
3       call c(A(1), A(2), x)
        if (n.gt.2) then
            call c(A(1), A(3), y)
        else
            y = .true.
        end if
        if (.not. (x .and. y)) then
            if (.not. y) then
                call c(A(2), A(3), x)
            end if
            if (.not. x) then
                j = 3
            else
                j = 2
            end if
            t = A(1)
            A(1) = A(j)
            A(j) = t
            go to 3
        end if
4       return
      end
