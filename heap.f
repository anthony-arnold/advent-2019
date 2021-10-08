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
        integer A(n), n, i, j, t, p
        logical x, y
        external c

        i = A(1)
        A(1) = A(n)
        n = n - 1
        p = 1
3       if (2*p.gt.n) go to 4
        call c(A(p), A(2*p), x)
        if (n.gt.2*p) then
            call c(A(p), A(2*p+1), y)
        else
            y = .true.
        end if
        if (.not. x .or. .not. y) then
            if (.not. y) then
                call c(A(2*p), A(2*p+1), y)
            end if
            if (.not. y) then
                j = 2 * p + 1
            else
                j = 2 * p
            end if
            t = A(p)
            A(p) = A(j)
            A(j) = t
            p = j
            go to 3
        end if
4       return
      end
