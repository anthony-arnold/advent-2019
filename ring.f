      subroutine ring_push(buffer, i, n, qp)
        integer*8 n, buffer(n), i, qp(2)

        qp(2) = mod(qp(2) + 1, n)
        if (qp(2) .eq. qp(1)) then
           write(*,*) 'overflow'
           stop
        end if

        buffer(qp(2)+1) = i
        return
      end

      subroutine ring_pop(buffer, i, n, qp)
        integer*8 n, buffer(n), i, qp(2)

        if (qp(1).eq.qp(2)) then
           write(*,*) 'empty'
           stop
        end if

        qp(1) = mod(qp(1) + 1, n)
        i = buffer(qp(1)+1)
        return
      end
