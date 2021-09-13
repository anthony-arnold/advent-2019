      program day07
        integer*8 prog(10000)
        integer params, i, seq(5)
        data seq/0,1,2,3,4/

        read(*,*, end=2) prog
c     Part One
 2      call fire(prog, seq)

c     Part two
        do i=1,5
           seq(i) = i+4
        end do
        call fire(prog, seq)
      stop
      end

      subroutine fire(prog, seq)
        external sig_in, sig_out
        integer*8 prog(10000), amp(10000,5), ip(2,5)
        integer params, i, j, thrust, started, halted, seq(5)
        logical final

        common/input/ params(2)

        thrust = 0
 1      params(1) = 0
        started = 0
        halted = 0
        do i = 1,5
           ip(1,i) = 1
        end do

 3      continue
        do 5 i = 1,5
           if (started .lt. i) then
              params(2) = params(1)
              params(1) = seq(i)
              do j=1,10000
                 amp(j,i) = prog(j)
              end do
              started = started + 1
           end if
           call intcode_yield(amp(1,i), sig_in, sig_out, ip(1,i), *5)
c     Halted
           halted = halted + 1
           if (halted.eq.5) go to 7
c     Pre-empted
 5      continue
        go to 3

 7      thrust = max(thrust, params(1))

        call permute(seq, final)
        if (.not. final) go to 1
        write(*,*) thrust
      return
      end

      subroutine sig_in(dst, *)
        integer*8 dst
        integer params
        logical  ir
        common/input/ params(2)

        dst = params(1)
        params(1) = params(2)

        return
      end

      subroutine sig_out(val, *)
        integer*8 val
        integer params
        logical ir
        common/input/ params(2)

        params(1) = val

c       Always yield after output
        return 1
      end

      subroutine permute(seq, final)
c       Use lexicographical ordering to get the next permutation.
        integer seq(5), k, l, t, i, j
        logical final

        final = .false.
c     Find the first index
        k = 0
        do i = 1,4
           if (seq(i) .lt. seq(i+1)) k = i
        end do
        if (k .eq. 0) then
           final = .true.
           go to 19
        end if

c     Find the swap candidate
        do i = k+1,5
           if (seq(k) .lt. seq(i)) l = i
        end do
c     Swap
        t = seq(k)
        seq(k) = seq(l)
        seq(l) = t
c     Reverse
        i = k + 1
        j = 5
 14     if (i.lt.j) then
           t = seq(i)
           seq(i) = seq(j)
           seq(j) = t
           i = i + 1
           j = j - 1
           go to 14
        end if
 19     return
      end
