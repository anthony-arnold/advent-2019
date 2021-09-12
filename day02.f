
      program day02
         integer n(255), i
         integer*8 m(255)
         integer noun, verb
         data noun/0/, verb/0/

         read(*,*,END=2) n
2        continue

3        do i = 1,255
            m(i) = n(i)
         end do
         m(2) = noun
         m(3) = verb
         call intcode(m)
c
c Write part one solution
         if (noun .eq. 12 .and. verb .eq. 2) then
            write(*,*) m(1)
         end if

         if (m(1).ne.19690720) then
             noun = noun + 1
             if (noun .eq. 100) then
               noun = 0
               verb = verb + 1
             end if
            go to 3
         end if

         write(*,*) 100 * noun + verb
      stop
      end
