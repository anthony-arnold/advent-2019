      program day14
        implicit none
        integer f, i, j, k, n, p
        character*6 chem(10, 100), name(100)
        integer point(10, 100), qty(10, 100), ning(100), makes(100)
        real*8 store(100), m
        common /shared/ point, name, qty, ning, makes, store
        data store/100*0/

        n = 0
 1      read(*,*,end=2) j
        n = n + 1
        ning(n) = j
        read(*,*) (qty(i,n), chem(i,n), i=1,j+1)
        name(n) = chem(j+1, n)
        makes(n) = qty(j+1, n)
        if (name(n).eq.'FUEL') then
           f = n
        end if
        go to 1

c     Yuck, let's just do this once.
 2      do 23 i=1,n
           do 22 j=1,ning(i)
              if (chem(j, i).eq.'ORE') then
                 point(j, i) = 100
              else
                 do 21 k=1,n
                    if (name(k).eq.chem(j,i)) then
                       point(j,i) = k
                    end if
 21              continue
              end if
 22        continue
 23     continue

        call fuelup(n, f)
        m = -store(100)
        write(*,*) int(m)

c     A *VERY* slow process when ore:fuel ratio is low.
c     I hope our factory isn't too efficient!
        store(100) = 1.0E12 - m
        p = 1
        do 4 while(store(100).ge.m)
           call fuelup(n, f)
           p = p + 1
 4      continue
        write(*,*) p

        stop
      end

      subroutine fuelup(n, f)
        implicit none
        character*6 name(100)
        integer point(10, 100), qty(10, 100), ning(100), makes(100)
        integer stack(2, 1000)
        real*8 store(100)
        integer sp, f, i, j, k, l, m, n, p, q, r
        common /shared/ point, name, qty, ning, makes, store

        stack(1, 1) = f
        stack(2, 1) = 1
        sp = 1

 6      do 10 while (sp.gt.0)
           i = stack(1, sp)
           j = stack(2, sp)
           sp = sp - 1

c     Any in the store?
           r = min(store(i), float(j))
           j = j - r
           store(i) = store(i) - r
           if (j.eq.0) go to 10

c     How many times to apply the recipe?
           p = j / makes(i)
           if (mod(j, makes(i)) .ne. 0) then
              p = p + 1
           end if
           store(i) = store(i) + makes(i)*p - j

c     Look up all ingredients to make this chemical.
           do 9 l = 1,ning(i)
c     How much is needed
              q = p * qty(l, i)
              m = point(l, i)
              if (m.eq.100) then
c     ORE
c     Remove it from the store if able
                 store(100) = store(100) - q
              else
c     Make it
 8               sp = sp + 1
                 stack(1, sp) = m
                 stack(2, sp) = q
              end if
 9         continue
 10     continue
        return
      end
