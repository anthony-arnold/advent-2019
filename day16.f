      program day16
        implicit none
        character*650 dat
        integer input(650), output(650)
        integer input2(6 500 000), output2(6 500 000)
        integer i, j, n, offset

        offset = 0
        i = 0
        read(*,*,end=1) dat
 1      j = ichar(dat(i+1:i+1)) - ichar('0')
        if (j.ge.0 .and. j.le.9) then
           i = i + 1
           input(i) = j
           if (i.le.7) offset = offset * 10 + j
           go to 1
        end if
        n = i

        do 10 i = 1,10 000
           do 11 j = 1,n
              input2(i * j) = input(j)
 11        continue
 10     continue

        call fft(input, output, n, 0)
        do 20 i = 1,8
           dat(i:i) = char(input(i) + ichar('0'))
 20     continue
        write(*,*) dat(1:8)

c        call fft(input2, output2, n * 10 000, offset)
c        do 30 i = 1,8
c           dat(i:i) = char(input2(i+offset) + ichar('0'))
c 30     continue
c        write(*,*) dat(1:8)
      stop
      end

      subroutine fft(input, output, n, offset)
        implicit none
        integer input(n), output(n), offset
        integer i, j, k, m, n, phase, elem

        do 80 phase = 1, 100
           do 70 elem = offset+1,n
              output(elem) = 0
              i = elem
              k = 1
 2            if (i.gt.n) go to 3
              m = 0
              do 60 j = i,min(i+elem-1, n)
                 m = m + input(j)
 60           continue
              output(elem) = output(elem) + k * m
              k = k * (-1)
              i = i + elem*2
              go to 2
 3            output(elem) = mod(abs(output(elem)), 10)
 70        continue
           input = output
 80     continue
      end
