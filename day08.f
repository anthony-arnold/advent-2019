      program day08
      character*20000 ch
      integer length, w, h, i, j, size
      integer few, factor, digits(10)
      parameter(w=25, h=6, size=w*h)
      character msg(size)
      data factor/0/, digits/10*0/, few/1000000/, msg/size*'2'/

      read(*,*) ch
      do i = 1,len(ch)
         if (ch(i:i).ne.' ') then
            j = ichar(ch(i:i)) - ichar('0')
            if (j.eq.0) j = 10
            digits(j) = digits(j) + 1
         end if
         if (mod(i, w*h).eq.0) then
            if (digits(10).lt.few) then
               few = digits(10)
               factor = digits(1) * digits(2)
            end if
            do j=1,10
               digits(j) = 0
            end do
         end if

         if (ch(i:i).eq.' ') go to 2
      end do

 2    write(*,*) factor

      i = 0
 3    do j = 1,size
         if (msg(j) .eq. '2') then
            msg(j) = ch(i+j:i+j)
         end if
      end do
      i = i + size
      if (ch(i+1:i+1).ne.' ') go to 3

c     Make the message easier to read
      do i = 1,size
         if (msg(i) .eq. '0') msg(i) = ' '
      end do

c     Print it line-by-line
      do i = 1,h
         write(*,*) msg((i-1)*w+1 : i*w)
      end do

      stop
      end
