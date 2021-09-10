       program day06
        character*4 table(2, 10000), stack(10000), name
        integer n, i, p, depths(10000), d, dtot
c        
c   Depth of me, santa, and common ancestor.        
        integer dme, dsan, danc
        data n/1/, dme/-1/, dsan/-1/

c       Read each orbit        
 1      read(*,*,end=2) (table(i, n), i=1,2)
        n = n + 1
        go to 1
 2      continue
c
c       Depth-first search to discover all the nodes
        stack(1) = 'COM'
        depths(1) = 0
        p = 1
        dtot = 0
c
c       While stack is not empty
 3      if (p.eq.0) go to 4
            name = stack(p)
            d = depths(p)
            p = p - 1
c
c           Sum total depths (equivalent to number of orbits)
            dtot = dtot + d
c
c           Find all the direct descendants of current node.
c           Unfortunate linear scan.
            do i = 1,n
                if (table(1, i).eq.name) then
                    p = p + 1
                    stack(p) = table(2, i)
                    depths(p) = d + 1
c
c                   Check for me or santa and remember the depth of each.
c                   After finding the first one, record the minimum depth of all
c                   subsequent nodes until the other is found. This gives the 
c                   depth of the common ancestor.
                    if (table(2, i).eq.'YOU') then
                        dme = d
                        if (dsan.lt.0) danc = d
                    else if (table(2, i).eq.'SAN') then
                        dsan = d
                        if (dme.lt.0) danc = d 
                    else if (dme.lt.0 .or. dsan.lt.0) then   
                        danc = min(danc, d-1)
                    end if
                end if
            end do  
        go to 3
c
c       Number of transfers is the sum of the differences in depth from me 
c       and santa to the common ancestor.
 4      write(*,*) dtot, (dme - danc) + (dsan - danc)
        stop
       end program
