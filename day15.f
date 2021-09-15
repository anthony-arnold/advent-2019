
      program day15
        implicit none
        integer*8 maze(80, 80), x, y, move, ox(2)
        common /droid/ maze, x, y, move, ox
        data maze/6400*0/

        call part1
        call part2
        stop
      end

      subroutine part1
        implicit none
        external input, output
        integer*8 maze(80, 80), x, y, move, ox(2)
        integer*8 mem(10000), Q(2, 80, 80)
        integer*8 i, j, pmin(2), dmin, w, v, alt
        common /droid/ maze, x, y, move, ox
        data Q/12800*0/

c     Get the program
        read(*,*,end=1) mem
 1      continue

c     Start in the middle
        maze(40, 40) = 1
        x = 40
        y = 40

c     Go up to start
        move = 1
        call intcode(mem, input, output)
c     ox now contains the target coordinates

c     Use Dijkstra to find shortest path
        do 5 j = 1,80
           do 4 i = 1,80
              if (i.ne.ox(2) .or. j.ne.ox(1)) then
                 if (maze(j,i).gt.0 .and. maze(j,i).lt.1 000 000) then
                    Q(1, j, i) = 1
                    Q(2, j, i) = 1 000 000
                 end if
              end if
 4         continue
 5      continue

c     Start at the oxygen system
        pmin(1) = ox(1)
        pmin(2) = ox(2)
        dmin = 0

c     Remove the current vertex from Q
 6      Q(1, pmin(1), pmin(2)) = 0

c     Stop when the origin is found
        if (pmin(1).eq.40 .and. pmin(2).eq.40) go to 99

c     Check all adjacent vertices
        do 8 move = 1,4
           call next(pmin(2), pmin(1), move, v, w)
           if (Q(1, w, v).eq.1) then
c     Still in Q
              alt = dmin + 1
              if (alt.lt.Q(2, w, v)) then
c     Update distance from ox
                 Q(2, w, v) = alt
              end if
           end if
 8      continue

c     Find the vertex in Q with smallest distance.
c     Would much prefer a min-heap but I don't want to write one.
        dmin = 1 000 001
        do 12 j = 1,80
           do 11 i = 1,80
              if (Q(1, j, i).ne.0 .and. Q(2, j, i).lt.dmin) then
                 pmin(1) = j
                 pmin(2) = i
                 dmin = Q(2, j, i)
              end if
 11        continue
 12     continue
        go to 6

 99     write(*,*) Q(2, 40, 40)
      end

      subroutine part2
        implicit none
        integer*8 maze(80, 80), x, y, move, ox(2)
        integer*8 w, v, time, i, j
        integer*8 queue(3, 1600), qp
        common /droid/ maze, x, y, move, ox

c     Perform a breadth-first search for all vertices to fill.

c     Start at the oxygen system at T=0
        queue(1, 1) = ox(1)
        queue(2, 1) = ox(2)
        queue(3, 1) = 0
        qp = 1

        do 20 while(qp.gt.0)
c     Get front of queue
           y = queue(1, 1)
           x = queue(2, 1)
           time = queue(3, 1)

c     No memmove (mumble, mumble)
           do 22 i = 2,qp
              do 23 j = 1,3
                 queue(j, i-1) = queue(j, i)
 23           continue
 22        continue
           qp = qp - 1

c     Mark as visited.
           maze(y, x) = 1 000 000

c     Enqueue all unvisited adjacent vertices
           do 21 move = 1, 4
              call next(x, y, move, v, w)
              if (maze(w, v).lt.1 000 000) then
                 qp = qp + 1
                 queue(1, qp) = w
                 queue(2, qp) = v

c     Increase time for the next step
                 queue(3, qp) = time + 1
              end if
 21        continue
 20     continue

        write(*,*) time
      end

      subroutine next(x, y, move, x2, y2)
c     Get the next coordinates based on the move
        integer*8 x, y, move, x2, y2
        x2 = x
        y2 = y
        select case(move)
          case (1)
             y2 = y - 1
          case (2)
             y2 = y + 1
          case (3)
             x2 = x - 1
          case (4)
             x2 = x + 1
          end select
        return
      end

      subroutine input(val, *)
        integer*8 val, x2, y2, m, small, smallm, i
        integer*8 maze(80, 80), x, y, move, ox(2)
        common /droid/ maze, x, y, move, ox
        small = 1 000 000

c     Find next best tile to go to
        m = move
        do 3 i=1,4
           call next(x, y, m, x2, y2)
           if (maze(y2, x2).lt.small) then
              smallm = m
              small = maze(y2, x2)
           end if
           m = m + 1
           if (m.gt.4) m = 1
 3      continue

        move = smallm
        val = move
        call next(x, y, move, x2, y2)
        return
      end

      subroutine output(status, *)
        integer*8 status, x2, y2, i, j, k
        integer*8 maze(80, 80), x, y, move, ox(2)
        common /droid/ maze, x, y, move, ox

        call next(x, y, move, x2, y2)
        if (status.eq.0) then
c     Wall
           maze(y2, x2) = 1 000 000
        else
c     Move the droid
           x = x2
           y = y2
           maze(y, x) = maze(y, x) + 1
           if (status.eq.2) then
c     Found it!
              ox(1) = y
              ox(2) = x
           end if
        end if

c     Finished when there are no free spaces with adjacent undiscovered tiles.
        do 50 j = 1,80
           do 80 i = 1,80
              if (maze(j, i).gt.0 .and. maze(j, i).lt.1 000 000) then
                 do 30 k = 1,4
                    call next(i, j, k, x2, y2)
                    if (maze(y2, x2).eq.0) return
 30              continue
              end if
 80        continue
 50     continue

c     Found none; finished
        return 1
      end
