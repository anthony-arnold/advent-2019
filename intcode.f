c
c     The IntCode machine
c
      subroutine ic_param(mem, ip, modes, idx, dst)
        integer*8 mem(*), ip(2), modes, mode, dst
        integer idx

        if (modes.eq.0) then
           mode = 0
        else
           mode = MOD(modes / int(10**(idx - 1)), 10)
        end if

        select case(mode)
        case(1)
c     Immediate mode
           dst = ip(1) + idx
        case(2)
c     Relative mode
           dst = ip(2) + mem(ip(1) + idx) + 1

        case default
c     Position mode
           dst = mem(ip(1) + idx) + 1
        end select

      return
      end

      subroutine ic_bin_params(mem, ip, modes, lhs, rhs, dst)
         integer*8 ip(2), mem(*), lhs, rhs, dst, modes

         call ic_param(mem, ip, modes, 1, lhs)
         call ic_param(mem, ip, modes, 2, rhs)
         call ic_param(mem, ip, modes, 3, dst)
         ip(1) = ip(1) + 4
         return
      end

      subroutine ic_op_add(mem, ip, modes)
         integer*8 ip(2), mem(*), lhs, rhs, dst, modes
         call ic_bin_params(mem, ip, modes, lhs, rhs, dst)
         mem(dst) = mem(lhs) + mem(rhs)
        return
      end

      subroutine ic_op_mul(mem, ip, modes)
         integer*8 ip(2), mem(*), lhs, rhs, dst, modes
         call ic_bin_params(mem, ip, modes, lhs, rhs, dst)
         mem(dst) = mem(lhs) * mem(rhs)
        return
      return
      end

      subroutine ic_op_lt(mem, ip, modes)
         integer*8 ip(2), mem(*), lhs, rhs, dst, modes

         call ic_bin_params(mem, ip, modes, lhs, rhs, dst)

         if (mem(lhs).lt.mem(rhs)) then
            mem(dst) = 1
         else
            mem(dst) = 0
         end if
      return
      end

      subroutine ic_op_eq(mem, ip, modes)
         integer*8 ip(2), mem(*), lhs, rhs, dst, modes
         call ic_bin_params(mem, ip, modes, lhs, rhs, dst)

         if (mem(lhs).eq.mem(rhs)) then
            mem(dst) = 1
         else
            mem(dst) = 0
         end if
      return
      end

      subroutine ic_op_inp(mem, ip, modes, input, *)
        integer*8 ip(2), mem(*), dst, modes
        external input

        call ic_param(mem, ip, modes, 1, dst)
        ip(1) = ip(1) + 2
        call input(mem(dst), *66)

        return
 66     return 1
      end

      subroutine ic_op_out(mem, ip, modes, output, *)
         external output
         integer*8 ip(2), mem(*), val, modes

         call ic_param(mem, ip, modes, 1, val)
         ip(1) = ip(1) + 2
         call output(mem(val), *77)

         return
 77      return 1
      end

      subroutine ic_op_jzn(mem, ip, modes)
         integer*8 ip(2), mem(*), val, modes, dst

         call ic_param(mem, ip, modes, 1, val)
         call ic_param(mem, ip, modes, 2, dst)

         if (mem(val).ne.0) then
            ip(1) = mem(dst) + 1
         else
            ip(1) = ip(1) + 3
         end if
         return
      end

      subroutine ic_op_jze(mem, ip, modes)
         integer*8 ip(2), mem(*), val, modes, dst

         call ic_param(mem, ip, modes, 1, val)
         call ic_param(mem, ip, modes, 2, dst)

         if (mem(val).eq.0) then
            ip(1) = mem(dst) + 1
         else
            ip(1) = ip(1) + 3
         end if
         return
      end

      subroutine ic_op_adj(mem, ip, modes)
         integer*8 ip(2), mem(*), val, modes

         call ic_param(mem, ip, modes, 1, val)

         ip(1) = ip(1) + 2
         ip(2) = ip(2) + mem(val)
         return
      end

      subroutine intcode(mem, input, output)
         external input, output
         integer*8 ip(2), mem(*)

         ip(1) = 1
         ip(2) = 0

         call intcode_yield(mem, input, output, ip, *88)
 88      return
      end

      subroutine intcode_yield(mem, input, output, ip, *)
         external input, output
         integer*8 ip(2), mem(*), instruction, op, modes

 1       instruction = mem(ip(1))
         op = MOD(instruction, 100)
         modes = instruction / 100

         select case (op)
         case (1)
            call ic_op_add(mem, ip, modes)

         case (2)
            call ic_op_mul(mem, ip, modes)

         case(3)
            call ic_op_inp(mem, ip, modes, input, *98)

         case(4)
            call ic_op_out(mem, ip, modes, output, *98)

         case(5)
            call ic_op_jzn(mem, ip, modes)

         case(6)
            call ic_op_jze(mem, ip, modes)

         case(7)
            call ic_op_lt(mem, ip, modes)

         case(8)
            call ic_op_eq(mem, ip, modes)

         case(9)
            call ic_op_adj(mem, ip, modes)

         case (99)
            go to 99

         case default
            write(*,*) 'INVALID OPCODE', op
            stop
         end select

         go to 1

 99     return
 98     return 1
      end
