c
c     The IntCode machine
c

      integer function ic_param(mem, ip, idx)
        integer mem(*), ip, idx

c     Get immediate value
        ic_param = mem(ip + idx)
      return
      end

      integer function ic_mode_param(mem, ip, modes, idx)
      integer mem(*), ip, modes, idx, mode
      if (modes.eq.0) then
         mode = 0
      else
         mode = MOD(modes / int(10**(idx - 1)), 10)
      end if

c     Get immediate value
        ic_mode_param = ic_param(mem, ip, idx)

        if (mode.eq.0) then
c     Position mode
           ic_mode_param = mem(ic_mode_param + 1)
        end if
      return
      end

      subroutine ic_bin_params(mem, ip, modes, lhs, rhs, dst)
        integer mem(*), ip, modes, lhs, rhs, dst
        lhs = ic_mode_param(mem, ip, modes, 1)
        rhs = ic_mode_param(mem, ip, modes, 2)
        dst = ic_param(mem, ip, 3) + 1
        ip = ip + 4
        return
      end

      subroutine ic_op_add(mem, ip, modes)
         integer mem(*), ip, modes, lhs, rhs, dst
         call ic_bin_params(mem, ip, modes, lhs, rhs, dst)
         mem(dst) = lhs + rhs
        return
      end

      subroutine ic_op_mul(mem, ip, modes)
         integer mem(*), ip, modes, lhs, rhs, dst
         call ic_bin_params(mem, ip, modes, lhs, rhs, dst)
         mem(dst) = lhs * rhs
        return
      return
      end

      subroutine ic_op_lt(mem, ip, modes)
         integer mem(*), ip, modes, lhs, rhs, dst
         call ic_bin_params(mem, ip, modes, lhs, rhs, dst)
         if (lhs.lt.rhs) then
            mem(dst) = 1
         else
            mem(dst) = 0
         end if
      return
      end

      subroutine ic_op_eq(mem, ip, modes)
         integer mem(*), ip, modes, lhs, rhs, dst
         call ic_bin_params(mem, ip, modes, lhs, rhs, dst)
         if (lhs.eq.rhs) then
            mem(dst) = 1
         else
            mem(dst) = 0
         end if
      return
      end

      subroutine ic_op_inp(mem, ip, input, *)
        integer mem(*), ip, dst
        external input

        dst = ic_param(mem, ip, 1)
        ip = ip + 2
        call input(mem(dst + 1), *66)

        return
 66     return 1
      end

      subroutine ic_op_out(mem, ip, modes, output, *)
        external output
        integer mem(*), ip, modes, val

        val = ic_mode_param(mem, ip, modes, 1)
        ip = ip + 2
        call output(val, *77)

        return
 77     return 1
      end

      subroutine ic_op_jzn(mem, ip, modes)
        integer mem(*), ip, modes, val
        val = ic_mode_param(mem, ip, modes, 1)
        dst = ic_mode_param(mem, ip, modes, 2)

        if (val.ne.0) then
           ip = dst + 1
        else
           ip = ip + 3
        end if
      return
      end

      subroutine ic_op_jze(mem, ip, modes)
        integer mem(*), ip, modes, val
        val = ic_mode_param(mem, ip, modes, 1)
        dst = ic_mode_param(mem, ip, modes, 2)

        if (val.eq.0) then
           ip = dst + 1
        else
           ip = ip + 3
        end if
      return
      end

      subroutine intcode(mem, input, output)
        external input, output
        integer ip, mem(*)
        ip = 1
        call intcode_preempt(mem, input, output, ip, *88)
 88     return
      end

      subroutine intcode_preempt(mem, input, output, ip, *)
        external input, output
        integer mem(*), ip, instruction, op, modes

 1      instruction = mem(ip)
        op = MOD(instruction, 100)
        modes = instruction / 100

        if (op.eq.99) go to 99
        if (op.eq.1) then
           call ic_op_add(mem, ip, modes)

        else if (op.eq.2) then
           call ic_op_mul(mem, ip, modes)

        else if (op.eq.3) then
           call ic_op_inp(mem, ip, input, *98)

        else if (op.eq.4) then
           call ic_op_out(mem, ip, modes, output, *98)

        else if (op.eq.5) then
           call ic_op_jzn(mem, ip, modes)

        else if (op.eq.6) then
           call ic_op_jze(mem, ip, modes)

        else if (op.eq.7) then
           call ic_op_lt(mem, ip, modes)

        else if (op.eq.8) then
           call ic_op_eq(mem, ip, modes)

        end if
        go to 1

 99     return
 98     return 1
      end
