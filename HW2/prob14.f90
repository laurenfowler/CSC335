! problem 14

      program prob14
      real *4 p0, p, tol
      integer i, max_iter
      i = 0
      max_iter = 10000
      tol = 0.00001

      !part a fixed point
      print *, "part a"
      p0=2.0
      do while (i < max_iter)
          p = part_a(p0)
          !print *,  p0, p
          if ((abs(p-p0)) < tol) then
              print *, "converges at ", p, "after", i, "iterations"
              exit
          else
              i = i + 1
              p0=p
          end if
      end do

      !part b fixed point
      print *, "part b"
      i=0
      p0 = 2.0
      do while (i < max_iter)
          p = part_b(p0)
          !print *,  p0, p
          if ((abs(p-p0)) < tol) then
              print *, "converges at ", p, "after", i, "iterations"
              exit
          else
              i = i + 1
              p0=p
          end if
      end do

      print *, "part c"
      i=0
      p0 = 0.0
      do while (i < max_iter)
         p = part_c(p0)
         !print *,  p0, p
          if ((abs(p-p0)) < tol) then
              print *, "converges at ", p, "after", i, "iterations"
              exit
          else
              i = i + 1
              p0=p
          end if
      end do

      print *, "part d"
      i=0
      p0=0.0
      do while (i < max_iter)
         p = part_d(p0)
          !print *,  p0, p
          if ((abs(p-p0)) < tol) then
              print *, "converges at ", p, "after", i, "iterations"
              exit
          else
              i = i + 1
              p0=p
          end if
      end do
      end program


      real function part_a(x)
      real *4 x
      part_a = 2 + sin(x) - x
      return
      end
 
      real function part_b(x)
      !rearranged fucntion
      real *4 x, calc1, expon
      calc1 = (2.0*x) + 5.0
      expon = 1.0/3.0
      part_b = (calc1)**expon
      return
      end 

      real function part_c(x)
      !rearranged function
      real *4 x, calc1
      calc1 = 0.333* exp(x)
      part_c = sqrt(calc1)
      return
      end 

      real function part_d(x)
      real *4 x
      part_d = x - cos(x)
      return
      end 

