
      program steffenson
      real *4 tol, p0, p1, p2, p
      integer i, max_iter
      i = 1
      max_iter = 100
      tol = 0.00001

      do while (i<max_iter)
         p1 = part_d(p0)
         p2 = part_d(p1)
         p = p0 - ((p1-p0)**2)/(p2-(2*p1) + p0)
         print *, p, p0
         if (abs(p-p0) < tol) then
            print *, "converges at", p, "after", i, "iterations"
            exit
         end if
         i = i+1
         p0 = p
      end do
      end program

      real function part_a(x)
      real *4 x      
      part_a = x - 2 - sin(x) + x
      return
      end 

      real function part_b(x)
      real *4 calc1
      calc1 = 1.0/3.0
      part_b = ((2.0)*x + 5.0)**calc1
      return
      end 

      real function part_c(x)
      part_c = x -  ((3.0 * ((x)**2)) - exp(x)) 
      return
      end 

      real function part_d(x)
      part_d = x - cos(x) + x
      return 
      end 
