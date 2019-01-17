      
      program drug_secant
      real *4 p, p0, p1, q, q0, tol
      integer i, max_iter
      i=2 
      max_iter = 100
      
      p0 = 20.0
      p1 = 25.0
      q0 = f(p0)
      q1 = f(p1)

      do while(i<max_iter)
         p = p1 - (q1*(p1-p0))/(q1-q0) 
         print *, p, p1
         if (abs(p-p1) < tol) then
            print *, "converges at", p, "after", i, "iterations"
            exit
         end if
         i = i+1
         p0 = p1
         p1 = p
         q0 = q1
         q1 = f(p)
      end do
      end program 


      real function f(x)
         real *4 x, calc1, A
         A = 0.75
         calc1 = A * exp(x/3.0)
         f = x-calc1
         return
      end

