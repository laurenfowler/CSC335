      ! Problem 14

      program drug_admin
      integer i, x, max_iter
      real*4 p, p0, tol
     
      ! Newtons Method 
      i = 1
      max_iter = 100
      p0= 21.0
      tol = .00001

      do while (i < max_iter)
         !print *, p, p0 
         p = p0 - (f1(p0)/f_prime1(p0))
         print *, p0, p
         if(abs(p-p0) < tol) then
             print *, "time for second injection is at ", p, "hours"
             exit
         else
             i=i+1
             p0 = p
         end if
      end do
      
      if( i == max_iter) then
          print *, "failed after ", i, "iterations"
      end if
      end program

      real function f(x)
         real *4 x, calc1, A
         A = 0.75
         calc1 = A * exp(x/3.0)
         f = x-calc1
         return 
      end

      real function f_prime(x)
         real *4 x, calc1, A, calc2
         A = 0.75
         calc1 = A/3.0
         calc2 = calc1 * exp(x/3.0)
         f_prime = 1 - calc2
         return
      end

      !real function f12(x)
      !   real *4 x, calc1, calc2, calc3, calc4, tt0
      !   tt0 = x + 11.0833
      !   calc1 = ((1.0/3.0)*exp(1.0)) *  (exp(-tt0/3.0)) * 11.0833
      !   calc2 = ((1.0/3.0)*exp(1.0)) * x * exp(-tt0/3.0)
      !   calc3 = (1.0/4.0) * x * exp(-x/3.0)
      !   f1 = calc1 + calc2 + calc3 - .25
      !   return 
      !end

      real function f1(x)
         real *4 x, A, t0, calc1, calc2
         t0 = 11.0833
         A = (.75 * (exp(1.0)/3.0)) + .25
         calc1 = -t0 * exp(-t0/3.0)
         calc2 = 1.0 + exp(-t0/3.0)
         f1 = x - A*(calc1/calc2)
         return
      end

      !real function f_prime1(x)
      !f_prime1 = 1.0
      !return
      !end

      real function f_prime1(x)
         real *4 x, calc1, calc2, calc3, calc4, tt0
         tt0 = x + 11.0833
         calc1 =((1.0/3.0)*exp(1.0))* 11.0833*exp(-tt0/3.0)*(-1.0/3.0)
         calc2 = ((1.0/3.0)* exp(1.0)) * exp(-tt0/3.0)
         calc3 = (-1.0/6.0) * exp(1.0) * x * exp(-tt0/3.0)
         calc4 = ((1.0/4.0) *(-1.0/3.0)) * x * exp(-x/3.0) 
         f_prime1=calc1+calc2+calc3+calc4 + (1.0/4.0)*exp(-t/3.0)
         return
      end

