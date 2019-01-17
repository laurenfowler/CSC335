      
      !Pounds Problem
      program many_methods
      real *8 x
      x = 1.5
       
      
      !CALL bisection(x)
      !CALL fixed_pt(x)
      !CALL newton(x)
      !CALL secant()
      CALL steffen()
      end program


      real function f(x)
      real *8 x
      f = ((x)**3.0) - ((x)**2.0) - x - 1.0
      return
      end

      real function f_prime(x)
      real *8 x
      f_prime = (3*((x)**2.0)) - (2.0 * x) - 1
      return 
      end

      real function g(x)
      real *8 x, calc1, calc2
      calc1 = ((x)**2.0) + x + 1
      calc2 = 1.0/3.0
      g = calc1**calc2
      return
      end

      real function g2(x)
      real *8 x, calc1
      g2 = x**3.0 - x**2.0  -1
      return
      end 

      !works
      subroutine bisection()
      !bisection algorithm
      real *8 a, b, tol, p, FA, FP
      integer i, max_iter

      a=0.0
      b=2.0
      tol = 0.000000000000001
      n=1
      max_iter = 100
      FA = f(a)
      do while(n<max_iter)
        !p will be the distance of the water from the top
         p = a + (b-a)/2
         FP = f(p)
         CALL make_table(n, p0, FP)
         if ((FP == 0.0 ) .or. ( ((b-a)/2.) .le. tol) ) then
             print *,"converges at", p, "after", n, "iterations"
             EXIT
         else
             n=n+1
             if( (FA*FP)>0 ) then
                      a=p
                      FA=FP
             else
                  b=p
             end if
         end if
      end do 
      
      end

      !works
      subroutine newton(x)
      real *8 x, p, tol
      integer i, max_iter
      i = 1
      max_iter = 100
      tol = 0.000000000000001

      do while(i < max_iter)
         p = x - (f(x)/f_prime(x))
         if (abs(p-x)<tol) then
            print *, "converges at", p, "after", i, "iterations"
            exit
         end if
         CALL make_table(i, x, f(x))
         i = i +1
         x = p
      end do

      end

      !works
      subroutine secant()
      real *8 p0, p1, p, q0, q1, tol
      integer i, max_iter
      i = 2
      max_iter = 100
      p0= 0.0
      p1= 2.0 
      tol = 0.000000000000001
      q0 = f(p0)
      q1 = f(p1)

      do while (i < max_iter)
         p= p1 - ((q1)*(p1-p0))/(q1-q0)
         if (abs(p-p1) < tol) then
            print *, "converges at", p, "after", i, "iterations"
            exit
         end if
         CALL make_table(i, p0, q0)
         i = i + 1
         p0 = p1
         q0 = q1
         p1 = p
         q1 = f(p)
      end do   
      end

      !works
      subroutine fixed_pt(x)
       integer i, max_iter
       real *8 x, p
       max_iter = 100
       i = 1
       p = 1.0
       print *, x
       do while (i < max_iter)
          p = g(x)
          CALL make_table(i, x, p) 
          if ((abs(p-x)) < tol) then
              print *, "converges at ", p, "after", i, "iterations"
              exit
          else
              i = i + 1
              x=p
          end if
      end do
      end

      !doesnt work :(
      subroutine steffen()
        integer i, max_iter
        real *8 p0, tol, p1, p2, p
        tol = 0.000001
        i = 1
        max_iter = 1000
        p0 =1.5
         do while (i < max_iter)
           p1 = g(p0)
           p2 = g(p1)
           p = p0 - ((p1-p0)**2)/(((p)**2) - 2*(p1) + p0)
           CALL make_table(i, p0, p1)
           if (abs(p-p0) < tol) then
              print *, "converges at", p, "after", i, "iterations"
              exit
           end if
           i = i + 1
           p0 = p
        end do      
      end

      subroutine make_table(iter, p0, func)
         integer iter
         real *8 x, p0, func, actual, relative
         x= 1.839286755214161
         actual = abs(x-p0)
         relative = actual/x
 
         print *, iter, p0, func, relative, actual
      end 
   




