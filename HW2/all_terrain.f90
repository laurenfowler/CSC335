 
      program all_terrain
      integer i, x, max_iter
      real*4 p, p0, tol, q0,q1,p1

      ! Secant
      i = 2
      max_iter = 100
      p0= 32.0
      p1= 33.0
      tol = .00001
      q0 = f(p0)
      q1 = f(p1)
 
      do while (i < max_iter)
         print *, p
         p = p1 - ((q1)*(p1-p0))/(q1-q0)
         if(abs(p-p1) < tol) then
             print *,  p
             exit
         else
             i=i+1
             p0 = p1
             q0=q1
             p1 = p
             q1 = f(p)
         end if
      end do

      if( i == max_iter) then
          print *, "failed after ", i, "iterations"
      end if
      end program

      real function f(x)
      real *4 x, A,B, C, E
      A = 89.0 * sin(11.5)
      B = 89.0 * cos(11.5)
      C = ((89 + (.5*30.0)) * sin(11.5)) - (.5 * tan(11.5) * 30.0)
      D = ((89 + (.5*30.0)) * cos(11.5)) - (.5 * 30.0)
    
      f= (A * (sin(x)*cos(x))) + (B * (sin(x))**2) - (C *cos(x)) - (E * sin(x)) 
      return
      end  
       
         
