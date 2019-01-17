      ! Section 2.1 #15
      ! Find depth of water within .01 ft

      PROGRAM trough
      !Volume, Length, Inital Height, Tolerance, current volume, and  previous and current
      !guess difference 

      !bisection algorithm
      real *4 a, b, tol, p, vol_a, vol_p, vol
      integer i, max_iter

      a=0
      b=1
      tol = 0.01
      i=1
      max_iter = 50
      vol = 12.4
      
      vol_a = calculate_vol(a)
      do while(i<max_iter)
        !p will be the distance of the water from the top
         p = a + (b-a)/2
         print *, a, b, p, vol_a, vol_p
         vol_p = calculate_vol(p)
         if ((vol_p == 0.0 ) .or. ( ((b-a)/2.) .le. tol) ) then
             print *, p
             EXIT
         else
             i=i+1
             if( (vol_a*vol_p)>0 ) then
                      a=p
                      vol_a=vol_p
             else
                  b=p
             end if
         end if
      end do         
      END PROGRAM

      REAL FUNCTION calculate_vol(height)
      REAL*4 calc1, calc2, calc3, pi, L
      REAL*4 height
      L = 10.0
      !1 is is a constant radius so I hard coded it
      pi=acos(-1.0) 
      calc1 = (0.5) * (pi) * ((1.0)**2)
      calc2 = ((1.0)**2) * asin(height/1.0)
      calc3 = height * sqrt((1**2)-(height**2))
      calculate_vol = L * (calc1-calc2-calc3) - 12.4
      RETURN
      END

  
