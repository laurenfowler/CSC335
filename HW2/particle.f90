!Section 21 #16 

      PROGRAM particle
      integer max_iter, i
      real *4 a, b, p, FA, FP, tol 
      max_iter = 100
      a = -10.0
      b= 10.0
      i = 0
      FA = calculate(a)
      tol = 10.0 ** (-5)

      do while (i < max_iter)
         p = a + (b-a)/2
         FP = calculate(p)
         print *, a, b, p, FA, FP         
         if ( (FP == 0.0 ) .or. ( ((b-a)/2.0) < tol ) ) then
             print *, p
             exit
         else
             i = i + 1
             if((FA * FP) > 0) then
                 a = p 
                 FA = FP
             else
                 b = p
             end if
         end if
      end do  
    
      END PROGRAM

      REAL FUNCTION calculate(w)
      real *4 g, t, x, calc1, calc2, calc3
      g = 32.17
      t = 1.0
      x = 1.7
      calc1 = (g/(2*(w**2)))
      calc2 = (exp(w*t) - exp(-(w*t)))/2
      calc3 = sin(w*t)
      calculate = -(calc1) * (calc2 - calc3) - x
      return 
      END        
