!Section 2.2 #6

      program converge
      integer i, max_iter
      real *4 p, p0, tol
      p0 = 1.0
      max_iter = 50
      tol = .001

      i = 1
      ! part a
      print *, "part a"
      do while (i < max_iter)
          p = ga(p0)
          !print *,  p0, p
          if ((abs(p-p0)) < tol) then
              print *, "converges at ", p
              exit
          else
              i = i + 1
              p0=p
          end if      
      end do
       
      if ( i == max_iter) then
          print *, "failed after ", max_iter, "iterations" 
      end if
     
      ! part b
      print *, "part b"
      i = 1
      p0 = 1.0
      do while (i < max_iter)
          p = gb(p0)
          !print *,  p0, p
          if ((abs(p-p0)) < tol) then
              print *,"converges at " ,  p
              exit
          else
              i = i + 1
              p0=p
          end if      
      end do

      if ( i == max_iter) then
          print *, "failed after ", max_iter, "iterations" 
      end if     


      ! part c
      print *, "part c" 
      i = 1
      p0 = 1.0
      do while (i < max_iter)
          p = gc(p0)
          !print *,  p0, p
          if ((abs(p-p0)) < tol) then
              print *, "converges at ", p
              exit
          else
              i = i + 1
              p0=p
          end if      
      end do

      if ( i == max_iter) then
          print *, "failed after ", max_iter, "iterations" 
      end if         

      ! part d
      max_iter = 500
      print *, "part d"     
      i = 1
      p0 = 1.0
      do while (i < max_iter)
          p = gd(p0)
        !  print *,  p0, p
          if ((abs(p-p0)) < tol) then
              print *, "converges at ", p, "after ", i, " iterations"
              exit
          else
              i = i + 1
              p0=p
          end if      
      end do

      if ( i == max_iter) then
          print *, "failed after", max_iter, "iterations" 
      end if

      end program   
  
      real function ga(x)
      real *4 x, calc1, calc2
      calc1 = 7 - ((x)**5)
      calc2 = 1 + (calc1/((x)**2))
      ga = x * ((calc2)**3)
      return
      end 

      real function gb(x)
      real *4 x, calc1, calc2
      calc1 = (x**5) - 7
      calc2 = calc1/(x**2)
      gb = x - calc2
      return
      end

      real function gc(x)
      real *4 x, calc1, calc2
      calc1 = ((x**5) - 7)
      calc2 = calc1/(5 * (x**4))
      gc = x - calc2
      return 
      end

      real function gd(x)
      real *4 x, calc1, calc2
      calc1 = ((x**5) - 7)
      calc2 = calc1/12.0
      gd = x - calc2
      return
      end

 
