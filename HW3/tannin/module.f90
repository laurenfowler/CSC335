      module interpolate
      implicit none

      contains
        real function interp ( N, xpt, ypt, x )
    
            double precision xpt(0:*), ypt(0:*)
            real (kind=8) :: sum, prod
            double precision :: x
            integer N, i, k            
   
!            print *, N, x
 
            sum = 0.0D0
            do 10 k = 0, N
                prod = 1.0D0
                do 20 i = 0, N
                    if (i .ne. k) then 
                        prod = prod * (x-xpt(i))/(xpt(k)-(xpt(i)))
                    end if
20              continue
                sum = sum + ypt(k)*prod
                !print *, sum
10          continue

            interp = sum
            return
        end function
      

      end module interpolate

