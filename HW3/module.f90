      module interpolate
      implicit none

      contains
        real function interp ( N, xpt, ypt, x )
    
            real(kind=8), dimension(1524) :: xpt(0:*), ypt(0:*)
            real(kind=8) :: sum, prod, x
            integer N, i, k            
   
            print *, N, x
 
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

        real function divdif (x, F, MAXPTS, n)
            integer :: i , j, MAXPTS, n
            double precision :: x(0:*)
            double precision :: F(0:MAXPTS, 0:MAXPTS)

            do 30 i=1, n
                do 40 j=1,i
                    F(j,i) = (F(j-1,i-1))/(x(i) - x(i-j))
40              continue 
30          continue
        end function          
      
      end module interpolate

