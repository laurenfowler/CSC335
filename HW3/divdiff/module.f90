      module interpolate
      implicit none

      contains
        real function interp ( N, xpt, ypt, F , MAXPTS, x )
            integer MAXPTS     
            real(kind=8), dimension(1524) :: xpt(0:*), ypt(0:*)
            real(kind=8) :: sum, prod, x
            real (kind=8) F(0:MAXPTS, 0:MAXPTS)
            integer N, i, j

            sum = 0.0D0
            do 10 i = 0, N
                prod = 1.0D0
                do 20 j = 0, i-1
                    prod = prod * (x-xpt(j))
20              continue
                sum = sum + F(i,i) * prod
                print *, sum
                !print *, sum
10          continue

            interp = sum
            return
        end function

        subroutine divdif (x, F, MAXPTS, n)
            integer :: i , j, MAXPTS, n
            double precision :: x(0:*)
            double precision :: F(0:MAXPTS, 0:MAXPTS)

            do 30 i=1, n
                do 40 j=1,i
                    F(j,i) = (F(j-1,i-1))/(x(i) - x(i-j))
                    print *, F(j,i)
40              continue 
30          continue
        end subroutine       
      
      end module interpolate

