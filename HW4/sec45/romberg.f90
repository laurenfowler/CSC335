        program romberg
        integer :: n
        real (kind=4) :: a, b, h, ans, sum, tol
        real (kind=4), allocatable :: R(:,:)

        tol = 10.0**(-6)

        a = 0.0
        b = 0.35
        n = 10
        allocate (R(1:10,1:10))

        h = b-a
        R(1,1) = (h/2.0) * (fa(a) + fa(b))
        print *, R(1,1)
       
        do i=2, n
            R(i, 1)=(1.0/2.0)*(R(i-1,1)+(h*summation(i,h,a)))
            do j=2, n
                R(i, j)=R(i, j-1)+ (R(i,j-1)-R(i-1,j-1))/((4**(j-1))-1)
            end do
            print *, R(i, 1:i)
            h = h/2.0

            if(abs(R(i-1, i-1)-R(i,i)) < tol)then
                exit
            end if
        end do
        
        end program

        real *4 function fa(x)

            fa = 2.0/((x**2)-4)

        return
        end function

        real *4 function fb(x)
            
            fb = exp(3.0*x)*sin(2.0*x)

        return
        end function

        real *4 function summation(i, h, a)
        real (kind=4) :: sum
        integer :: upper_index
   
            upper_index = 2**(i-2)
            sum = 0
            do k=1, upper_index
                sum = sum + fa(a + (k-0.5)*h)
            end do

        return
        end function
