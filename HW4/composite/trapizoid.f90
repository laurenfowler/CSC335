        program trapezoid
        integer :: n
        real(kind=4) :: a, b, h, x, sum, ans      

        a = 0.0
        b=(3*3.14)/8.0
        n=8
        h = (b-a)/8.0

        sum = 0
        do j=1, n-1
            x = a + j*h
            sum = sum + func2(x)            
        end do

        ans = (h/2.0)*(func2(a) + 2.0*sum + func2(b))
        print *, ans

        end program

        real *4 function func(x)

            func = 2.0/((x**2)+4)

        return
        end function func

        real*4 function func2(x)

            func2 = tan(x)

        return
        end function func2



