        program velocity
        integer :: n
        real (kind = 4) :: a, b, h, sum1, sum2, x, ans

        n = 300
        a = 5.0
        b = 10.0

        h = (b-a)/300.0
        sum1 = 0.0
        sum2 = 0.0

        do i=1, n-1
            x = a + i*h
            if(mod(i,2)==0) then
                sum2 = sum2 + f(x)
            else
                sum1 = sum1 + f(x)
            end if
        end do

        ans = (h/2.0)*(f(a)+(2.0*sum2)+(4.0*sum1)+f(b))
        print *, ans
        
        end program 

        real *4 function f(x)

        f = 10.0/(x*sqrt(x))

        end function f
