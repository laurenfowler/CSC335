        program simpsons
        integer :: n
        real (kind=4) :: x, a, b, sum1, sum2, h, ans

            n = 8
            a = 0.0
            b = (3*3.14)/8.0
            h = (b-a)/8.0

            sum1=0.0
            sum2=0.0

            do i=1, n-1
                x = a + i*h
                if(mod(i,2)==0) then
                    sum2 = sum2 + func2(x)
                else
                    sum1 = sum1 + func2(x)
                end if
            end do

            ans = (h/3.0)*(func2(a)+(2*sum2)+(4*sum1)+func2(b))
            print *, ans

        end program 

        real *4 function func(x)

            func = 2.0/((x**2)+4.0)

        return
        end function

        real*4 function func2(x)

            func2 = tan(x)        

        return
        end function
