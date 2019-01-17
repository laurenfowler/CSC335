!Single Precision Fibonacci
!Lauren Fowler
!9-3-2017
        
      PROGRAM single_precision
        CHARACTER (len=4) :: arg
        INTEGER :: i, x
        real(4) :: five, one, two, half, A, B, fib_num
        
        !get command line input
        DO i=1, iargc()
          CALL getarg(i, arg)  
        END DO

        !assign iteration
        read(arg, '(i10)'), x      

        !Calculate Fibonacci Sequence
        five = 5.0
        two = 2.0
        half = 0.5
        one = 1.0

        !WRITE (*,*) (one- ((1+(5**half))/two))**x

        A = ((one+(five**(half)))/two)**x 
        B = ((one-(five**(half)))/two)**x
       
        fib_num = (one/(five**(half)))*(A-B)
        WRITE(*,*) fib_num          

      END PROGRAM 
