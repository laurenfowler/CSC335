        program runge_kutta
        real(kind=4) :: a, b, init, h, t, w
        real(kind=4) :: K1, K2, K3, K4

        init = 8.0
        h = 20
        a = 0.0
        b = 600
        t = a
        w = init
        print *, t, w

        t = t + h
        do while(w>0)
            K1 = h* f(t,w)
            K2 = h* f(t+(h/2.0), w + (K1/2.0))
            K3 = h* f(t+(h/2.0), w + (K2/2.0))
            K4 = h* f(t+h, w + K3)

            w = w + (K1+2.0*K2+2.0*K3+K4)/6.0
            t = t + h
            print *, t, w
        end do

        end program

        real *4 function f(x, y)

        f=(-0.6*(.1**2))*sqrt(2.0*32.1)*(sqrt(y)/(y**2))   
   
        return
        end function
