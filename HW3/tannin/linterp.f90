        program linterp
        use interpolate

            !define program constants
            parameter (MAXPTS = 1000)
            parameter (MAXVAL = 1000)

            !define program variables
            double precision xpt(0:MAXPTS), ypt(0:MAXPTS)
!            double precision iterp_y(0:MAXPTS)
            double precision x(0:MAXPTS)
            double precision xtmp, ytmp, xmin, xmax, delx
            character (len=30) :: file_name
            integer :: num_pts


            !get file
            call getarg(1, file_name)
            call read_file(file_name, xpt, ypt, num_pts)

            !find min and max
            xmin=xpt(0)
            xmax=xpt(0)
            do 10 i=1, num_pts
                xmin = min(xmin, xpt(i))
                xmax = max(xmax, xpt(i))
        10  continue

            !set values for domain of interpolating polynomial
            !is delx the step value??
            delx = (xmax-xmin)/dble(MAXVAL-1)
            x(1)=xmin
            do 20 i=2, MAXVAL
                x(i) = x(i-1) + delx
        20  continue


            !compute values for the interpolating polynomial
            do 30 i=1, MAXVAL
                !interp_y(i) = interp(num_pts, xpt, ypt, x(i))
                print *, x(i), interp(num_pts, xpt, ypt, x(i))
        30  continue

        end program linterp

        subroutine read_file(file_name, xpt, ypt, num_pts)
            real (kind=8), dimension(1524) :: xpt, ypt
            real (kind=8) ::  x, y
            integer :: errval, cntr, num_pts
            character (len=30) :: file_name

            open (unit=15, file=file_name, status="old")

            num_pts = 0
            errval = 0
            cntr = 0
            do while ( errval /= -1 )
                num_pts = num_pts + 1
                read ( 15, *, iostat=errval ) x, y
                if ( errval == 0 ) then
                    cntr = cntr + 1
                    xpt(cntr) = x
                    ypt(cntr) = y
                else
                    close(15)
                endif
            enddo
        end subroutine

            
!        double precision function interp ( N, xpt, ypt, x )
!
!          double precision xpt(0:*), ypt(0:*)
!         double precision sum, prod, x
!          integer N, i, k
!
!          sum = 0.0D0
!          do 10 k = 0, N
!              prod =1.0D0
!              do 20 i = 0, N
!                 if (i .ne. k) prod = prod * (x-xpt(i))/(xpt(k)-xpt(i))
!20            continue
!               sum = sum + ypt(k) * prod
               !print *, sum
!10        continue
!
!          interp = sum
!          return
!        end

