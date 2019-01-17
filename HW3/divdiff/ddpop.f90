      !Problem 17, Section 3.3
      program ddpop
      use interpolate

        parameter (MAXPTS = 1000)
        parameter (MAXVAL = 10000)

        integer i, num_pts
        double precision :: ans
        character (len=30) :: file_name, x
        real (kind=8) :: x_find
        double precision :: xpt(0:MAXPTS), ypt(0:MAXPTS)
        real(kind=8) F(0:MAXPTS, 0:MAXPTS)
        
        call getarg(1, file_name)
        call getarg(2, x)
        
        read(x,*) x_find
        call read_file(file_name, xpt, ypt, num_pts)
        !I think it is making one extra point
        num_pts = num_pts-1

        print *, "fire"
        !Populate F Matrix 
        call divdif(xpt, F, MAXPTS, n)

        print *, num_pts
        !compute interpolated values
        ans = interp(num_pts, xpt, ypt, F, MAXPTS,  x_find)
        print *, "hole"
    
        print *, ans
      end program

   
      !reads file in and returns xpt ypt arrays 
      subroutine read_file(file_name, xpt, ypt, num_pts)
        double precision :: xpt(0:*), ypt(0:*)
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

!       do i=1, cntr
!          print *, xpt(i), ypt(i)
!       enddo         
      end subroutine
