      !Problem 19, Section 3.1
      program population
      use interpolate
        integer i, num_pts
        double precision :: ans
        character (len=30) :: file_name, X
        real (kind=8) :: x_find
        real (kind=8), dimension(1524) :: xpt, ypt
       
        call getarg(1, file_name)
        call getarg(2, X)
        
        read(X,*) x_find
        call read_file(file_name, xpt, ypt, num_pts)
        !I think it is making one extra point
        num_pts = num_pts-1 
        ans = interp(num_pts, xpt, ypt, x_find)

        print *, ans
      end program

   
      !reads file in and returns xpt ypt arrays 
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

!       do i=1, cntr
!          print *, xpt(i), ypt(i)
!       enddo         
      end subroutine
