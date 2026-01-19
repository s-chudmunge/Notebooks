program main
	implicit none
	integer, parameter :: n = 2
	real(8)     :: a(n, n)  ! Matrix A.
	real(8)     :: b(n)     ! Vector b/x.
	integer     :: pivot(n) ! Pivot indices (list of swap operations).
	integer  :: rc       ! Return code.

	a = reshape((/ 2., 3., 1., 1. /), (/ n, n /))
	b = (/ 5., 6. /)
        print*,a
        print*,b
        print*,size(a)
        print*,size(b)

	call dgesv(n, 1, a, n, pivot, b, n, rc)

	if (rc /= 0) then
			print*, 'Error: ', rc
			stop
	end if

	print*,"Solution (x1, x2): ",  b
end program main
