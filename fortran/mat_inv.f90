program inverse_matrix
	implicit none
	! Parameters
	integer, parameter :: n = 3 ! Size of the matrix
	integer :: info, lwork
	! Arrays
	real(kind=8),dimension(n,n) ::  A,Atmp      ! Input matrix
	real(kind=8), allocatable :: work(:) ! Workspace array
	integer :: ipiv(n)           ! Pivot array
	integer :: i, j
	lwork = n*n 
	allocate(work(lwork))
	! Initialize the input matrix A
	A = reshape((/ 1.0, 2.0, 3.0, &
								 0.0, 1.0, 4.0, &
								 5.0, 6.0, 0.0 /), (/n, n/))
	Atmp = A
	! Call LAPACK routine dgetrf to factorize A into LU decomposition
	call dgetrf(n, n, A, n, ipiv, info)
	
	if (info /= 0) then
			print *, "Error: LU factorization failed!"
			stop
	endif
	
	! Call LAPACK routine dgetri to compute the inverse of the matrix
	call dgetri(n, A, n, ipiv, work, lwork, info)
	
	if (info /= 0) then
			print *, "Error: Inverse computation failed!"
			stop
	endif
	
	! Print the inverse matrix
	print *, "Inverse Matrix:"
	call print_mat(n,n,A)
	print*, "A times A inverse"
	call print_mat(n,n,matmul(A,Atmp))
	!
	! Deallocate the workspace array
	deallocate(work)
end program inverse_matrix
!
subroutine print_mat(n,m,A)
	implicit none
	integer, intent(in) :: n,m
	real(8),dimension(n,m), intent(in) :: A
	integer :: i,j
  do i = 1, n
			print*, (A(i, j), j = 1, m)
	end do
	
end subroutine print_mat
!
