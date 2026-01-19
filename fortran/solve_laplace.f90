program laplas_2d
	implicit none
	integer :: i,j,n
	integer :: ok
	real(8) :: h,lx,ly
	real(8) :: maxdiff,tol 
	real(8), allocatable, dimension(:,:) :: phi,phiold,E
	tol = 1e-5
	lx = 1.0d0
	ly = 1.0d0
	n = 100
	h = lx/dble(n)
	allocate(phi(0:n,0:n),E(0:n,0:n), stat = ok)
	if (ok /= 0) stop "Error: could not allocate phi"
	allocate(phiold(0:n,0:n), stat = ok)
	if (ok /= 0) stop "Error: could not allocate phiold"
	!
	phi = 0.0d0
	! Applying boundary conditions
	phi(0,:) = 0.0d0
	phi(n,:) = 0.0d0
	phi(:,0) = 0.0d0
	phi(:,n) = 100.0d0
	!
	do
		phiold = phi
		call laplas(n,phi)
		maxdiff = maxval(abs(phi-phiold))
		if (maxdiff < tol) exit
	enddo

        call Electric_field(n,phi,E)



	!
	! write phi to a file
	open(unit=1,file='phi.dat',status='unknown')
	do j = 1,n-1
		write(1,*) (phi(i,j),i=1,n-1)
	enddo
	close(1)
	!

        ! write E to a file
        open(unit=1,file='E.dat',status='unknown')
        do j = 1,n-1
                write(1,*) (E(i,j),i=1,n-1)
        enddo
        close(1)
        !

end program laplas_2d

subroutine laplas(n,phi)
	implicit none
	integer, intent(in) :: n
	real(8),dimension(0:n,0:n), intent(inout) :: phi
	integer :: i,j
	
		do j = 1,n - 1
			do i = 1,n - 1
				phi(i,j) = 0.25d0*(phi(i+1,j)+phi(i-1,j) &
													+phi(i,j+1)+phi(i,j-1))
			enddo
		enddo
end subroutine laplas

subroutine Electric_field(n,phi,E)
        implicit none
        integer , intent(in)::n
        real(8),dimension(0:n,0:n), intent(inout) :: phi,E
        real(8)::h
        real(8),dimension(0:n,0:n) :: E_x,E_y
        integer :: i,j
        h=0.001d0
        
                do j = 1,n - 2
                        do i = 1,n - 2
                                E_x(i,j) =(0.5d0*(phi(i+1,j))-phi(i-1,j))/h
                                E_y(i,j) =(0.5d0*(phi(i,j+1))-phi(i,j-1))/h
                enddo
                enddo
                do j = 1,n-2
                do i=1,n-2
                  E(i,j)=sqrt((E_x(i,j)**2)+(E_y(i,j)**2))
                  end do
                  end do
end subroutine Electric_field
