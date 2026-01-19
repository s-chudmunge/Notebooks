program diff
	implicit none
	integer, parameter :: N = 50
	integer :: i,nt,it,nsave
	real(8), dimension(0:N) :: phi
	real(8), dimension(N-1) :: rhs
	real(8) :: D, dx,dt,lam,l,tmax,s
	!
	D = 1.5d0
	l = 1.0d0
        s = 6.0d0
	dx = l/dble(N)
	lam = 0.5
	dt = lam*dx**2/D
	tmax = 1.0d0
	nt = nint(tmax/dt)
	nsave = nt/10 
	print*,'dx = ',dx
	print*,'dt = ',dt
	print*,'nt = ',nt
	! BC
	phi(0) = 0.0d0
	phi(N) = 0.0d0
	! initial condition
	phi(1:N-1) = 100.0d0
	!
	open(unit=1,file='temp.dat',status='unknown')
	write(1,*)(phi(i),i=0,N)
	do it = 1,nt
		call rhs_diffu(D,s,dx,N,phi,rhs)
		do i = 1,N-1
			phi(i)  = phi(i) + dt*rhs(i)	
		enddo
		if (mod(it,nsave) == 0) then
			write(1,*)(phi(i),i=0,N)
		endif
	enddo
	close(1)
end program diff
!
subroutine rhs_diffu(D,s,dx,N,phi,rhs)
	implicit none
	real(8), intent(in) :: D,dx
	integer, intent(in) :: N 
	real(8), dimension(0:N), intent(in) :: phi
	real(8), dimension(N-1), intent(out) :: rhs
	integer :: i
	real(8) :: fct,s
	fct = D/dx**2
	do i = 1,N-1
		rhs(i) = fct*(phi(i+1)-2.0d0*phi(i)+phi(i-1))+s*phi(i)*(1-phi(i))
	enddo
end subroutine rhs_diffu
