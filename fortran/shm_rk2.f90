program shm_rk2
	implicit none
	integer :: it,nt 
	real(8) :: k,m,x0,v0
	real(8) :: t,dt,tmax
	real(8), dimension(2) :: y
	!
	k = 
	m = 
	x0 = 
	v0 = 
	dt = 
	tmax = 
	nt = nint(tmax/dt)
	y(1) = x0
	y(2) = v0
	!
	open(unit=1,file='shm_rk2_dt1e-1.dat',status='unknown')
	t = 0.0d0
	write(1,*)t,y(1),y(2)
	do it = 1,nt
		call rk2(dt,k,m,y)
		t = t + dt
		write(1,*)t,y(1),y(2)
	enddo
	close(1)
end program shm_rk2
!
subroutine rk2(dt,k,m,y)
	implicit none
	real(8) ,intent(in) :: dt,k,m
	real(8), dimension(2), intent(inout) :: y 
	real(8), dimension(2) :: k1,k2 
	!
	call dydt_shm(k,m,y,k1)
	call dydt_shm(k,m,y+0.5d0*dt*k1,k2)
	y = y + dt*k2
	!
end subroutine rk2
!
subroutine dydt_shm(k,m,y,dydt)
	implicit none
	real(8), intent(in) :: k,m
	real(8),dimension(2), intent(in) :: y
	real(8),dimension(2), intent(out) :: dydt
	!
	dydt(1) = y(2)
	dydt(2) = -k/m*y(1)
	!
end subroutine dydt_shm
