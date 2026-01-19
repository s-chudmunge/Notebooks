program heat_equation_modified
    implicit none
    real(8), allocatable, dimension(:,:) :: phi
    integer :: n, m, i, j
    real(8) :: L, T_max, dx, dt, D, lambda

    L = 1.0d0   ! length
    T_max = 11.0d0 ! max time
    D = 0.5d0  !
    dx = 1.0d0 / real(50,8) ! position step size
    lambda = 0.5
    dt = lambda * dx**2 / D
    n = nint(T_max / dt)
    m = nint(L / dx)
    print*, m
    allocate(phi(0:m+1,0:n+1))

    ! Initialize temperature
    phi = 1.0d0

    ! Time integration loop
    do j = 1, n
        phi(0,j) = phi(0,j-1) + lambda * (phi(1,j-1) - phi(0,j-1))
        phi(m+1,j) = phi(m+1,j-1) + lambda * (phi(m,j-1) - phi(m+1,j-1))
        do i = 1, m
            phi(i,j) = phi(i,j-1) + lambda * (phi(i+1,j-1) + phi(i-1,j-1) - 2.0d0 * phi(i,j-1))
        end do
    end do

    ! Write results to file
    open(unit=6, file='phi_modified.dat', status='unknown', action='write')
    do j = 0, n+1
        if ((j*dt.eq.0).or.(j*dt.eq.0.1).or.(j*dt.eq.0.5).or.(j*dt.eq.1).or.(j*dt.eq.2).or.(j*dt.eq.10)) then
            write(6, *)(phi(i,j), i=0,m+1)
        endif
    end do
    close(6)

    deallocate(phi)
end program heat_equation_modified

