program rk2
    implicit none
    real(8) :: rhs1, rhs2, del_t
    real(8), dimension(:), allocatable :: x, y, t
    integer :: i, n
    del_t = 0.01
    n = 3000
    allocate(x(n), y(n), t(n))
    open(20, file='output_assgn_3.txt')
    x(1) = 2.0d0
    y(1) = 1.0d0
    t(1) = 0.0d0
    do i = 1, n
        if (t(i) >= 30.0d0) exit
        call rk2_step(x(i), y(i), del_t, x(i+1), y(i+1))
        t(i+1) = t(i) + del_t
    end do
    do i = 1, n
        write(20, *) x(i), y(i), t(i)
    end do
    deallocate(x, y, t)
end program rk2

subroutine rk2_step(x, y, del_t, x_new, y_new)
    real(8), intent(in) :: x, y, del_t
    real(8), intent(out) :: x_new, y_new
    real(8) :: k1_x, k1_y, k2_x, k2_y
    k1_x = del_t * (1.2d0 * x - 0.6d0 * x * y)
    k1_y = del_t * (-0.8d0 * y + 0.3d0 * x * y)
    k2_x = del_t * (1.2d0 * (x + 0.5d0 * k1_x) - 0.6d0 * (x + 0.5d0 * k1_x) * (y + 0.5d0 * k1_y))
    k2_y = del_t * (-0.8d0 * (y + 0.5d0 * k1_y) + 0.3d0 * (x + 0.5d0 * k1_x) * (y + 0.5d0 * k1_y))
    x_new = x + k2_x
    y_new = y + k2_y
end subroutine rk2_step

