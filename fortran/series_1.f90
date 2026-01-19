program series_calculation
    implicit none
    integer, parameter :: start_N = 1000, end_N = 1000000, step = 1000
    integer :: N, i
    real(kind=4) :: S_up, S_down, diff
    real(kind=4), dimension((end_N - start_N) / step + 1) :: result_single
    real(kind=8) :: S_up_d, S_down_d, diff_d
    real(kind=8), dimension((end_N - start_N) / step + 1) :: result_double
    open(unit=10, file='output_single_precision.txt')
    open(unit=20, file='output_double_precision.txt')

    ! Single-precision calculation
    do N = start_N, end_N, step
        S_up = 0.0
        S_down = 0.0
        do i = 1, N
            S_up = S_up + 1.0 / real(i, kind=4)
        end do
        do i = N, 1, -1
            S_down = S_down + 1.0 / real(i, kind=4)
        end do
        diff = abs(S_up - S_down) / (S_up + S_down)
        result_single((N - start_N) / step + 1) = diff
        write(10, *) N, diff
    end do

    ! Double-precision calculation
    do N = start_N, end_N, step
        S_up_d = 0.0d0
        S_down_d = 0.0d0
        do i = 1, N
            S_up_d = S_up_d + 1.0d0 / real(i, kind=8)
        end do
        do i = N, 1, -1
            S_down_d = S_down_d + 1.0d0 / real(i, kind=8)
        end do
        diff_d = abs(S_up_d - S_down_d) / (S_up_d + S_down_d)
        result_double((N - start_N) / step + 1) = diff_d
        write(20, *) N, diff_d
    end do

    close(10)
    close(20)


end program series_calculation

