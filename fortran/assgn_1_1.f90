program summation
    implicit none
    
    integer, parameter :: start_N = 1000
    integer, parameter :: end_N = 1000000
    integer, parameter :: step = 1000
    integer :: N
    real :: sum_up, sum_down
    
    open(unit=10, file='output.txt', status='replace')
    
    do N = start_N, end_N, step
        sum_up = calculate_sum_up(N)
        sum_down = calculate_sum_down(N)
        write(10, '(I10, 2E16.6)') N, sum_up, sum_down
    end do
    
    close(unit=10)
    
contains

    real function calculate_sum_up(N)
        implicit none
        integer, intent(in) :: N
        integer :: n
        
        calculate_sum_up = 0.0
        do n = 1, N
            calculate_sum_up = calculate_sum_up + 1.0_real / real(n, kind=selected_real_kind(6, 37))
        end do
    end function calculate_sum_up

    real function calculate_sum_down(N)
        implicit none
        integer, intent(in) :: N
        integer :: n
        
        calculate_sum_down = 0.0
        do n = N, 1, -1
            calculate_sum_down = calculate_sum_down + 1.0_real / real(n, kind=selected_real_kind(6, 37))
        end do
    end function calculate_sum_down

end program summation

