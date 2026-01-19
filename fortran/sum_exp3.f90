program ComputeExpMinusX
    implicit none
    real(8) :: x, term, exp_minus_x, series_sum
    integer :: n, max_terms
    write(*,*) "Enter the value of x:"
    read(*,*) x
    max_terms = 100
    series_sum = 1.0E0
    term = 1.0E0

    do n = 1, max_terms
        term = term * (-x / n)
        series_sum = series_sum + term

        if (abs(term) < 1.0E-7) then
            exit
        end if
    end do

    exp_minus_x = series_sum

    write(*,*) "exp(-", x, ") =", exp_minus_x

end program ComputeExpMinusX

