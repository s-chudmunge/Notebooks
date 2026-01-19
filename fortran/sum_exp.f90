program ComputeExpMinusX
    implicit none
    real(8) :: x, term, exp_minus_x, series_sum
    integer :: n, max_terms

    ! Set the value of x
    write(*,*) "Enter the value of x:"
    read(*,*) x

    ! Set the maximum number of terms to ensure error below 10^(-7)
    max_terms = 1000000

    ! Initialize series sum
    series_sum = 1.0d0

    ! Compute the series sum
    do n = 1, max_terms
        term = (-x)**n / real(product(1,n), kind=8)
        series_sum = series_sum + term

        ! Check if the error is below 10^(-7)
        if (abs(term) < 1.0d-7) then
            exit
        end if
    end do

    ! Compute exp(-x)
    exp_minus_x = 1.0d0 / series_sum

    ! Print the result
    write(*,*) "exp(-", x, ") =", exp_minus_x

contains

    ! Function to compute the product of integers from 1 to n
    recursive function product(n) result(prod)
        integer, intent(in) :: n
        integer(8) :: prod

        if (n == 0) then
            prod = 1
        else
            prod = n * product(n - 1)
        end if
    end function product

end program ComputeExpMinusX

