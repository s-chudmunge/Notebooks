program simpsons
    implicit none
    integer :: a, b, n, i
    real :: h, integral, f_out, sum_even, sum_odd
    real, dimension(1000) :: array

    n = 99
    do i = 1, n
        call funx(real(i), f_out)
        array(i) = f_out
    end do

    a = 1
    b = 3
    h = (b - a) / real(n)

    sum_even = 0.0
    sum_odd = 0.0

    do i = 2, n-1, 2
        sum_even = sum_even + 4.0 * array(i)
    end do

    do i = 3, n-2, 2
        sum_odd = sum_odd + 2.0 * array(i)
    end do

    integral = h / 3.0 * (array(1) + sum_even + sum_odd + array(n))
    print*, integral

contains

    subroutine funx(x, f_out)
        implicit none
        real, intent(in) :: x
        real, intent(out) :: f_out
        f_out = x**6 - x**2 * sin(2 * x)
    end subroutine funx

end program simpsons


