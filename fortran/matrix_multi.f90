program MatrixMultiplicationExample
    implicit none
    real(8) :: A(3, 2), B(2, 4), C(3, 4)
    integer :: n, m, l, i, j, k

    n = 3
    m = 2
    l = 4

    A = reshape([1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0, 6.0d0], [3, 2])
    B = reshape([7.0d0, 8.0d0, 9.0d0, 10.0d0, 11.0d0, 12.0d0, 13.0d0, 14.0d0], [2, 4])

    write(*,*) "Matrix A ="
    do i = 1, n
        write(*,*) A(i, :)
    end do

    write(*,*) "Matrix B ="
    do i = 1, m
        write(*,*) B(i, :)
    end do

    do i = 1, n
        do j = 1, l
            C(i, j) = 0.0d0
            do k = 1, m
                C(i, j) = C(i, j) + A(i, k) * B(k, j)
            end do
        end do
    end do

    write(*,*) "Matrix C ="
    do i = 1, n
        write(*,*) C(i, :)
    end do

end program MatrixMultiplicationExample

