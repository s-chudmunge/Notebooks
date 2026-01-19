program MatrixMultiplication
    implicit none
    real(8) :: A(2, 2), B(2, 2), C(2, 2)
    integer :: i, j, k

    A = reshape([1.0d0, 2.0d0, 3.0d0, 4.0d0], [2, 2])
    B = reshape([5.0d0, 6.0d0, 7.0d0, 8.0d0], [2, 2])

    write(*,*) "Matrix A ="
    do i = 1, 2
        write(*,*) A(i, :)
    end do

    write(*,*) "Matrix B ="
    do i = 1, 2
        write(*,*) B(i, :)
    end do

    do i = 1, 2
        do j = 1, 2
            C(i, j) = 0.0d0
            do k = 1, 2
                C(i, j) = C(i, j) + A(i, k) * B(k, j)
            end do
        end do
    end do

    write(*,*) "Matrix C ="
    do i = 1, 2
        write(*,*) C(i, :)
    end do

end program MatrixMultiplication

