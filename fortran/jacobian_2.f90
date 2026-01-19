program compute_jacobian
    implicit none
    real(8) :: x1, x2, x3
    integer::i,j
    real(8), dimension(3,3) :: F,Ftemp
    integer, parameter :: n = 3 ! Size of the matrix
	integer :: info, lwork
	! Arrays
	real(kind=8), allocatable :: work(:) ! Workspace array
	integer :: ipiv(n)          
	lwork = n*n 
        x1 = 2
    x2 = 0.5
    x3 = 1.0
    call delf_delx(x1, x2, x3, F)
    call print_mat(3,3,F)
    contains

    subroutine delf_delx(x1, x2, x3, F)
        implicit none
        real(8), intent(in) :: x1, x2, x3
        real(8), intent(out), dimension(3,3) :: F
        real(8) :: h
        real(8) :: f1_x1, f1_x2, f1_x3
        real(8) :: f2_x1, f2_x2, f2_x3
        real(8) :: f3_x1, f3_x2, f3_x3

        h = 0.01
        f1_x1 = (f1(x1 + h, x2, x3) - f1(x1 - h, x2, x3)) / (2 * h)
        f1_x2 = (f1(x1, x2 + h, x3) - f1(x1, x2 - h, x3)) / (2 * h)
        f1_x3 = (f1(x1, x2, x3 + h) - f1(x1, x2, x3 - h)) / (2 * h)
        f2_x1 = (f2(x1 + h, x2, x3) - f2(x1 - h, x2, x3)) / (2 * h)
        f2_x2 = (f2(x1, x2 + h, x3) - f2(x1, x2 - h, x3)) / (2 * h)
        f2_x3 = (f2(x1, x2, x3 + h) - f2(x1, x2, x3 - h)) / (2 * h)
        f3_x1 = (f3(x1 + h, x2, x3) - f3(x1 - h, x2, x3)) / (2 * h)
        f3_x2 = (f3(x1, x2 + h, x3) - f3(x1, x2 - h, x3)) / (2 * h)
        f3_x3 = (f3(x1, x2, x3 + h) - f3(x1, x2, x3 - h)) / (2 * h)

        F(1, 1) = f1_x1
        F(1, 2) = f1_x2
        F(1, 3) = f1_x3
        F(2, 1) = f2_x1
        F(2, 2) = f2_x2
        F(2, 3) = f2_x3
        F(3, 1) = f3_x1
        F(3, 2) = f3_x2
        F(3, 3) = f3_x3

    end subroutine

    function f1(x1, x2, x3)
        implicit none
        real(8), intent(in) :: x1, x2, x3
        real(8) :: f1
        f1 = x1 + x2 + x3 - 6
    end function f1

    function f2(x1, x2, x3)
        implicit none
        real(8), intent(in) :: x1, x2, x3
        real(8) :: f2
        f2 = x1 * x2 + x2 * x3 + x3 * x1 - 11
    end function f2

    function f3(x1, x2, x3)
        implicit none
        real(8), intent(in) :: x1, x2, x3
        real(8) :: f3
        f3 = x1 * x2 * x3 - 36
    end function f3

subroutine print_mat(n,m,A)
	implicit none
	integer, intent(in) :: n,m
	real(8),dimension(n,m), intent(in) :: A
	integer :: i,j
  do i = 1, n
			print*, (A(i, j), j = 1, m)
	end do
	
end subroutine print_mat

    end program
