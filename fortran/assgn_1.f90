program summation
    implicit none
    real, dimension(:), allocatable ::sum_up,sum_down
    real::S_up,S_down
    integer ::  N,m,j
    m=1500
    allocate(sum_up(n),sum_down(n))
    open(20,file='single_preci.txt')
    do i=1,m
    do N=1000,1000000,1000
    sum_up(j)=S_up(N)
    sum_down(j)=S_down(N)
    end do
    end do
    do j = 1, m
        write(20, *) sum_up(j), sum_down(j)
    end do
    close(20)
    deallocate(sum_up,sum_down)
contains 

 real(kind=4) function S_up(S)
        integer,intent(in) :: S,i
        S_up = 0.0
        do i =1, S
            S_up = S_up + 1.0 / real(i, kind=4)
        end do
    end function S_up

    real(kind=4) function S_down(S)
        integer,intent(in) :: S,i
        S_down = 0.0
        do i =S,1,-1
            S_down = S_down + 1.0 / real(i, kind=4)
        end do
    end function S_down
    end program summation




