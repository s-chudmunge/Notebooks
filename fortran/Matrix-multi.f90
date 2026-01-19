program matrix_multiplication 
implicit none
integer::n,m,l
real,allocatable,dimension(:,:)::a,b,c 


write(*,*)"Enter the dimension of first matrix"
read(*,*)n,m
write(*,*)"Enter the dimension of second matrix"
read(*,*)m,l
allocate(a(n,m))
allocate(b(m,l))
allocate(c(n,l))
write(*,*)"Enter matrix A"
read(*,*)a
write(*,*)"Enter matrix B"
read(*,*)b

call matrix_multi(n,m,l,a,b,c)

write(*,*)"The multiplication of two prorgam is",c

deallocate(a)
deallocate(b)
deallocate(c)


end program 
subroutine matrix_multi(n,m,l,a,b,c)
        implicit none
        integer,intent(in)::n,m,l
        real,dimension(n,m)::a
        real,dimension(m,l)::b
        real,dimension(n,l)::c
        integer::i,j,k
        do j=1,l
        do i=1,n
        do k=1,m
        c(i,j)=c(i,j)+a(i,k)*b(k,j)
        end do 
        end do 
        end do 
end subroutine matrix_multi
