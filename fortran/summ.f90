program summation
      implicit none
        real,allocatable,dimension(:)::x,y
        real::sum_x,sum_y,sum_xy,sum_y2
        integer::n
      open(10,file='data_xy.dat')
      allocate
      read(*,*)x,y

      close(10)
      print*,x,y
      n=size(x)
      call summ(x,y,n,sum_x,sum_y,sum_xy,sum_y2)
      print*,sum_x,sum_y,sum_xy,sum_y2

end program 

subroutine summ(x,y,n,sum_x,sum_y,sum_xy,sum_y2)
      real,dimension(10),intent(in)::x,y
      real,intent(out)::sum_x,sum_y,sum_xy,sum_y2
      integer::n
      do i=1,n
      sum_x=sum_x+x(i)
      sum_y=sum_y+y(i)
      sum_xy=sum_xy+x(i)*y(i)
      sum_y2=sum_y2+y(i)**2
      end do
      end subroutine


