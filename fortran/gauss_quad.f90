program gauss
        implicit none
        integer::i
        real::intg,f
        real,dimension(10)::weight,point
        open(10,file='gauss.dat')
        do i=1,10
        read(10,*)weight(i),point(i)
        end do
        close(10)
        intg=0.
        do i=1,10
        intg=intg+weight(i)*f(2.5+0.5*point(i))
        end do
        intg=0.5*intg
        print*,intg      
        end program gauss

real function f(x)
        real::x
        f=x**2
        end function


