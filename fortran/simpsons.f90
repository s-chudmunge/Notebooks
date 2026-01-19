program simpsons
        implicit none
        integer::a,b,n,i
        real::h,integral,f_out
        n=99
        real::array(1000)
        do i=1,n
        array(i)=call funx(i,f_out)
        end do
        print*,array
        a=1
        b=3
        h=b-a/n


       
!-------------------------------------------------------------------------------------
        subroutine funx(x,f_out)
                implicit none
                real,intent(in)::x
                real,intent(out)::f_out,integral
                integer::n
                f_out=x**6 - x**2*sin(2*x)  
                end subroutine funx
!----------------------------------------------------------------------------------------
end program 
