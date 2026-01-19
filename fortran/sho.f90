program sho_new

        implicit none
        real(8)::rhs1,E,rhs2,del_t,x,v,t
        integer::i,n
        del_t=0.1
        x=1
        v=0
        t=0
        n=10000
        open(20,file="result_Euler.out")
        
        do i=1,n
        call get_rhs(x,v,rhs1,rhs2)
        x=x+del_t*rhs1
        v=v+del_t*rhs2
        t=t+del_t
        E=0.5*20*(x**2)+0.5*(v**2)
        write(20,"(4e16.6e3)")x,v,t,E
        if (t>=2.0) then 
                exit
        end if
        end do
        close(20)
        
        end program

        subroutine get_rhs(x,v,rhs1,rhs2)
                real(8),intent(in)::x,v
                real(8),intent(out)::rhs1,rhs2
                rhs1=v
                rhs2=-20*x
                end 

