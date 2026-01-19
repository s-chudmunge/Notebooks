program reccurence
        implicit none
        real::x_n_prev,r,x_n_plus
        integer::i
        x_n_prev=0.5
        do i=1,100
        x_n_plus=r*x_n_prev*(1-x_n_prev)
        print*,x_n_plus
        x_n_prev=x_n_plus
        end do
        
        
 end program
