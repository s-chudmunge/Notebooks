program matrix_rot
        implicit none
        real::alp,bet,gam,alp_deg,bet_deg,gam_deg
        real(8)::R(3,3),v_prime(3,1),v(3,1)
        real::pi
        integer::i,j
        pi = 4.0d0*datan(1.0d0)
        print*,'enter values of alpha,beta and gamma'
        read(*,*)alp,bet,gam
        print*,'enter the matrix v'
        read(*,*)v
        call rot(alp,bet,gam,R)
        v_prime = matmul(R,v)
        print*,v,v_prime 
        
        
        
        ! do i = 1, 3
           !       do j = 1, 3
          !               write(*, *) R(i, j)
         !        end do
        ! end do
       ! v_prime = matmul(R,v)
         !         do j = 1, 3
       !                  write(*, *) v(j, 1)
        !         end do
         


        
        end program

        subroutine rot(alp,bet,gam,R)
                implicit none
                real(8)::R_x(3,3),R_y(3,3),R_z(3,3),R(3,3),R_r(3,3)
                real::alp,bet,gam
                R_x = reshape([1.0,0.0,0.0,0.0,cos(bet),-sin(bet),0.0,sin(bet),cos(bet)],[3,3])
                R_y = reshape([cos(gam),-sin(gam),0.0,sin(gam),cos(gam),0.0,0.0,0.0,1.0],[3,3])
                R_z = reshape([cos(alp),-sin(alp),0.0,sin(alp),cos(alp),0.0,0.0,0.0,1.0],[3,3])
                R_r = matmul(R_y,R_x)
                R   = matmul(R_r,R_z)
                end subroutine  
