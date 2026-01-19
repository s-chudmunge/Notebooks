program compute_jacobian

       implicit none
        real(8)::x1,x2,x3
        real(8),dimension(3,3)::F
        x1=2
        x2=0.5
        x3=1.0
        call delf_delx(x1,x2,x3,F)
        print*,F
       end program


                subroutine delf_delx(x1,x2,x3,F)
                        implicit none
                        real(8),intent(in)::x1,x2,x3
                        real(8),intent(out),dimension(3,3)::F
                        real(8)::delf1,delf2,delf3,delf4,delf5,delf6,delf7,delf8,delf9,h
                        h=0.01                       
                        delf1=(f1(x1+h,x2,x3)-2*f1(x1,x2,x3)+f1(x1-h,x2,x3))*10000
                        delf2=(f1(x1,x2+h,x3)-2*f1(x1,x2,x3)+f1(x1,x2-h,x3))*10000
                        delf3=(f1(x1,x2,x3+h)-2*f1(x1,x2,x3)+f1(x1,x2,x3-h))*10000
                         delf4=(f2(x1+h,x2,x3)-2*f2(x1,x2,x3)+f2(x1-h,x2,x3))*10000
                        delf5=(f2(x1,x2+h,x3)-2*f2(x1,x2,x3)+f2(x1,x2-h,x3))*10000
                        delf6=(f2(x1,x2,x3+h)-2*f2(x1,x2,x3)+f2(x1,x2,x3-h))*10000
                         delf7=(f3(x1+h,x2,x3)-2*f3(x1,x2,x3)+f3(x1-h,x2,x3))*10000
                        delf8=(f3(x1,x2+h,x3)-2*f3(x1,x2,x3)+f3(x1,x2-h,x3))*10000
                        delf9=(f3(x1,x2,x3+h)-2*f3(x1,x2,x3)+f3(x1,x2,x3-h))*10000
                        F(1,1)=delf1
                        F(1,2)=delf2
                        F(1,3)=delf3
                        F(2,1)=delf4
                        F(2,2)=delf5
                        F(2,3)=delf6
                        F(3,1)=delf7
                        F(3,2)=delf8
                        F(3,3)=delf9

                        end subroutine

           function f1(x1, x2, x3)
          real(8), intent(in) :: x1, x2, x3
          real(8) :: f1
          f1 = x1 + x2 + x3 - 6
          return
        end function f1

        function f2(x1, x2, x3)
          real(8), intent(in) :: x1, x2, x3
          real(8) :: f2
          f2 = x1 * x2 + x2 * x3 + x3 * x1 - 11
         return
        end function f2

        function f3(x1, x2, x3)
          real(8), intent(in) :: x1, x2, x3
          real(8):: f3
         f3 = x1 * x2 * x3 - 36
          return
        end function f3
