program q2
        implicit none
        real(kind = 8) :: x_new, x_old, tol, fun, fun_
        tol = 1e-7
        print*,'give the guess value'
        read*,x_old
        do while (fun(x_new) > tol)
        x_new = x_old - (fun(x_old)/fun_(x_old))
        x_old = x_new
        end do
        print*,'The root is = ',x_new
end program q2

real(kind = 8) function fun(x)
        real(kind = 8) :: x
        fun = sqrt(100.0-x)*tan(sqrt(100.0-x))-sqrt(x)
end function

real(kind = 8) function fun_(x)
        real(kind = 8) :: x
        fun_ = -(1/(2.0*sqrt(x)))-(tan(sqrt(100.0-x))/(2*sqrt(100.0-x)))-(((1.0/cos(sqrt(100.0-x)))**2)/2)
end function
