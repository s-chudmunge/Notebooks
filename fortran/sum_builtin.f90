program exp_builtin

        implicit none 

        real::x,y

        print *,'enter value of x:'

        read*,x

        y= exp(-x)

        print *,'exp(-x) is:',y



        end program exp_builtin
