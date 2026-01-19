program simpson_rule
  implicit none
  real,external::f
  real(8)::x,a,b,h,simson,f_odd,f_even
  integer::n,i,m
  print*,'enter the number of sub-intervals'
  read*,n

  a=1
  b=3
  h= (b-a)/n
  m=n/2
  f_odd=0
  do i=0,m-1
  f_odd=f_odd+f(a+(2*i+1)*h)
  end do
  f_even=0
  do i=2,m-2
  f_even=f_even+f(a+2*i*h)
  end do
  simson= (h/3.)*(f(a)+f(b)+4*f_odd+2*f_even)
  print*,simson




end program simpson_rule


real function f(x)
       real(8)::x
    ! Define the function here
    f = x**6 - x**2*sin(2*x)
  end function 


