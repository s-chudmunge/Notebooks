program quantumWell
    implicit none
    real(kind=8) :: Eb, Eb_old, tol, asym_f,sym_f,sym_f_derivative,asym_f_derivative
    integer :: i
    tol = 1e-7
    Eb_old = 4.0
    
    do i = 1, 3
        do while(sym_f(Eb_old) > tol)
            Eb = Eb_old - (sym_f(Eb_old)/sym_f_derivative(Eb_old))
            Eb_old = Eb
        end do
        print*, 'Symmetric wavefunction Root', i, ':', Eb
        Eb_old = Eb + 40.0 
    end do
    
    Eb_old = 10.0
    
    do i = 1, 3
        do while(asym_f(Eb_old) > tol)
            Eb = Eb_old - (asym_f(Eb_old)/asym_f_derivative(Eb_old))
            Eb_old = Eb
        end do
        print*, 'Anti-symmetric wavefunction Root', i, ':', Eb
        Eb_old = Eb + 50.0 
    end do
    
end program quantumWell

real(kind=8) function sym_f(Eb)
    real(kind=8) :: Eb
    sym_f = sqrt(100.0-Eb)*tan(sqrt(100.0-Eb))-sqrt(Eb)
end function

real(kind=8) function sym_f_derivative(Eb)
    real(kind=8) :: Eb
      sym_f_derivative = -0.5 * (1.0 / sqrt(Eb) + tan(sqrt(100.0 - Eb)) / sqrt(100.0 - Eb) + tan(sqrt(100.0 - Eb))**2 + 1.0)
end function

real(kind=8) function asym_f(Eb)
    real(kind=8) :: Eb
    asym_f = sqrt(100.0-Eb)*(1/tan(sqrt(100.0-Eb)))-sqrt(Eb)
end function

real(kind=8) function asym_f_derivative(Eb)
    real(kind=8) :: Eb
    asym_f_derivative = -1.0 / (2.0 * sqrt(Eb)) - 1.0 / sqrt(100.0 - Eb) + 1.0 / (sin(sqrt(100.0 - Eb)) ** 2)
    end function

