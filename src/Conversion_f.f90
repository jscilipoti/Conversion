double precision function conversion()
    use InputData
    use Flash
    use fobjtype
    use obj_funcs, only: f
    implicit none
    integer::variables
    real*8::fmin
    real*8,allocatable,dimension(:)::x
    
    double precision,external::praxis, newton

    
    !SENTENCIAS

    variables = 1
    allocate(x(variables))
    x(1) = 0.7
    if(.true.)then
     !   if (variables == 1) then
     !       fmin = newton(x)
      !  else
            fobj = .false. !se llama desde conversion
            FMIN = PRAXIS (3.D-5,2.22D-16,5.D-4,variables,3,x,f,1D-2)  
     !  endif
    else
        call llecalas(T,P,Z)
    endif
    write(*,*)fmin

    conversion = x(1)
endfunction conversion