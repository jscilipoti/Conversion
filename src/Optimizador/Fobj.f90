module obj_funcs
contains
doubleprecision function f(x,n)
    use Flash
    use flashout

    implicit none
    integer,intent(in)::n
    real*8::x(n)
    integer::i
    real*8::act(10),Keqcalc,Kexp,zf(10)
    double precision,external::conversion
    !Sentencias


        !Calculo composici�n final a partir de la nueva conversi�n
        if(x(1)<0 .or. x(1)>1)then
            f = 10000
            return
        endif
        zf(:) = z(:)
        zf(1) = z(1) - z(1)*x(1)
        zf(2) = z(2) - z(1)*x(1)
        zf(3) = z(3) + z(1)*x(1)
        zf(4) = z(4) + z(1)*x(1)
        !normalizo Z
        zf(:) = zf(:)/sum(zf(:))
        !flash
        call llecalas(T,P,zf)
        !c�lculo de actovidades
        do i=1,size(z)
            act(i) = compfases(i,1)*exp(agam(i,1))
        enddo
        !c�lculo de Keq(clac)
        Keqcalc = act(3)*act(4)/(act(1)*act(2))
        !Kexp = 50.83900666   !ac�tico + hexanol 
        Kexp = 19.009725*dexp((-21580/8.314)*((1/T)-(1/298.15))) ! Ol�ico + etanol
        f = (Kexp-keqcalc)**2

        contador = contador + 1

    endfunction f
    
!********************************************************
doubleprecision function f_n(x,n)
    use Flash
    use flashout

    implicit none
    integer,intent(in)::n
    real*8::x(n),conv
    double precision,external::conversion
    !Sentencias

    if(isnan(x(1)))pause
    if(x(1)<0) then
        f_n = 1000
        return
    endif
    !z(4) = x(1)
    !z(2) = x(2)
    !T = x(3)
    contador=0
    conv=conversion()
    write(333,*) x(1),conv
    pause
    f_n = (1-conv)**2

endfunction f_n
end module
