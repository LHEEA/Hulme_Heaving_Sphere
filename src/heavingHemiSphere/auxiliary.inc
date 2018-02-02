!!! ---------------------------------------------------------------------------
Function Imn(m, n) result(ImnValue)
!!! ---------------------------------------------------------------------------
!!
!!  Compute Integral of Legendre Function
!!
!!      Int_0^1 { Pm(x) * Pn(x) } * dx
!!
!!          1
!!          $  { Pm(x) * Pn(x) } * dx
!!          0
!!
!!! ---------------------------------------------------------------------------
Implicit None
    Integer, intent(in) :: m, n
    Real(RP)            :: ImnValue
    Integer             :: mm

    if ((m.lt.0).or.(n.lt.0)) then
        write(*,*) "    [Error] Imn(m, n), given m, n should be larger than 0"
        write(*,*) "    m, n : ", m , n
    endif

    if (mod(m,2).eq.0) then
        ImnValue = I2mn(m, n)
    else
        ImnValue = I2m1n(m, n)
    end if

End Function
!!! ---------------------------------------------------------------------------

!!! ---------------------------------------------------------------------------
Function I2m1n(twoM1, n) result(Im1nValue)
!!! ---------------------------------------------------------------------------
!!
!!  Compute Integral of Legendre Function
!!
!!      Int_0^1 { P2m+1(x) * Pn(x) } * dx
!!
!!          1
!!          $  { P2m+1(x) * Pn(x) } * dx
!!          0
!!
!!! ---------------------------------------------------------------------------
Implicit None
    Integer, intent(in) :: twoM1
    integer, intent(in) :: n
    Real(RP) :: Im1nValue
    Integer :: mm, nn

    if (mod(twoM1,2).ne.1) then
        write(*,*) "    [Error] I2mn(2m, n), given 2m+1 is not odd !"
        write(*,*) "    2m+1, n : ", twoM1, n
    end if
    mm = (twoM1 - 1)/2

    Im1nValue = 0.0_RP
    if (mod(n,2).eq.1) then
        Im1nValue = 0.0_RP
        if (twoM1.eq.n) Im1nValue = 1.0_RP / (2.0_RP * n + 1.0_RP)
    else
        Im1nValue = I2mn(n, twoM1)
    end if

End Function
!!! ---------------------------------------------------------------------------

!!! ---------------------------------------------------------------------------
Function I2mn(twoM, n) result(ImnValue)
!!! ---------------------------------------------------------------------------
!!
!!  Compute Integral of Legendre Function
!!
!!      Int_0^1 { P2m(x) * Pn(x) } * dx
!!
!!          1
!!          $  { P2m(x) * Pn(x) } * dx
!!          0
!!
!!! ---------------------------------------------------------------------------
Implicit None
    Integer, intent(in) :: twoM
    integer, intent(in) :: n
    Real(RP) :: ImnValue, argM, argN
    Integer :: mm, nn
    Integer :: i
!! -------------------------------------------------------------------------
if (mod(twoM,2).ne.0) then
    write(*,*) "    [Error] I2mn(2m, n), given 2m is not even !"
    write(*,*) "    2m, n : ", twoM, n
end if
mm = twoM/2

ImnValue = 0.0_RP
if (mod(n,2).eq.0) then
    !! Even n
    ImnValue = 0.0_RP
    if (twoM.eq.n) ImnValue = 1.0_RP / (2.0_RP * n + 1.0_RP)
else
    !! Odd n
    nn = (n - 1)/ 2
    ImnValue = -1.0_RP / (twoM - 2.0_RP*nn - 1.0_RP )/(twoM + 2.0_RP*nn + 2.0_RP)
    argM = 1.0_RP
    do i = 1, mm
        argM = -argM * real(mm + i, rp) / real(i, rp) / 4.0_RP
    enddo
    argN = 2.0_RP*nn + 1.0_RP
    do i = 1, nn
        argN = -argN * real(nn + i, rp) / real(i, rp) / 4.0_RP
    enddo
    ImnValue = ImnValue * argM * argN
end if

End Function
!!! ---------------------------------------------------------------------------

!!! ---------------------------------------------------------------------------
Function dImndn(m, n) result(dImndnValue)
!!! ---------------------------------------------------------------------------
!!
!!  Compute Integral of Legendre Function
!!
!!      Int_0^1 { Pm(x) * dPn(x)/dn } * dx
!!
!!          1
!!          $  { P2m(x) * dPn(x)/dn } * dx
!!          0
!!
!!! ---------------------------------------------------------------------------
    Implicit None
    Integer, intent(in) :: m, n
    Complex(RP)         :: dImndnValue
    Integer             :: mm, nn

    Real(RP)            :: PmRn, sum1
    integer             :: k
    Real(RP)            :: A, Ap, B, Bp
    Real(RP)            :: arg
    Real(RP)            :: halpPiN

    if ((m.lt.0).or.(n.lt.0)) then
        write(*,*) "    [Error] dImndn(m, n), given m, n should be larger than 0"
        write(*,*) "    m, n : ", m , n
    endif

    if (m.eq.n) then
        !! - Direct Integral (Log)
        dImndnValue = IntPmPnlog(m, n)
        sum1 = 0.0_RP

        !! - Series Expansion
        do k = 0, n-1
            sum1 = sum1 + (-1.0_RP)**(k+n) * (2.0_RP * k + 1.0_RP)      &
                        / real(n - k,rp) / real(k + n + 1.0_RP, rp)   &
                        * Imn(m, k)
        enddo
        PmRn = 2.0_RP * ( diGamma(2*n+1) - diGamma(n+1) ) * Imn(m, n) + 2.0_RP * sum1
        dImndnValue = dImndnValue + PmRn
    else
        if (mod(m,2).eq.0) then
            mm = m / 2

            A  = gamma(0.5_RP * (m + 1)) * gamma(1.0_RP + 0.5_RP * n) &
                /gamma(0.5_RP * (n + 1)) / gamma(1.0_RP + 0.5_RP * m)
            if (mod(n,2).eq.0) then
                nn = n / 2
                Ap = 0.5_RP * A * ( digamma(1 + nn) - diGammahalf( nn ))
            else
                nn = (n - 1)/ 2
                Ap = 0.5_RP * A * ( diGammahalf(1 + nn) - digamma( nn + 1 ))
            end if

            halpPiN = 0.5_RP * pi * n

            arg = (Ap * sin(halpPiN) + 0.5_RP * pi * A * cos(halpPiN)) &
                 * real(n - m, RP) * real( m + n + 1,rp) &
                 - A * sin(halpPiN) * real(2 * n + 1,rp)

            arg = arg / (n - m) / (n - m) / (m + n + 1) / (m + n + 1)

            dImndnValue = 2.0_RP / pi * (-1)**mm * arg
        else
            mm = (m - 1) / 2

            B =  gamma(0.5_RP * (n + 1)) * gamma(1.0_RP + 0.5_RP * m)  &
               / gamma(0.5_RP * (m + 1)) / gamma(1.0_RP + 0.5_RP * n)

           if (mod(n,2).eq.0) then
               nn = n / 2
               Bp = 0.5_RP * B * ( diGammahalf( nn ) - digamma(1 + nn))
           else
               nn = (n - 1)/ 2
               Bp = 0.5_RP * B * ( digamma( nn + 1) - diGammahalf(1 + nn))
           end if

           halpPiN = 0.5_RP * pi * n

           arg = (Bp * cos(halpPiN) - 0.5_RP * pi * B * sin(halpPiN)) &
                * real(n - m, RP) * real( m + n + 1,rp) &
                - B * cos(halpPiN) * real(2 * n + 1,rp)

           arg = arg / (n - m) / (n - m) / (m + n + 1) / (m + n + 1)

           dImndnValue = -2.0_RP / pi * (-1)**mm * arg

        end if
    endif

End Function
!!! ---------------------------------------------------------------------------

!!! ---------------------------------------------------------------------------
Function Jm(m, ka) result(JmValue)
!!! ---------------------------------------------------------------------------
!!
!!      Compute source of multipole expansion
!!
!!           1
!!      Jm = $ { F(mu,ka) * Pm(mu)} dmu
!!           0
!!
!!! ---------------------------------------------------------------------------
    Implicit None
    Integer, intent(in) :: m
    Real(RP), intent(in) :: ka
    Complex(RP) :: JmValue
    Complex(RP) :: sum1, arg1
    Complex(RP) :: sum2, arg2
    Complex(RP) :: cI2mndn
    integer :: n

    JmValue = (0.0_RP, 0.0_RP)

    sum1 = (0.0_RP, 0.0_RP)
    do n = 1,nMax
        arg1  = (-ka)**n/factorial(n-1) * dImnDn(m, n)
        sum1  = sum1 + arg1
    enddo

    sum2 = (0.0_RP, 0.0_RP)
    do n = 0, nMax
        arg2 = (-ka)**n/factorial(n) &
             * ( real(n,rp)*(diGamma(n+1) + pi * ai - log(ka)) - 1.0_RP ) * Imn(m, n)
        sum2 = sum2 + arg2
    enddo

    JmValue = -Imn(m, 0) - ka * sum1 + ka * sum2

End Function
!!! ---------------------------------------------------------------------------

!!! ---------------------------------------------------------------------------
Function PmPnLog(m, n, x) result(y)
!!! ---------------------------------------------------------------------------
!!
!!  Return Pm(x) * Pn(x) * log( (x+1) / 2 ) used for direct integral
!!                                          (used for computation)
!!
!!! ---------------------------------------------------------------------------
Implicit none
    integer, intent(in)  :: m, n
    real(rp), intent(in) :: x
    Real(rp) :: y
    real(rp),dimension(0:m) :: Pm, dPmdz
    real(rp),dimension(0:n) :: Pn, dPndz

    Call LegendrePn(m, x, Pm, dPmdz)
    Call LegendrePn(n, x, Pn, dPndz)

    y = Pm(m) * Pn(n) * log( ( x + 1.0_RP ) / 2.0_RP )

End Function
!!! ---------------------------------------------------------------------------


!!! ---------------------------------------------------------------------------
Function PmPn(m, n, x) result(y)
!!! ---------------------------------------------------------------------------
!!
!!  Return Pm(x) * Pn(x) used for direct integral (just for comparion)
!!
!!! ---------------------------------------------------------------------------
Implicit none
    integer, intent(in)  :: m, n
    real(rp), intent(in) :: x
    Real(rp) :: y
    real(rp),dimension(0:m) :: Pm, dPmdz
    real(rp),dimension(0:n) :: Pn, dPndz

    Call LegendrePn(m, x, Pm, dPmdz)
    Call LegendrePn(n, x, Pn, dPndz)

    y = Pm(m) * Pn(n)

End Function
!!! ---------------------------------------------------------------------------

!!! ---------------------------------------------------------------------------
Function PmPnn(m, n, x) result(y)
!!! ---------------------------------------------------------------------------
!!
!!  Return Pm(x) * dPn(x)/dn used for direct integral (just for comparion)
!!
!!! ---------------------------------------------------------------------------
Implicit none
    integer, intent(in)  :: m, n
    real(rp), intent(in) :: x
    Real(rp) :: y
    real(rp),dimension(0:m) :: Pm, dPmdz
    real(rp),dimension(0:n) :: Pn, dPndz, dPndN, d2Pndzdn

    Call LegendrePn(m, x, Pm, dPmdz)
    Call LegendredPdN(n, x, Pn, dPndz, dPndN, d2Pndzdn)

    y = Pm(m) * dPndN(n)

End Function

!!! ---------------------------------------------------------------------------
Function IntPmPn(m, n)  result(IntPmPnValue)
!!! ---------------------------------------------------------------------------
!!
!!  Integrated value of Pm(x)*Pn(x) with adaptive Gaussian Quadrature
!!
!!      Int_0^1 { Pm(x) * Pn(x) } * dx
!!
!!          1
!!          $  { Pm(x) * Pn(x) } * dx
!!          0
!!
!!! ---------------------------------------------------------------------------
    real(rp) :: toleranceInt = 1d-9
    Integer, intent(in) :: m, n
    Real(RP) :: IntPmPnValue
    integer  :: nDivide
    Real(RP) :: dx, x1, x2
    integer  :: i
    real(rp) :: sumInt, tempValue

    nDivide = 5 * m + 5 * n + 20

    dx = 1.0_RP / real(nDivide, rp)

    x1 = 0.0_RP
    sumInt = 0.0_RP
    do i = 1, nDivide
        x2 = x1 + dx
        if (x2.ge.1.0_RP) x2 = 1.0_RP
        Call adaptGQ_dFunc_mn(PmPn, m, n, x1, x2, tempValue, toleranceInt)
        sumInt = sumInt + tempValue
        x1 = x2
    enddo
    IntPmPnValue = sumInt

End function
!!! ---------------------------------------------------------------------------


!!! ---------------------------------------------------------------------------
Function IntPmPnn(m, n)  result(IntPmPnnValue)
!!! ---------------------------------------------------------------------------
!!
!!  Integrated value of Pm(x)*dPn(x)/dn with adaptive Gaussian Quadrature
!!
!!      Int_0^1 { Pm(x) * dPn(x)/dn } * dx
!!
!!          1
!!          $  { Pm(x) * dPn(x)/dn } * dx
!!          0
!!
!!! ---------------------------------------------------------------------------
    real(rp) :: toleranceInt = 1d-9
    Integer, intent(in) :: m, n
    Real(RP) :: IntPmPnnValue
    integer  :: nDivide
    Real(RP) :: dx, x1, x2
    integer  :: i
    real(rp) :: sumInt, tempValue

    nDivide = 5 * m + 5 * n + 20

    dx = 1.0_RP / real(nDivide, rp)

    x1 = 0.0_RP
    sumInt = 0.0_RP
    do i = 1, nDivide
        x2 = x1 + dx
        if (x2.ge.1.0_RP) x2 = 1.0_RP
        Call adaptGQ_dFunc_mn(PmPnn, m, n, x1, x2, tempValue, toleranceInt)
        sumInt = sumInt + tempValue
        x1 = x2
    enddo
    IntPmPnnValue = sumInt

End function
!!! ---------------------------------------------------------------------------

!!! ---------------------------------------------------------------------------
Function IntPmPnlog(m, n) result(IntPmPnlogValue)
!!! ---------------------------------------------------------------------------
!!
!!  Integrated value of Pm(x)*Pn(x)* log( (x+1) / 2 )
!!  with adaptive Gaussian Quadrature
!!
!!      Int_0^1 { Pm(x) * Pn(x) * log ((x+1) / 2) } * dx
!!
!!          1
!!          $  { Pm(x) * Pn(x) * log ( (x+1)/2 ) } * dx
!!          0
!!
!!! ---------------------------------------------------------------------------
    Implicit None
    real(rp) :: toleranceInt = 1d-9
    Integer, intent(in) :: m, n
    Real(RP) :: IntPmPnlogValue
    integer  :: nDivide
    Real(RP) :: dx, x1, x2
    integer  :: i
    real(rp) :: sumInt, tempValue
    ! real(rp),external :: PmPnLog

    nDivide = 5 * m + 5 * n + 20

    dx = 1.0_RP / real(nDivide, rp)

    x1 = 0.0_RP
    sumInt = 0.0_RP
    do i = 1, nDivide
        x2 = x1 + dx
        if (x2.ge.1.0_RP) x2 = 1.0_RP
        Call adaptGQ_dFunc_mn(PmPnLog, m, n, x1, x2, tempValue, toleranceInt)
        sumInt = sumInt + tempValue
        x1 = x2
    enddo

    IntPmPnlogValue = sumInt

End function
!!! ---------------------------------------------------------------------------

!!! ---------------------------------------------------------------------------
Subroutine LegendredPdN(n, z, Pn, dPndz, dPndN, d2Pndzdn, writeFlag)
!!! ---------------------------------------------------------------------------
!!
!!  Legenrde Function and its derivatives with respect to argument and order
!!
!!  Output
!!
!!      Pn(z)        : Legendere function
!!      dPn(z)/dz    : Derivatives of Legendere function with respect to z
!!      dPn(z)/dn    : Derivatives of Legendere function with respect to n
!!      d2Pn(z)/dzdn : Derivatives of Legendere function with respect to z and n
!!
!!  References
!!      Szmytkowski R.,2011,"On the derivatives of the associated Legendre
!!        function of the first kind of integer order with respect to its degree
!!        (with applications to the construction of the associated Legendre
!!        function of the second kind of integer degree and order)", J. of Math.
!!        Chemistry,49(7),pp.1436-1477.
!!
!!  Written by Y.-M. Choi (Ecole Centrale de Nantes)
!!
!!! ---------------------------------------------------------------------------
    Implicit None
    integer, intent(in)  :: n
    real(rp), intent(in) :: z
    logical, optional :: writeFlag

    !! - Local Variables
    Real(RP) :: arg, sum1, sum2, term1, term2
    integer  :: k, i, nn
    Real(RP),dimension(0:n) :: Pn, dPndz
    Real(RP),dimension(0:n) :: dPndN, d2Pndzdn

    Real(RP) :: gammaArg
    Real(RP),dimension(0:n) :: Rn, dRndz

    Call LegendrePn(n, z, Pn, dPndz)

    if (present(writeFlag)) then
    if (writeFlag) then
    write(*,*) "----------------------------------------------------------------"
    write(*,*) " "
    write(*,*) "    Derivatives of Legendre Function with respect to its order  "
    write(*,*) " "
    write(*,*) "    Check Rn(z)"
    write(*,*) " "
    write(*,*) "       dPn(z)/dz = Pn(z) * log( (z+1) / 2 ) + Rn(z) "
    write(*,*) " "
    write(*,*) "----------------------------------------------------------------"
    write(*,1002) "n","Residual","Schelkunoff","Bromwich"
    write(*,*) "----------------------------------------------------------------"
    end if
    end if

    !! Compute Rn(z) and dRn(z)/dz
    do nn = 0, n

        !! Rn(z) - Residual Form (Most Efficient and Safe)

        sum1 = 0.0_RP           !! Rn(z)
        sum2 = 0.0_RP           !! dRn(z)/dz
        do k = 0, nn - 1

            arg  = (-1.0_RP)**(k+nn) * (2.0_RP * k + 1.0_RP) / real(nn - k, rp) / (k + nn + 1.0_RP)

            term1 =  arg * Pn(k)
            term2 =  arg * dPndz(k)

            sum1 = sum1 + term1
            sum2 = sum2 + term2
        enddo

        gammaArg = diGamma(2*nn+1) - diGamma(nn+1)

        Rn(nn)    = 2.0_RP * (gammaArg * Pn(nn) + sum1)
        dRndz(nn) = 2.0_RP * (gammaArg * dPndz(nn) + sum2)

        dPndN(nn)    = Pn(nn) * dlog( (z + 1.0_RP) / 2.0_RP ) + Rn(nn)
        d2Pndzdn(nn) = dPndz(nn) * dlog( (z + 1.0_RP) / 2.0_RP ) + Pn(nn) / (z + 1.0_RP) + dRndz(nn)

        if (present(writeFlag)) then
        if (writeFlag) then
        !! Rn(z) - Schelkunuoff

        sum1 = 0.0_RP
        do k = 0, nn
            arg  = (-1.0_RP)**(k+nn) * factorial(k + nn) / factorial(k) / factorial(k) / factorial(nn-k) &
                  * (diGamma(k + nn + 1) - diGamma(k + 1) ) * ( (z+1.0_RP)/ 2.0_RP )**k
            sum1 = sum1 + arg
        enddo
        sum1 = 2.0_RP * sum1

        !! Rn(z) - Bromwich

        sum2 = 0.0_RP
        do k = 0, nn
            arg  = factorial(k + nn) * diGamma(k + nn + 1) / factorial(k) / factorial(k) &
                  / factorial(nn - k) * ( (z-1.0_RP)/ 2.0_RP )**k
            sum2 = sum2 + arg
        enddo
        sum2 = -2.0_RP * diGamma(nn + 1) * Pn(nn) + 2.0_RP * sum2

        write(*,1001) nn, Rn(nn), sum1, sum2
        end if
        end if
    end do

    if (present(writeFlag)) then
    if (writeFlag) then
    write(*,*) "----------------------------------------------------------------"
    end if
    end if

    1001 format(i5, 3(e20.6))
    1002 format(a5, 3(a20))

End Subroutine
!!! ---------------------------------------------------------------------------


!!! ---------------------------------------------------------------------------
Subroutine LegendrePn(n, z, Pn, dPndz)
!!! ---------------------------------------------------------------------------
!!
!!  Legenrde Function and its derivatives with respect to argument
!!
!!  Output
!!
!!      Pn(z)        : Legendere function
!!      dPn(z)/dz    : Derivatives of Legendere function with respect to z
!!
!!! ---------------------------------------------------------------------------
    Implicit None
    integer, intent(in)   :: n
    real(rp), intent(in)  :: z
    Real(RP),Dimension(0:n), intent(out) :: Pn, dPndz
    integer :: i, k, m

    !! - Legendre Polynomials
    if (n.eq.0) then
        Pn(0) = 1.0_RP
    else
        Pn(0) = 1.0_RP
        Pn(1) = z
        do i = 1, n-1
            Pn(i+1) = ( (2.0_RP*i + 1.0_RP)*z*Pn(i) - real(i,rp)*Pn(i-1) ) / real(i + 1.0_RP, rp)
        end do

        !! - Derivatives of Pn(z) : dPn(z)/dz
        do i = 0, n
            dPndz(i) = 0.0_RP
            if (mod(i,2).eq.0) then
                m = i / 2
                do k = 0, m -1
                    dPndz(i) = dPndz(i) + (4.0_RP * k + 3.0_RP) * Pn(2*k+1)
                enddo
            else
                m = (i - 1) / 2
                do k = 0, m
                    dPndz(i) = dPndz(i) + (4.0_RP * k + 1.0_RP) * Pn(2*k)
                enddo
            end if
        end do
    end if

End Subroutine
!!! ---------------------------------------------------------------------------

!!! ---------------------------------------------------------------------------
Function diGamma(n) result(psi)
!!! ---------------------------------------------------------------------------
!!
!!  Return digamm function of integer argument
!!
!!  Output
!!
!!      psi(n) : Digamma function
!!
!!! ---------------------------------------------------------------------------
    Implicit None
    Real(RP),Parameter  :: eulerC = 0.577215664901532860606512_RP
    Integer, intent(in) :: n
    Real(RP) :: psi
    integer :: k
    psi = - eulerC
    if (n.ge.1) then
        do k = 1, n-1
            psi = psi + 1.0_RP / real(k,RP)
        enddo
    else
        write(*,*) "    [Error] : diGamma(n), n should be larger than 0"
        write(*,*) "              given n : ", n
        stop
    end if
End Function
!!! ---------------------------------------------------------------------------

!!! ---------------------------------------------------------------------------
Function diGammahalf(n05) result(psi)
!!! ---------------------------------------------------------------------------
!!
!!  Return digamm function of integer and half argument
!!
!!  Output
!!
!!      psi(n + 1/2) : Digamma function
!!
!!! ---------------------------------------------------------------------------
    Implicit None
    Real(RP),Parameter  :: eulerC = 0.577215664901532860606512_RP
    integer, intent(in) :: n05
    Real(RP) :: psi
    integer :: k, twoN1, twoN, n

    psi = - eulerC - 2.0_RP * log(2.0_RP)
    do k = 1, n05
        psi = psi + 2.0_RP / (2.0_RP * k - 1.0_RP)
    enddo
End Function
!!! ---------------------------------------------------------------------------


!!! ---------------------------------------------------------------------------
Function combination(n, k) result(nCk)
!!! ---------------------------------------------------------------------------
!!
!!  Return combination
!!
!!  Output
!!
!!      nCk : combination
!!
!!! ---------------------------------------------------------------------------
    Implicit None
    Integer :: n, k
    Real(RP) :: nCk
    integer :: i
    nCk = 1.0_RP
    do i = 1, k
        nCk = nCk * (n - i + 1.0_RP) / real(i, RP)
    enddo
End function
!!! ---------------------------------------------------------------------------

!!! ---------------------------------------------------------------------------
Recursive function factorial(n) result(yy)
!!! ---------------------------------------------------------------------------
!!
!!  Return factorial
!!
!!  Output
!!
!!      n! : factorial
!!
!!! ---------------------------------------------------------------------------
    implicit None
    integer, intent(in) :: n
    double precision :: yy
    if (n.eq.0) then
        yy = 1.d0
    elseif (n.eq.1) then
        yy = 1.d0
    else
        yy = dble(n) * factorial(n-1)
    end if
end function
!!! ---------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------
Subroutine GaussJ(A,N,B,M)
!-------------------------------------------------------------------------------------------------
!
!   MATRIX INVERSE SUBROUTINE GAUSSJ
!
!   DESCRIPTION :
!
!   LINEAR EQUATION SOLUTION BY GAUSS-JORDAN ELIMINATION. A IS AN INPUT MATRIX OF N BY N ELEMENTS,
!   STORED IN AN ARRAY OF PHYSICAL DIMENSIONS n BY n. B IS AN INPUT MATRIX OF N BY M CONTAINING
!   THE M RIGHT HAND SIDE VECTORS, STORED IN AN ARRAY OF PHYSICAL DIMENSIONS n BY m. ON OUTPUT,
!   A IS REPLACED BY ITS MATRIX INVERSE, AND B IS REPLACED BY THE CORRESPONDING SET OF SOLUTION
!   VECTORS.
!
!   REFERENCES :
!
!       NUMERICAL RECIPES
!
!   Written by Y.-M CHOI, 18TH, JULY, 2014
!
!-------------------------------------------------------------------------------------------------
    Implicit None
    Integer,parameter :: Nmax = 10000
    Integer,intent(in) :: N,M
    Real(RP),intent(inOUT) :: A(n,n),B(n,m)

    integer :: IPTV(Nmax),INDXR(Nmax),INDXC(Nmax)

    integer :: I,j,K,L,LL

    Real(RP) :: BIG,DUM
    INTEGER :: IROW,ICOL

    Real(RP) :: PIVINV
!-------------------------------------------------------------------------------------------------

    do j = 1,N
        IPTV(J) = 0.D0
    enddo

    DO I = 1,N
        BIG = 0.D0
        DO J=1,N
            IF (IPTV(J).NE.1) THEN
                DO K = 1,N
                    IF (IPTV(K).EQ.0) THEN
                        IF (DABS(A(J,K)).GE.BIG) THEN
                            BIG = DABS(A(J,K))
                            IROW = J
                            ICOL = K
                        ENDIF
                    ELSEIF (IPTV(K).GT.1) THEN
                        STOP 'SINGULAR MATRIX'
                    ENDIF
                ENDDO
            ENDIF
        ENDDO

        IPTV(ICOL) = IPTV(ICOL) + 1

        IF (IROW.NE.ICOL) THEN
            DO L = 1,N
                DUM = A(IROW,L)
                A(IROW,l) = A(ICOL,L)
                A(ICOL,L) = DUM
            ENDDO

            DO L = 1,M
                DUM = B(IROW,L)
                B(IROW,L) = B(ICOL,L)
                B(ICOL,L) = DUM
            ENDDO
        ENDIF

        INDXR(I) = IROW
        INDXC(I) = ICOL

        IF (A(ICOL,ICOL).EQ.0.D0) STOP 'SINGULAR MATRIX'

        PIVINV = 1.D0/A(ICOL,ICOL)
        A(ICOL,ICOL) = 1.D0

        DO L = 1,N
            A(ICOL,L) = A(ICOL,L)*PIVINV
        ENDDO

        DO L = 1,M
            B(ICOL,L) = B(ICOL,L)*PIVINV
        ENDDO

        DO LL = 1,N
            IF (LL.NE.ICOL) THEN
                DUM = A(LL,ICOL)
                A(LL,ICOL) = 0.D0
                DO L = 1,N
                    A(LL,L) = A(LL,L) - A(ICOL,L)*DUM
                ENDDO

                DO L = 1,M
                    B(LL,L) = B(LL,L) - B(ICOL,L)*DUM
                ENDDO
            ENDIF
        ENDDO
    ENDDO

    DO L = N,1,-1
        IF (INDXR(L).NE.INDXC(L)) THEN
            DO K=1,N
                DUM = A(K,INDXR(L))
                A(K,INDXR(L)) = A(K,INDXC(L))
                A(K,INDXC(L)) = DUM
            ENDDO
        ENDIF
    ENDDO
!-------------------------------------------------------------------------------------------------
    End Subroutine
!-------------------------------------------------------------------------------------------------
