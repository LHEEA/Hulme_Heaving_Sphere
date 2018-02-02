!! -----------------------------------------------------------------------------
Subroutine initializeHeavingHemiSphere(this, radius, omega, motionAmp)
!! -----------------------------------------------------------------------------
!!
!!  Initialize Heaving Hemisphere
!!
!!      - Construct System Equation of Heaving Hemisphre
!!
!!      - Compute Multipole Amplitudes from System Equation
!!
!!      - Compute Nondimensional Added Mass and Damping
!!
!!  Input
!!
!!      this      : Heving Hemisphre Type(Class; typHeavingHemiSphere)
!!      radius    : Radius of Heaving Hemisphere [m]
!!      omega     : Moving Frequency [rad/s]
!!      motionAmp : Moving Amplitude [m], default = 1
!!
!! -----------------------------------------------------------------------------
    Implicit None
    Class(typHeavingHemiSphere), intent(inout) :: this
    Real(RP), intent(in) :: radius
    Real(RP), intent(in) :: omega
    Real(RP), optional :: motionAmp
    ! -------------------------------------------------------------------

    !!... Set Heaving Hemisphere parameters
    this%a      = radius
    this%omega  = omega

    this%k  = omega * omega / gravi
    this%ka = this%k * this%a

    if (present(motionAmp)) then
        this%motionAmp = motionAmp
    else
        this%motionAmp = 1.0_RP
    end if

    !!
    !!  Hulme's formulation is based on velocity amplitude.
    !!  In this code, heave motion is given as
    !!
    !!       Heave = A * cos(omega * t) = Re[ A * exp( -i * omega  * t) ]
    !!
    this%velocityAmp = ai * omega * this%motionAmp

    !!... Write Log
    write(*, *) " "
    write(*, *) "   Multipole Expansion : Heaving Hemisphere on Free Surface"
    write(*, *) " "
    write(*, 1001) "       - Radius      [m]     : ", this%a
    write(*, 1001) "       - Frequency   [rad/s] : ", this%omega
    write(*, 1001) "       - Wave Number [rad/m] : ", this%k
    write(*, 1001) "       - ka [-]              : ", this%ka
    write(*, *) " "

    write(*, *) "       Solve Multipole System                  ... "

    !! Construct System Equation and Solve
    Call this%constructModeMatrix()

    write(*, *) "                                               ... Done"
    write(*, *) " "

    write(*, *) "       Compute Added Mass and Damping          ... "

    !! Compute Radiation Force (Added Mass and Damping)
    Call this%ComputeRadiationForce()

    write(*, *) "                                               ... Done "
    write(*, *) " "
    write(*, 1001) "       - Added Mass / rho g V    [-] : ", this%addedMass
    write(*, 1001) "       - Damping / rho g V omega [-] : ", this%damping
    write(*, *) " "

1001 format(a, f15.8)

End Subroutine
!! -----------------------------------------------------------------------------

!! -----------------------------------------------------------------------------
Subroutine constructModeMatrix(this)
!! -----------------------------------------------------------------------------
!!
!!  Construct and Solve Multipole System Equation
!!
!!      - Construct System Equation of Heaving Hemisphre
!!
!!      - Compute Multipole Amplitudes from System Equation
!!
!! -----------------------------------------------------------------------------
    Implicit None
    Class(typHeavingHemiSphere), intent(inout) :: this
    integer  :: m, n
    integer  :: twoM, twoN
    real(rp) :: mFn1mTn1
    !! Multipole Influence Matrix
    Real(RP),dimension(mMax, mMax) :: Mmn
    !! Multipole Amplitudes Vector
    Complex(RP),dimension(mMax)    :: betaM
    !! Multipole Forcing Matrix
    Real(RP),dimension(mMax, 2)    :: betaMSolve
    Complex(RP) :: sumArg
    Real(RP)    :: ItwoM1
    Complex(RP) :: J0

    this%c0  = (0.0_RP, 0.0_RP)
    !! - initialize Matrix
    do m = 1, mMax
        this%multipoleAmp(m)  = (0.0_RP, 0.0_RP)
        betaM(m)              = (0.0_RP, 0.0_RP)
        betaMSolve(m, 1)      = 0.0_RP
        betaMSolve(m, 2)      = 0.0_RP
        do n = 1, mMax
            Mmn(m, n) = 0.0_RP
        enddo
    enddo

    J0 = Jm(0, this%ka)
    !! - Influence & Forcing Matrix (System Equation)
    do m = 1, mMax
        twoM = 2 * m
        mFn1mTn1 = (2.0_RP * m + 1.0_RP) / (4.0_RP * m + 1.0_RP)

        ItwoM1 = Imn(twoM, 1)
        do n = 1, mMax
            twoN = 2 * n
            Mmn(m, n) = this%ka * ( Imn(twoM, twoN - 1) - 2.0_RP * ItwoM1 * Imn(0, twoN - 1) )
            if (n.eq.m) Mmn(m, n) = Mmn(m, n) + mFn1mTn1
        enddo

        betaM(m) = Jm(twoM, this%ka) - 2.0_RP * J0 * ItwoM1

        betaMSolve(m, 1) = dreal( betaM(m) )
        betaMSolve(m, 2) = dimag( betaM(m) )

    enddo

    !! - Matrix Solving (Checked)
    Call GaussJ(Mmn, mMax, betaMSolve, 2)

    !! Get Multipole Amplitudes (Wave Free term)
    do m = 1, mMax
        this%multipoleAmp(m) = betaMSolve(m, 1) + ai * betaMSolve(m, 2)
    enddo

    !! Compute Multipole Amplitudes (Wave term)
    sumArg = (0.0_RP, 0.0_RP)
    do m = 1, mMax
        twoM = 2 * m
        sumArg = sumArg + this%multipoleAmp(m) * Imn(0, twoM - 1)
    enddo
    this%c0 = 0.5_RP / (J0 - this%ka * sumArg)

End Subroutine
!! -----------------------------------------------------------------------------

!! -----------------------------------------------------------------------------
Subroutine ComputeRadiationForce(this)
!! -----------------------------------------------------------------------------
!!
!!  Compute Radiation Force (Added Mass and Damping)
!!
!!      - Compute Nondimensional Added Mass and Damping
!!
!! -----------------------------------------------------------------------------
    Implicit None
    Class(typHeavingHemiSphere), intent(inout) :: this
    Complex(RP)              :: wavePole
    Real(RP),dimension(mMax) :: waveFreePole
    Complex(RP)             :: sum1, sum2, arg
    Real(RP)             :: argR, argI
    integer              :: nn
    Complex(RP)          :: nonDimForce
    !! - External Functions ------------------------------------------------
    Real(RP)             :: ImnValue
    Complex(RP)          :: dImndnValue

    !! - Wave Term ---------------------------------------------------------
    wavePole = (0.0_RP, 0.0_RP)
    wavePole = Imn(0, 1)
    do nn = 0, nMax
        arg =  (-this%ka)**nn / factorial(nn) &
              * ( ( pi * ai + diGamma(nn+1) - dlog(this%ka) ) * Imn(nn, 1) - dImndn(1, nn) )
        wavePole = wavePole + this%ka * arg
    enddo

    !! Wave Free Term ------------------------------------------------------
    do nn = 1, mMax
        waveFreePole(nn) = 0.0_RP
        waveFreePole(nn) = Imn(2*nn, 1) + this%ka / 2.0_RP / real(nn,rp) * Imn(2*nn - 1, 1)
    enddo

    !! Force Summation -----------------------------------------------------
    nonDimForce = wavePole
    do nn = 1, mMax
        nonDimForce = nonDimForce + this%multipoleAmp(nn) * waveFreePole(nn)
    enddo
    nonDimForce = 3.0_RP * ai * this%c0 * nonDimForce

    this%rForce    =  nonDimForce
    this%addedMass = -dimag(nonDimForce)
    this%damping   =  dreal(nonDimForce)

    ! write(*,*) this%ka, this%omega, this%addedMass, this%damping

End Subroutine
!! -----------------------------------------------------------------------------

!! -----------------------------------------------------------------------------
Subroutine radiationForce(this, addedMass, damping)
!! -----------------------------------------------------------------------------
!!
!!  Return Nondimensional added mass and damping
!!
!! -----------------------------------------------------------------------------
    Implicit None
    Class(typHeavingHemiSphere), intent(in) :: this
    Real(RP), intent(out) :: addedMass, damping

    addedMass = this%addedMass
    damping = this%damping
End Subroutine
!! -----------------------------------------------------------------------------


!! -----------------------------------------------------------------------------
Subroutine hSpherePressureAmp(this, x, y, z, pAmp)
!! -----------------------------------------------------------------------------
!!
!!  Return Pressure Amplitudes (Complex Value)
!!
!!  Input
!!
!!      this    : heaving hemisphere type(class)
!!      x, y, z : position
!!
!!        -  sqrt(x*x + y*y + z*z) > radius
!!        -  z <=0
!!
!!  Output
!!
!!      pAmp    : Pressure Amplitude for given position
!!
!! -----------------------------------------------------------------------------
    Implicit None
    Class(typHeavingHemiSphere), intent(in) :: this
    Real(RP), intent(in) :: x, y, z
    Complex(RP), intent(out) :: pAmp
    !! Local Variables --------------
    Real(RP)    :: r, theta, alpha
    Complex(RP) :: velocityPotential
    Complex(RP) :: waveAmp, expmIomegaT

    Call this%transCoord(x, y, z, r, theta, alpha)

    velocityPotential = hSpherePhi(this, r, theta)

    pAmp = ai * this%omega * rhoWater * velocityPotential
    pAmp = this%velocityAmp * pAmp

End Subroutine
!! -----------------------------------------------------------------------------


!! -----------------------------------------------------------------------------
Subroutine hSpherePressureTime(this, x, y, z, t, p)
!! -----------------------------------------------------------------------------
!!
!!  Return Pressure Time Series for given position and time vector
!!
!!  Input
!!
!!      this    : heaving hemisphere type(class)
!!      x, y, z : position
!!      t       : time array
!!
!!        -  sqrt(x*x + y*y + z*z) > radius
!!        -  z <=0
!!
!!  Output
!!
!!      p       : Pressure time series
!!
!! -----------------------------------------------------------------------------
    Implicit None
    Class(typHeavingHemiSphere), intent(in) :: this
    Real(RP), intent(in) :: x, y, z
    Real(RP), intent(in) :: t(:)
    Real(RP), intent(out) :: p(:)
    !! Local Variables --------------
    Complex(RP) :: pAmp, expmIomegaT
    integer     :: it, nT

    Call hSpherePressureAmp(this, x, y, z, pAmp)

    nT = sizeof(t)/sizeof(1.0_RP)
    do it = 1, nT
        expmIomegaT = this%expTime( t(it) )
        p(it) = real(pAmp * expmIomegaT, rp)
    enddo

End Subroutine
!! -----------------------------------------------------------------------------

!! -----------------------------------------------------------------------------
Subroutine hSphereZetaAmp(this, x, y, z, zetaAmp)
!! -----------------------------------------------------------------------------
!!
!!  Return Wave Elevation Amplitudes for given position (complex value)
!!
!!  Input
!!
!!      this    : heaving hemisphere type(class)
!!      x, y, z : position
!!
!!        -  sqrt(x*x + y*y + z*z) > radius
!!        -  z <=0
!!
!!  Output
!!
!!      zetaAmp : Wave Elevation Amplitudes
!!
!! -----------------------------------------------------------------------------
    Implicit None
    Class(typHeavingHemiSphere), intent(in) :: this
    Real(RP), intent(in) :: x, y, z
    Complex(RP), intent(out) :: zetaAmp
    !! Local Variables --------------
    Real(RP)    :: r, theta, alpha
    Complex(RP) :: velocityPotential

    Call this%transCoord(x, y, z, r, theta, alpha)

    velocityPotential = hSpherePhi(this, r, theta)

    zetaAmp = ai * this%omega / gravi * velocityPotential
    zetaAmp = this%velocityAmp * zetaAmp

End Subroutine
!! -----------------------------------------------------------------------------

!! -----------------------------------------------------------------------------
Subroutine hSphereZetaTime(this, x, y, z, t, zeta)
!! -----------------------------------------------------------------------------
!!
!!  Return Wave Elevation Time Series for given position and time vector
!!
!!  Input
!!
!!      this    : heaving hemisphere type(class)
!!      x, y, z : position
!!      t       : time array
!!
!!        -  sqrt(x*x + y*y + z*z) > radius
!!        -  z <=0
!!
!!  Output
!!
!!      zeta    : Wave Elevation time series
!!
!! -----------------------------------------------------------------------------
    Implicit None
    Class(typHeavingHemiSphere), intent(in) :: this
    Real(RP), intent(in) :: x, y, z
    Real(RP), intent(in) :: t(:)
    Real(RP), intent(out) :: zeta(:)
    !! Local Variables --------------
    Complex(RP) :: zetaAmp, expmIomegaT
    integer     :: it, nT

    Call hSphereZetaAmp(this, x, y, z, zetaAmp)

    nT = sizeof(t)/sizeof(1.0_RP)
    do it = 1, nT
        expmIomegaT = this%expTime( t(it) )
        zeta(it) = real(zetaAmp * expmIomegaT, rp)
    enddo

End Subroutine
!! -----------------------------------------------------------------------------

!! -----------------------------------------------------------------------------
Subroutine hSphereUAmp(this, x, y, z, uAmp, vAmp, wAmp)
!! -----------------------------------------------------------------------------
    Implicit none
    Class(typHeavingHemiSphere), intent(in) :: this
    real(rp), intent(in)  :: x, y, z
    Complex(rp), intent(out) :: uAmp, vAmp, wAmp
    !! Local Variables
    Real(RP)    :: r, theta, alpha
    Complex(RP) :: dPhiDr, dPhiDtheta
    Real(RP)    :: drdx, drdy, drdz, dthetadx, dthetady, dthetadz
    Real(RP)    :: st, ct, ca, sa

    !! Cartesian Coord. to Spherical Coord.
    Call this%transCoord(x, y, z, r, theta, alpha)

    !! Gradient of velocity potential
    Call hSphereGradPhi(this, r, theta, dPhiDr, dPhiDtheta)

    st = dsin(theta);   ct = dcos(theta);   ca = dcos(alpha);   sa = dsin(alpha)

    !! Chain Rule
    drdx     =  st * ca;        drdy     =  st * sa;        drdz     = -ct
    dthetadx =  ct * ca / r;    dthetady =  ct * sa / r;    dthetadz =  st / r

    !! Apply Jacobian
    uAmp = dPhiDr * drdx + dPhiDtheta * dthetadx
    vAmp = dPhiDr * drdy + dPhiDtheta * dthetady
    wAmp = dPhiDr * drdz + dPhiDtheta * dthetadz

    uAmp =  this%velocityAmp * uAmp
    vAmp =  this%velocityAmp * vAmp
    wAmp =  this%velocityAmp * wAmp

End Subroutine
!! -----------------------------------------------------------------------------


!! -----------------------------------------------------------------------------
Subroutine hSphereU(this, x, y, z, t, u, v, w)
!! -----------------------------------------------------------------------------
!!
!!  Return Fluids Velocity Time Series for given position and time vector
!!
!!  Input
!!
!!      this    : heaving hemisphere type(class)
!!      x, y, z : position
!!      t       : time array
!!
!!        -  sqrt(x*x + y*y + z*z) > radius
!!        -  z <=0
!!
!!  Output
!!
!!      u, v, w : Fluids Velocity
!!
!! -----------------------------------------------------------------------------
    Implicit none
    Class(typHeavingHemiSphere), intent(in) :: this
    real(rp), intent(in)  :: x, y, z
    Real(RP), intent(in)  :: t(:)
    real(rp), intent(out) :: u(:), v(:), w(:)
    !! Local Variables
    complex(rp) :: uAmp, vAmp, wAmp
    Complex(rp) :: expmIomegaT
    integer :: it, nT

    Call hSphereUAmp(this, x, y, z, uAmp, vAmp, wAmp)

    nT = sizeof(t)/sizeof(1.0_RP)
    do it = 1, nT
        expmIomegaT = this%expTime( t(it) )
        u(it) = real(uAmp * expmIomegaT, rp)
        v(it) = real(vAmp * expmIomegaT, rp)
        w(it) = real(wAmp * expmIomegaT, rp)
    enddo

End Subroutine
!! -----------------------------------------------------------------------------

!! -----------------------------------------------------------------------------
Function expTime(this, t) result(expmIomegaT)
!! -----------------------------------------------------------------------------
!!
!!  Return exp(-i * omega * t)
!!
!! -----------------------------------------------------------------------------
    Implicit None
    Class(typHeavingHemiSphere), intent(in) :: this
    real(rp),intent(in) :: t
    real(rp)            :: timePhase
    complex(rp)         :: expmIomegaT

    timePhase   = this%omega * t
    expmIomegaT = cmplx( dcos(timePhase), -dsin(timePhase), rp )    !! exp(-i*omega*t)
End Function
!! -----------------------------------------------------------------------------


!! -----------------------------------------------------------------------------
Function hSpherePhi(this, r, theta) result(phiValue)
!! -----------------------------------------------------------------------------
!!
!!  Return velocity potential of heaving hemisphere
!!
!!  Input
!!
!!      - this  : heaving hemisphere type(class)
!!      - r     : r-coodinates of heaving hemisphere problem
!!      - theta : theta-coordinates of heaving hemisphere problem
!!
!!  Output
!!
!!      - phiValue : velocity potential of heaving hemisphere
!!
!! -----------------------------------------------------------------------------
    Implicit none
    Class(typHeavingHemiSphere), intent(in) :: this
    real(rp), intent(in) :: r, theta
    complex(rp) :: phiValue, phi0
    real(rp), dimension(mMax) :: phi2m
    !!-- Local Variables
    complex(rp) :: arg1, arg2, sum1
    integer     :: n, m, k
    real(rp)    :: kr, mu
    Real(RP),dimension(0:2*nMax) :: Pn, dPndz
    Real(RP),dimension(0:2*nMax) :: dPndN, d2Pndzdn
    real(rp) :: a2

    kr = this%k * r
    mu = dcos(theta)

    a2 = this%a*this%a

    phiValue = (0.0_RP, 0.0_RP)

    !! Wave Term ------------------------------------------------------
    Call LegendredPdN(2*mMax, mu, Pn, dPndz, dPndN, d2Pndzdn)

    !! Oscillating Term
    sum1 = (0.0_RP, 0.0_RP)
    do n = 0, 2 * nMax
        arg1 = (1.0_RP, 0.0_RP)
        do k = 1, n
            arg1 = arg1 * (-kr) / real(k,rp)
        enddo
        arg2 = (digamma(n + 1) - dlog(kr) + ai * pi) * Pn(n) - dPndN(n)
        sum1 = sum1 + arg1 * arg2
    enddo

    phi0 = 1.0_RP / r + this%k * sum1

    !! Wave Free Term ------------------------------------------------
    do m = 1, mMax
        phi2m(m) = Pn(2*m) / r**(2*m+1) + this%k / (2.0_RP*m) * Pn(2*m - 1) / r**(2*m)
    enddo

    !! Total Potential
    phiValue = (0.0_RP, 0.0_RP)
    do m = 1, mMax
        phiValue = phiValue + this%multipoleAmp(m) * a2**m * phi2m(m)
    enddo

    phiValue = this%c0 * a2 * ( phi0 + phiValue)

End Function
!! -----------------------------------------------------------------------------

!! -----------------------------------------------------------------------------
Subroutine hSphereGradPhi(this, r, theta, dPhiDr, dPhiDtheta)
!! -----------------------------------------------------------------------------
!!
!!  Return velocity potential of heaving hemisphere
!!
!!  Input
!!
!!      - this  : heaving hemisphere type(class)
!!      - r     : r-coodinates of heaving hemisphere problem
!!      - theta : theta-coordinates of heaving hemisphere problem
!!
!!  Output
!!
!!      - phiValue : velocity potential of heaving hemisphere
!!
!! -----------------------------------------------------------------------------
    Implicit none
    Class(typHeavingHemiSphere), intent(in) :: this
    real(rp), intent(in) :: r, theta
    Complex(rp) :: argMM
    Complex(rp) :: dPhiDr, dPhiDtheta
    Complex(rp) :: arg01, arg02, sum01, sum02
    real(rp)    :: argm1, argm2, summ1, summ2
    Real(RP),dimension(0:2*nMax) :: Pn, dPndz
    Real(RP),dimension(0:2*nMax) :: dPndN, d2Pndzdn
    real(rp)    :: r2, a2, kr, mu
    real(rp)    :: krNFac
    complex(rp) :: diGammaLogPi

    Complex(rp) :: dPhi0Dr, dPhi0Dmu, dPhi0Dtheta
    Real(rp),dimension(nMax) :: dPhi2mDr, dPhi2mDmu, dPhi2mDtheta
    real(rp) :: dMuDtheta
    integer     :: n, m, k

    r2 = r*r
    a2 = this%a * this%a

    kr = this%k * r
    mu = dcos(theta)

    dMuDtheta = -dsin(theta)

    Call LegendredPdN(2*mMax, mu, Pn, dPndz, dPndN, d2Pndzdn)

    !! Wave Term
    sum01 = (0.0_RP, 0.0_RP);           sum02 = (0.0_RP, 0.0_RP)
    do n = 0, 2*nMax
        arg01 = (0.0_RP, 0.0_RP);       arg02 = (0.0_RP, 0.0_RP)

        diGammaLogPi = digamma(n+1) - dlog(kr) + ai * pi

        !! (-kr)**n / factorial(n)
        krNFac = 1.0_RP
        do k = 1, n
            krNFac = krNFac * (-kr) / real(k,rp)
        enddo

        !! dPhi/dr
        arg01 = (real(n, rp) * diGammaLogPi - 1.0_RP) * Pn(n) - real(n, rp) * dPndN(n)
        arg01 = krNFac / (-kr) * arg01

        !! dPhi/dmu
        arg02 = krNFac * ( diGammaLogPi * dPndz(n) - d2Pndzdn(n))

        sum01 = sum01 + arg01
        sum02 = sum02 + arg02

    enddo

    dPhi0Dr     = -1.0_RP / r2 - this%k**2 * sum01
    dPhi0Dmu    = this%k * sum02
    dPhi0Dtheta = dPhi0Dmu * dMuDtheta

    !! Wave Free Term
    do m = 1, mMax
        dPhi2mDr(m)     = -( (2.0_RP * m + 1.0_RP) * Pn(2*m) + kr * Pn(2*m-1) ) / r2**(m+1)
        dPhi2mDmu(m)    = ( dPndz(2*m) + kr / (2.0_RP * m) * dPndz(2*m - 1)) / r2**m / r
        dPhi2mDtheta(m) = dPhi2mDmu(m) * dMuDtheta
    enddo

    !! Total Potential Derivatives
    dPhiDr = (0.0_RP, 0.0_RP);   dPhiDtheta = (0.0_RP, 0.0_RP)
    do m = 1, mMax
        argMM       = this%multipoleAmp(m) * a2**m
        dPhiDr      = dPhiDr     + argMM * dPhi2mDr(m)
        dPhiDtheta  = dPhiDtheta + argMM * dPhi2mDtheta(m)
    enddo

    dPhiDr     = this%c0 * a2 * (dPhi0Dr + dPhiDr)
    dPhiDtheta = this%c0 * a2 * (dPhi0Dtheta + dPhiDtheta)

End Subroutine
!! -----------------------------------------------------------------------------

!! -----------------------------------------------------------------------------
Subroutine transCoord(this, x, y, z, r, theta, alpha)
!! -----------------------------------------------------------------------------
!!
!!  Transform Cartesian Coordinates to Spherical Coordinates
!!
!!  Input
!!
!!      x, y, z : Cartesian Coordinates
!!
!!  Output
!!
!!      r, theta, alpha : Spherical Coordinates (Check theta definition before use)
!!
!! -----------------------------------------------------------------------------
    Implicit none
    Class(typHeavingHemiSphere), intent(in) :: this
    real(rp), intent(in)  :: x, y, z
    real(rp), intent(out) :: r, theta, alpha
    real(rp) :: RR2, R2

    !! Compute
    RR2 = x*x + y*y
    R2  = dsqrt(RR2)
    r   = dsqrt(RR2 + z*z)

    if (r.le.0.0001) then
        write(*,*) "    [ERROR] transCoord(x, y, z, r, theta, alpha)"
        write(*,*) "        r is less than 0.0001."
        write(*,*) "        given (x, y, z) : ", x, y, z
        stop
    end if

    alpha = datan2(y, x)
    theta = datan2(R2, -z)

End Subroutine
!! -----------------------------------------------------------------------------
