    Subroutine testImn(m)
        Implicit None
        Integer :: m, n
        Real(RP) :: intValue1, intValue2

        write(*,*) "----------------------------------------------------------------"
        write(*,*) " Test Integral of Legendre Function."
        write(*,*) " "
        write(*,*) "        I(m,n) = int_0^1 { Pm(x) Pn(x)} dx"
        write(*,*) " "
        write(*,*) " m = ", m
        write(*,*) " "
        write(*,*) "----------------------------------------------------------------"
        write(*,1002) "n","Values","Direct Integral","Difference"
        write(*,*) "----------------------------------------------------------------"

        !! Int Pm Pn from 0 to 1 (Formula vs. Numerical Quad)
        do n = 0, nMax
            intValue1 = Imn(m, n)
            intValue2 = IntPmPn(m, n)
            write(*,1001) n, intValue1, intValue2, intValue1 - intValue2
        enddo

        write(*,*) "----------------------------------------------------------------"
        write(*,*) " "
        write(*,*) " Test Integral of Legendre Function and derivates w.r.t. order."
        write(*,*) " "
        write(*,*) "        I(m,n) = int_0^1 { Pm(x) dPn(x)/dn} dx"
        write(*,*) " "
        write(*,*) " m = ", m
        write(*,*) " "
        write(*,*) "----------------------------------------------------------------"
        write(*,1002) "n","Values","Direct Integral","Difference"
        write(*,*) "----------------------------------------------------------------"

        !! Int Pm dPn/dn from 0 to 1 (Formula vs. Numerical Quad)
        do n = 0, nMax
            intValue1 = dImndn(m, n)
            intValue2 = IntPmPnn(m, n)
            write(*,1001) n, intValue1, intValue2,  intValue1 - intValue2
        enddo
        write(*,*) "----------------------------------------------------------------"

        1001 format(i5, 3(e20.8))
        1002 format(a5, 3(a20))

    End Subroutine

    Subroutine testDiGamma()
        Implicit None
        integer :: i
        real(RP) :: psi

        do i = 1, 20
            psi = diGamma(i)
            write(*,*) i, psi
        enddo

    End Subroutine

    Subroutine testDiGamma05()
        Implicit None
        integer :: i
        real(RP) :: psi

        do i = 0, 20
            psi = diGammahalf(i)
            write(*,*) i + 0.5, psi
        enddo

    End Subroutine

    Subroutine testJ2m()
        Implicit None
        integer :: m, twoM
        integer :: i
        Complex(RP) :: JmValue, dm, J0
        Real(RP) :: ka, Im

        ka = 0.8_RP

        J0 = Jm(0, ka)
        do i = 1, 10
            twoM = 2 * i
            JmValue = Jm(twoM, ka)
            Im = Imn(twoM, 1)
            dm = JmValue - 2.0_RP * J0 * Im
            write(*,*) i, JmValue, dm
        enddo

    End Subroutine

    Subroutine testPnl()
        Implicit None
        integer, parameter :: n = 10
        integer  :: nTheta
        real(RP) :: z, dtheta
        Real(RP),dimension(0:n) :: Pn, dPndz, dPndn, d2Pndzdn
        integer :: i

        nTheta = 20

        ! z = 0.2_RP

        z = 1.0_RP

        Call LegendredPdN(n, z, Pn, dPndz, dPndn, d2Pndzdn, .TRUE. )

        write(*,*) " "
        write(*,*) " "
        write(*,*) " "
        write(*,*) "----------------------------------------------------------------"
        write(*,*) " "
        write(*,*) "                       Legendre Function"
        write(*,*) " "
        write(*,*) " z = ", z
        write(*,*) " "
        write(*,*) "----------------------------------------------------------------"
        write(*,1001) "n","Pn(z)","dPn(z)/dz","dPn(z)/dn","d2Pn(z)/dndz"
        write(*,*) "----------------------------------------------------------------"
        do i = 0, n
            write(*,1000) i, Pn(i), dPndz(i), dPndn(i), d2Pndzdn(i)
        enddo
        write(*,*) "----------------------------------------------------------------"

        1000 format(i5,4(f15.8))
        1001 format(a5,4(a15))

    End Subroutine

    Subroutine testHemiSphere(ka)
        Implicit None
        real(rp), intent(in) :: ka
        type(typHeavingHemiSphere) :: hSphere
        real(RP) :: radius, omega, k

        radius = 1.0_RP
        k      = ka / radius

        omega  = dsqrt(k * gravi)

        Call hSphere%initialize(radius, omega)

    End Subroutine

    Subroutine batchRunHemiSphere(radius, nkA, kAMin, kAMax)
        Implicit None
        integer,parameter :: iUnit = 101
        Real(RP), intent(in) :: radius, kAMin, kAMax
        integer, intent(in) :: nkA
        Type(typHeavingHemiSphere), dimension(nkA) :: hSphere

        Real(RP),dimension(nKA) :: addedMass, damping
        Real(RP) :: am, bm
        integer :: ik
        real(RP) :: kA, omega, dkA, k

        kA = kAMin
        dkA = (kAMax - kAMin) / (nKA - 1.0_RP)

        open(unit = iUnit, file="radiationForce.dat", status="replace")
        write(iUnit, "(a)") "# ik  ka   addedMass   Damping"

        do ik = 1, nKa
            k = kA / radius
            omega  = dsqrt(k * gravi)

            Call hSphere(ik)%initialize(radius, omega)
            Call hSphere(ik)%radiationForce(am, bm)

            addedMass(ik) = am
            damping(ik) = bm

            write(iUnit,1001) ik, ka, addedMass(ik), damping(ik), &
                               dreal(hSphere(ik)%multipoleAmp(1)), &
                               dimag(hSphere(ik)%multipoleAmp(1))

            kA = kA + dkA
        Enddo

        1001 format(i5, 99(e16.8))

    End Subroutine
