!!! ----------------------------------------------------------------------------
Subroutine checkPressure(radius, omega, motionAmp, nR, nZ)
!!! ----------------------------------------------------------------------------
!!
!!  Commpare Pressure Integrated Force on Heaving Hemisphere with Analytic Values
!!
!!! ----------------------------------------------------------------------------
    Implicit None
    real(rp), intent(in) :: radius, omega, motionAmp
    integer, intent(in)  :: nR, nZ
    !! Local Variables --------------------------------------
    type(typHeavingHemiSphere) :: hSphere
    Type(typHemiSphereSurfMesh):: hSphereMesh
    Complex(RP)                :: localPressureAmp
    Complex(RP)                :: totalForceAmp

    real(rp) :: x, y, z
    integer  :: iPanel

    real(rp) :: volume

    !! Solve heaving hemiSphere
    Call hSphere%initialize(radius, omega, motionAmp)

    !! Make Hemisphere Mesh
    Call hSphereMesh%initialize(radius, nR, nZ)

    totalForceAmp = (0.0_RP, 0.0_RP)

    do iPanel = 1, hSphereMesh%nPanel
        x = hSphereMesh%Panel(iPanel)%center%x()
        y = hSphereMesh%Panel(iPanel)%center%y()
        z = hSphereMesh%Panel(iPanel)%center%z()

        Call hSphere%pressureAmp(x, y, z, localPressureAmp)

        totalForceAmp = totalForceAmp + localPressureAmp * hSphereMesh%Panel(iPanel)%area() &
                                                         * hSphereMesh%Panel(iPanel)%normal%z()

    enddo

    volume = 2.0_RP / 3.0_RP * pi * radius**3
    totalForceAmp = totalForceAmp / rhoWater / volume / motionAmp / omega / omega

    write(*,"(a,f15.8)") "       - Direct Added Mass       [-] : ", real(totalForceAmp)
    write(*,"(a,f15.8)") "       - Direct Damping          [-] : ", imag(totalForceAmp)
    write(*,*) " "

End Subroutine
!!! ----------------------------------------------------------------------------
