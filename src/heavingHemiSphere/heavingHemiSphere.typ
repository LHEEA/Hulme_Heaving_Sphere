!!! ----------------------------------------------------------------------------
!!
!!  Type : typHeavingHemiSphere
!!
!!      Heaving Hemisphere Type(Class) based on Hulme's Formulation
!!
!!  Author
!!
!!      YoungMyung Choi (Ecole Centrale de Nantes)
!!         youngmyung.choi@ec-nantes.fr
!!
!!  References
!!
!!      Hulme A.,1982,"The wave forces acting on a floating hemisphere undergoing
!!         forced periodic oscillation", J. Fluid Mech.,121,pp.443-463
!!
!!! ----------------------------------------------------------------------------

    Type, public :: typHeavingHemiSphere

    private

    !! Data -------------------------------------------

        !! Radius
        Real(RP), public :: a

        !! Moving Frequency
        Real(RP), public :: omega

        !! Wave Number
        Real(RP) :: k

        !! k * a
        Real(RP) :: ka

        !! Motion Amplitude
        real(rp),public :: motionAmp

        !! Velocity Amplitude
        complex(rp)     :: velocityAmp

        !! Magnification Factor (wave pole amplitude)
        Complex(RP) :: c0

        !! Multipole Amplitudes Vector
        Complex(RP),dimension(mMax) :: multipoleAmp

        !! Radiation Force
        Complex(RP) :: rForce

        !! Added Mass
        Real(RP) :: addedMass

        !! Damping
        Real(RP) :: damping

    contains

        !! Initialize and Solve Multipole
        procedure, pass, public :: initialize => initializeHeavingHemiSphere

        !! Construct System Matrix and Solve
        procedure, pass         :: constructModeMatrix

        !! Compute Addded mass and damping
        procedure, pass, public :: ComputeRadiationForce

        !! Added mass and damping
        procedure, pass, public :: radiationForce

        !! Transform Coordinates
        procedure, pass         :: transCoord

        !! Velocit Potential
        procedure, pass, public :: phi => hSpherePhi

        !! Gradient of Velocity Potential
        procedure, pass, public :: gradPhi => hSphereGradPhi

        !! Complex Wave Elevation Time Series
        procedure, pass, public :: waveElevation => hSphereZetaTime

        !! Complex Fluids Velocity Time Series
        procedure, pass, public :: fluidsVelocity => hSphereU

        !! Complex Pressure Time Series
        procedure, pass, public :: pressure => hSpherePressureTime

        !! Complex Wave Amplitudes
        procedure, pass, public :: waveElevationAmp => hSphereZetaAmp

        !! Complex Fluids Velocity Amplitudes
        procedure, pass, public :: fluidsVelocityAmp => hSphereUAmp

        !! Complex Pressure Amplitudes
        procedure, pass, public :: pressureAmp => hSpherePressureAmp

        !! Exp(-i * omega * t)
        procedure, pass         :: expTime

    End Type
