!!! ----------------------------------------------------------------------------
!!
!!  Module : modHeavingHemiSphere
!!
!!      Potential code for heaving hemisphere on the free surface
!!
!!      This code based on the Multipole expansion of Hulme (1982).
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

    Module modHeavingHemiSphere
        use modGlobal
        use modQuadraturePack
    Implicit None

    private

    !! - Parameter

        !! Multipole Order (It should have a large value if hemisphere has a
        !!                  high frequency motion)
        Integer, parameter :: mMax = 35

        !! Multipole Order 2 (nMax >=mMax).
        !!   It is recommended to use same value of mMax
        Integer, parameter :: nMax = 35

    !! - Type (Class)

        !! Heaving Hemisphere Type
        Include "heavingHemiSphere.typ"

    !! Test Subroutines

        !! Test integrals of Legendre related functions
        public :: testImn, testJ2m

        !! Test di-gamma functions
        public :: testDiGamma, testDiGamma05

        !! Test Legendre function and factorial
        public :: testPnl, factorial

        !! Test Heaving Hemisphere Type
        public :: testHemiSphere

        !! Run Batch cases (Added Mass and Damping)
        public :: batchRunHemiSphere

        !! Integral of Pm(x) * Pn(x) * log((x+1)/2)
        private :: IntPmPnlog

    contains

        !! Auxiliary functions needed for heaving hemisphere
        Include "auxiliary.inc"

        !! Heaving Hemisphre Type(Class) function
        Include "heavingHemiSphere.inc"

        !! Test script for testing functions
        Include "testHeavingHemiSphere.inc"

    End Module
