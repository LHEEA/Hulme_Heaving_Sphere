!!! ----------------------------------------------------------------------------
!!
!!  Module : modQuadraturePack
!!
!!      Quadrature Pack
!!
!!  Author
!!
!!      YoungMyung Choi (Ecole Centrale de Nantes)
!!         youngmyung.choi@ec-nantes.fr
!!
!!  REFERENCE
!!
!!      There is raw source code available in the web.
!!      But I cannot remember the source.
!!
!!! ----------------------------------------------------------------------------
    Module modQuadraturePack
        use modGlobal
    Implicit None

        !!!  3 - Point Gaussian Quadrature abscissa
        double precision, parameter :: auxGQ3W = DSQRT(0.6D0)

    contains

        !!! Adaptive Gaussian Quadrature script
        Include "adaptGauss3Quadrature.inc"

    End Module
