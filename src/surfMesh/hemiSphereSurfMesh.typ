!!*------------------------------------------------------------------*!!
!!  Project : mkSurfaceMesh
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Type
!!      typHemiSphereSurfMesh
!!
!!  Description
!!      typHemiSphereSurfMesh Type
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!                      Type typHemiSphereSurfMesh
!! ------------------------------------------------------------------ !!

    Type,extends(typSurfMesh), public :: typHemiSphereSurfMesh

!! Type Data -------------------------------------------------------- !!

    Private

        !! - Radius
        Real(RP) :: radius_

        !! - Nr, Nz
        Integer  :: nR_, nZ_

        !! - Origin
        Real(RP) :: x0_, y0_, z0_

!! Type Procedures -------------------------------------------------- !!

    contains

        !! Initialize Sphere Mesh
        procedure, pass, public :: initialize => initializeHemiSphereMesh

    End Type

!! ------------------------------------------------------------------ !!
!!                      Type typHemiSphereSurfMesh
!! ------------------------------------------------------------------ !!
