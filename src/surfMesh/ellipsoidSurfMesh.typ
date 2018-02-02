!!*------------------------------------------------------------------*!!
!!  Project : mkSurfaceMesh
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Type
!!      typEllipsoidSurfMesh
!!
!!  Description
!!      typEllipsoidSurfMesh Type
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!                      Type typEllipsoidSurfMesh
!! ------------------------------------------------------------------ !!

    Type,extends(typSurfMesh), public :: typEllipsoidSurfMesh

!! Type Data -------------------------------------------------------- !!

    Private

        !! - Radius
        Real(RP) :: rX_, rY_, rZ_

        !! - Nr, Nz
        Integer  :: nX_, nY_, nZ_

        !! - Origin
        Real(RP) :: x0_, y0_, z0_

!! Type Procedures -------------------------------------------------- !!

    contains

        !! Initialize Sphere Mesh
        procedure, pass, public :: initialize => initializeEllipsoidSurfMesh

    End Type

!! ------------------------------------------------------------------ !!
!!                      Type typSphereSurfMesh
!! ------------------------------------------------------------------ !!
