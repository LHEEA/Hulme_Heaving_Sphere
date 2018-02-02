!!*------------------------------------------------------------------*!!
!!  Project : mkSurfaceMesh
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Module
!!      surfMesh
!!
!!  Description
!!      Generates simple surface mesh with user given parameters
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!                      Module modSurfMesh
!! ------------------------------------------------------------------ !!

    Module modSurfMesh

!! ------------------------------------------------------------------ !!

        Use modGlobal
        Use modBaseGeometry

!! ------------------------------------------------------------------ !!
    Implicit None
!! ------------------------------------------------------------------ !!

!! Header File - Type Data ------------------------------------------ !!

    Private

        !! surfMesh type - Base Type
        Include 'surfMesh.typ'

        !! paraPipe SurfMesh type - Derived Type
        Include 'paraPipeSurfMesh.typ'

        !! sphere SurfMesh type - Derived Type
        Include 'sphereSurfMesh.typ'

        !! hemi-Sphere SurfMesh type - Derived Type
        Include 'hemiSphereSurfMesh.typ'

        !! ellipsoid SurfMesh type - Derived Type
        Include 'ellipsoidSurfMesh.typ'

        !! semi Ellipsoid SurfMesh type - Derived Type
        Include 'semiEllipsoidSurfMesh.typ'

!! Procedures File -------------------------------------------------- !!

    Contains

        !! - surfMesh Procedures
        Include 'surfMesh.inc'

        !! - paraPipe SurfMesh Procedures
        Include 'paraPipeSurfMesh.inc'

        !! - sphere SurfMesh Procedures
        Include 'sphereSurfMesh.inc'

        !! - hemi-Sphere SurfMesh Procedures
        Include 'hemiSphereSurfMesh.inc'

        !! - ellipsoid SurfMesh Procedures
        Include 'ellipsoidSurfMesh.inc'

        !! semi Ellipsoid SurfMesh type - Derived Type
        Include 'semiEllipsoidSurfMesh.inc'

    End Module

!! ------------------------------------------------------------------ !!
!!                      Module modSurfMesh
!! ------------------------------------------------------------------ !!
