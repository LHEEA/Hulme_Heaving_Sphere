!!*------------------------------------------------------------------*!!
!!  Project : mkSurfaceMesh
!!*------------------------------------------------------------------*!!
!!
!!  Fortran Module
!!      baseGeometry
!!
!!  Description
!!      This module holds basic geometry related type.
!!
!!  Authors
!!      YoungMyung Choi, Ecole Centrale de Nantes
!!          - youngmyung.choi@ec-nantes.fr
!!
!!*------------------------------------------------------------------*!!

!! ------------------------------------------------------------------ !!
!!                      Module modBaseGeometry
!! ------------------------------------------------------------------ !!

    Module modBaseGeometry

!! ------------------------------------------------------------------ !!

        Use modGlobal

!! ------------------------------------------------------------------ !!
    Implicit None
!! ------------------------------------------------------------------ !!

!! Header File - Type Data ------------------------------------------ !!

    Private

        !! - Point Type
        Include "point.typ"

        !! - Panel Type
        Include "panel.typ"


!! Procedures File -------------------------------------------------- !!

    Contains

        !! - Point Procedures
        Include "point.inc"

        !! - Panel Procedures
        Include "panel.inc"

    End Module

!! ------------------------------------------------------------------ !!
!!                      Module modBaseGeometry
!! ------------------------------------------------------------------ !!
