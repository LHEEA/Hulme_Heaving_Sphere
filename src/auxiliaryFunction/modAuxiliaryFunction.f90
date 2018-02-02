!! -----------------------------------------------------------------------------
!!
!!  Module : modAuxiliaryFunction
!!
!!      Contiains auxiliary functions for Fortran Programming
!!
!!  Written by YoungMyung Choi (Ecole Centrale de Nantes)
!!  mail to : youngmyung.choi@ec-nantes.fr
!!
!!  Some subroutines are downloaded from Dr. George Benthiem website
!!
!!       See. http://gbenthien.net/about.html
!!
!! -----------------------------------------------------------------------------
    Module modAuxiliaryFunction
        Use modGlobal
    Implicit None

    private

        !!... Split Line
        ! splitLine(str, nArg, cArg, scArg, lcArg, iArg, rArg, numFlag, iflag)
        public :: splitLine

        !!.. isAllTrue
        ! isAllTrue(logicalArray)
        public :: isAllTrue

        !!.. Data Convertion (character to integer)
        public :: str2int

        !!.. Data Convertion (integer to character)
        public :: int2str

        !!... Change character to Upper Character
        public :: lower

        !!... Change Character to Lower Character
        public :: upper

    Contains

        !! Split Line with space
        Include "splitLine.inc"

        !! Data Conversion
        Include "dataConversion.inc"

    End Module
