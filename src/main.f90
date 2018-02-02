!!! ----------------------------------------------------------------------------
    Program Main
!!! ----------------------------------------------------------------------------
!   Copyright (C) 2017 - LHEEA Lab., Ecole Centrale de Nantes, UMR CNRS 6598
!
!   This program is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   This program is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with this program.  If not, see <https://www.gnu.org/licenses/>.

    !! Modules

        Use modGlobal
        Use modHeavingHemiSphere
        use modHSpherePostProcessing

!!! ----------------------------------------------------------------------------

    !! Declaration -------------------------------------------------------------

    Implicit None

        !! Heaving Hemisphere Type(Class)
        type(typHeavingHemiSphere) :: hSphere

        !! Radius of Heaving Hemisphere
        Real(RP) :: radius

        !! Moving Frequency [rad/s]
        Real(RP) :: omega

        !! Moving Amplitude [m]
        Real(RP) :: motionAmp

        !! kAMin and kAMax and nKA for batch run
        Real(RP) :: kAMin, kAMax
        integer  :: nkA

        !! surface and field mesh for visualization
        Character(CHAR_LEN) :: surfFilePath, fieldFilePath

        !! Number of the surface Mesh of the heaving hemisphere for Pressure Check
        integer :: nR, nZ

    !!! ------------------------------------------------------------------------

        !! Solve single frequency ----------------------------------

        radius    = 1.0_RP
        omega     = 3.0_RP
        motionAmp = 1.0_RP

        Call hSphere%initialize(radius, omega, motionAmp)

        !! ---------------------------------- Solve single frequency

        !!! Batch Run ----------------------------------------------

        radius = 1.0_RP
        kAMin  = 0.0000000001_RP
        kAMax  = 10.0_RP

        nKA = 501

        Call batchRunHemiSphere(radius, nkA, kAMin, kAMax)

        !!! ---------------------------------------------- Batch Run

        !!! Wave Field visualization -------------------------------

        radius    = 1.0_RP
        omega     = 5.0_RP
        motionAmp = 0.1_RP
        surfFilePath  = "freeSurface.ucd"
        fieldFilePath = "fieldSurface_X.ucd"
        ! fieldFilePath = "fieldSurface_Y.ucd"

        Call runHsphereVisual(radius, omega, motionAmp, surfFilePath, fieldFilePath)

        !!! ------------------------------- Wave Field visualization

        !!! Check Pressure -----------------------------------------

        radius    = 2.0_RP
        omega     = 7.0_RP
        motionAmp = 0.5_RP

        nR = 71
        nZ = 31

        Call checkPressure(radius, omega, motionAmp, nR, nZ)

        !!! ----------------------------------------- Check Pressure

    End Program
