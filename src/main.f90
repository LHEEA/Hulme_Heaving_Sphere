!!! ----------------------------------------------------------------------------
    Program Main
!!! ----------------------------------------------------------------------------

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
