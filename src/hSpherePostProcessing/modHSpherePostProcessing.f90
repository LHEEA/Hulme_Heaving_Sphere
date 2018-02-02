!!! ----------------------------------------------------------------------------
!!
!!  Module : modHSpherePostProcessing
!!
!!      Post Processing Tool for heaving hemisphere
!!
!!  Author
!!
!!      YoungMyung Choi (Ecole Centrale de Nantes)
!!         youngmyung.choi@ec-nantes.fr
!!
!!! ----------------------------------------------------------------------------

    Module modHSpherePostProcessing
        use modGlobal
        use modBaseGeometry
        use modHeavingHemiSphere
        use modSurfMesh
    Implicit None

    private

        !! VTK Point Data Type (Class)
        Include "pointDataVTK.typ"

        !! field mesh Type (Class) for visualization
        Include "fieldMesh.typ"

        !! Solve Heaving Hemisphere and Visualize Wave Field
        Public :: runHsphereVisual

        !! Check Pressure Integrated Force with Analytical Values
        Public :: checkPressure

    contains

        !! VTK Point Data Type (Class) Function Script
        Include "pointDataVTK.inc"

        !! Wave Field Visualization Function Script
        Include "waveFieldVTKPlot.inc"

        !! Pressure Integrated Force Function Script
        Include "pressureValidation.inc"

    End Module
