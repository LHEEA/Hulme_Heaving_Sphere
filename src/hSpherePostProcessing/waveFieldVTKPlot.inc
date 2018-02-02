!!! ----------------------------------------------------------------------------
Subroutine runHsphereVisual(radius, omega, motionAmp, surfFilePath, fieldFilePath)
!!! ----------------------------------------------------------------------------
!!
!!  Solve Heaving hemisphere problem and Visualize wave field
!!
!!! ----------------------------------------------------------------------------
    Implicit None
    real(rp), intent(in) :: radius, omega, motionAmp
    Character(len=CHAR_LEN),intent(in) :: surfFilePath, fieldFilePath
    !! Local Variables
    type(typHeavingHemiSphere) :: hSphere
    Character(len=CHAR_LEN)    :: outFolderName
    integer ::nTime, nPeriod

    !! Output folder name
    outFolderName = 'VTK'

    !! Delete VTK Folder and re-make
    CALL System("rm -rf "//trim(outFolderName))
    CALL System("mkdir -p "//trim(outFolderName))

    !! Solve heaving hemiSphere
    Call hSphere%initialize(radius, omega, motionAmp)

    !! Number of Time step to write
    nTime   = 60

    !! Number of period to write
    nPeriod = 1

    !! Write Wave Field
    Call makeWaveFieldVTK(hSphere, surfFilePath, fieldFilePath, outFolderName, nTime, nPeriod)

End Subroutine
!!! ----------------------------------------------------------------------------

!!! ----------------------------------------------------------------------------
Subroutine makeWaveFieldVTK(hSphere, surfFilePath, fieldFilePath, outFolderName, nTime, nPeriod)
!!! ----------------------------------------------------------------------------
!!
!!  Write Wave Field for Visualization
!!
!!! ----------------------------------------------------------------------------
    use modAuxiliaryFunction
    Implicit None
    INTEGER, Parameter :: iUnit = 211
    Integer, Parameter :: nR = 11, nZ = 11
    Type(typHeavingHemiSphere), intent(in)  :: hSphere
    Character(len=CHAR_LEN), intent(in)     :: surfFilePath, fieldFilePath
    Character(len=CHAR_LEN), intent(in)     :: outFolderName
    integer, intent(in)                     :: nTime, nPeriod
    !! Local Variables --------------------------------------
    Type(typSphereSurfMesh) :: sphereMesh
    type(typFieldMesh)      :: freeSurfMesh, fieldMesh
    integer                 :: iPanel, iNode
    real(rp)                :: x, y, z
    Character(len=CHAR_LEN) :: vtkFilePath, suffix
    integer                 :: iTime, i, nConnect,j, nT
    real(rp)                :: period, omega, dt, timePhase
    real(rp)                :: motionAmp, heaveMotion
    real(rp),allocatable    :: orgZ(:), time(:)
    real(rp),allocatable    :: elevation(:,:), u(:,:), v(:,:), w(:,:), p(:,:)
    !! -------------------------------------- Local Variables

    omega     = hSphere%omega
    period    = 2.0_RP * pi / omega
    dt        = period / real(nTime, rp)
    motionAmp = hSphere%motionAmp

    !! Read Free Surface Mesh
    Call readMeshUCD(surfFilePath, freeSurfMesh)

    !! Read Field Surface Mesh
    Call readMeshUCD(fieldFilePath, fieldMesh)

    nT = nTime * nPeriod
    allocate(time(nT))

    !! Write Sphere VTK file
    do iTime = 1,  nT
        time(iTime) = (iTime - 1.0_RP) * dt
    enddo

    !! Time Varying Value (Real Value)
    Allocate(elevation(freeSurfMesh%nNode, nT), &
             u(fieldMesh%nNode, nT), &
             v(fieldMesh%nNode, nT), &
             w(fieldMesh%nNode, nT), &
             p(fieldMesh%nNode, nT))

     !! Compute Wave Elevation Amplitudes
    do iNode = 1, freeSurfMesh%nNode
        x = freeSurfMesh%node(iNode)%x()
        y = freeSurfMesh%node(iNode)%y()
        z = freeSurfMesh%node(iNode)%z()

        ! freeSurfMesh%nodeElevation(iNode) = hSphere%waveAmplitude(x, y, z)
        Call hSphere%waveElevation(x, y, z, time, elevation(iNode, :))
    Enddo

    !! Compute Velocity Amplitudes
    do iNode = 1, fieldMesh%nNode
        x = fieldMesh%node(iNode)%x()
        y = fieldMesh%node(iNode)%y()
        z = fieldMesh%node(iNode)%z()

        Call hSphere%fluidsVelocity(x, y, z, &
                              time, &
                              u(iNode, :), &
                              v(iNode, :), &
                              w(iNode, :))

        Call hSphere%pressure(x, y, z, &
                              time, &
                              p(iNode, :))

    Enddo

    !! Make Sphere Mesh to visualization

    Call sphereMesh%initialize(hSphere%a, nR, nZ)
    suffix = "vtk"

    Allocate(orgZ(sphereMesh%nNode))
    do i = 1, sphereMesh%nNode
        orgZ(i) = sphereMesh%node(i)%z()
    enddo

    time = 0.0_RP
    !! Write Sphere VTK file
    do iTime = 1, nTime * nPeriod

        timePhase = omega * time(iTime)

        vtkFilePath = trim(outFolderName)//"/sphere_"//adjustl(int2str(iTime))

        heaveMotion = motionAmp * dcos(timePhase)

        do i = 1, sphereMesh%nNode
            x = sphereMesh%node(i)%x()
            y = sphereMesh%node(i)%y()
            z = orgZ(i) + heaveMotion
            Call sphereMesh%node(i)%move(x, y, z)
        enddo

        Call sphereMesh%writeVTK(vtkFilePath, suffix)

        time = time + dt
    enddo

    !! Write Wave Elevation VTK File
    do iTime = 1, nT

        vtkFilePath = trim(outFolderName)//"/waveElevation_"//adjustl(int2str(iTime))
        vtkFilePath = trim(vtkFilePath)//".vtk"

        open(iUnit, &
             file = trim(vtkFilePath), &
             status='replace')

        write(iUnit,1001) "# vtk DataFile Version 3.0"
        write(iUnit,1001) "vtk output"
        write(iUnit,1001) "ASCII"
        write(iUnit,1001) "DATASET UNSTRUCTURED_GRID"

        write(iUnit,1002) "POINTS ", freeSurfMesh%nNode, " float"

        do i = 1, freeSurfMesh%nNode
        write(iUnit,1003) freeSurfMesh%node(i)%x(), &
                          freeSurfMesh%node(i)%y(), &
                          freeSurfMesh%node(i)%z()
        enddo

        nConnect = 0
        do i =1, freeSurfMesh%nPanel
            nConnect = nConnect + freeSurfMesh%panel(i)%nNode() + 1
        enddo

        write(iUnit,1004) "CELLS ", freeSurfMesh%nPanel, nConnect
        do i = 1, freeSurfMesh%nPanel
        write(iUnit,1005) freeSurfMesh%panel(i)%nNode(), &
                          (freeSurfMesh%panel(i)%nodeLabel(j) - 1, j = 1,freeSurfMesh%panel(i)%nNode())
        enddo

        write(iUnit,1002) "CELL_TYPES ", freeSurfMesh%nPanel
        do i = 1, freeSurfMesh%nPanel
        write(iUnit,1006) freeSurfMesh%panel(i)%vtkCellType()
        enddo

        write(iUnit,1002) "POINT_DATA ", freeSurfMesh%nNode
        write(iUnit,1001) "SCALARS waveElevation float"
        write(iUnit,1001) "LOOKUP_TABLE default"
        do i = 1, freeSurfMesh%nNode
         write(iUnit,1003) elevation(i, iTime)
        enddo

        write(iUnit,1001) "VECTORS waveElevation float"
        do i = 1, freeSurfMesh%nNode
        write(iUnit,1003) 0.0_RP, 0.0_RP, elevation(i, iTime)
        enddo
        Close(iUnit)
    Enddo


    time = 0.0_RP
    !! Write Velocity & Pressure VTK File
    do iTime = 1, nTime * nPeriod

        vtkFilePath = trim(outFolderName)//"/velocity_"//adjustl(int2str(iTime))
        vtkFilePath = trim(vtkFilePath)//".vtk"

        open(iUnit, &
             file = trim(vtkFilePath), &
             status='replace')

        write(iUnit,1001) "# vtk DataFile Version 3.0"
        write(iUnit,1001) "vtk output"
        write(iUnit,1001) "ASCII"
        write(iUnit,1001) "DATASET UNSTRUCTURED_GRID"

        write(iUnit,1002) "POINTS ", fieldMesh%nNode, " float"

        do i = 1, fieldMesh%nNode
        write(iUnit,1003) fieldMesh%node(i)%x(), &
                          fieldMesh%node(i)%y(), &
                          fieldMesh%node(i)%z()
        enddo

        nConnect = 0
        do i =1, fieldMesh%nPanel
            nConnect = nConnect + fieldMesh%panel(i)%nNode() + 1
        enddo

        write(iUnit,1004) "CELLS ", fieldMesh%nPanel, nConnect
        do i = 1, fieldMesh%nPanel
        write(iUnit,1005) fieldMesh%panel(i)%nNode(), &
                          (fieldMesh%panel(i)%nodeLabel(j) - 1, j = 1,fieldMesh%panel(i)%nNode())
        enddo

        write(iUnit,1002) "CELL_TYPES ", fieldMesh%nPanel
        do i = 1, fieldMesh%nPanel
        write(iUnit,1006) fieldMesh%panel(i)%vtkCellType()
        enddo

        write(iUnit,1002) "POINT_DATA ", fieldMesh%nNode
        write(iUnit,1001) "SCALARS velocityMagnitude float"
        write(iUnit,1001) "LOOKUP_TABLE default"
        do i = 1, fieldMesh%nNode
         write(iUnit,1003) dsqrt(u(i, iTime) * u(i, iTime) + v(i, iTime) * v(i, iTime) + w(i, iTime) * w(i, iTime) )
        enddo

        write(iUnit,1001) "SCALARS pressure float"
        write(iUnit,1001) "LOOKUP_TABLE default"
        do i = 1, fieldMesh%nNode
         write(iUnit,1003) p(i, iTime)
        enddo

        write(iUnit,1001) "VECTORS velocity float"
        do i = 1, fieldMesh%nNode
         write(iUnit,1003) u(i, iTime), v(i, iTime), w(i, iTime)
        enddo
        Close(iUnit)
    Enddo

    deallocate(orgZ)
    deallocate(elevation)
    deallocate(u, v, w)

    1001 format(a)
    1002 format(a,i15,a)
    1003 format(3(e15.7))
    1004 format(a,2(i15))
    1005 format(i5,99(i8))
    1006 format(i5)

End Subroutine
!!! ----------------------------------------------------------------------------

!!! ----------------------------------------------------------------------------
Subroutine readMeshUCD(filePath, surfMesh)
!!! ----------------------------------------------------------------------------
!!
!!  Read .ucd file to constuct wave field Information (free surface and field mesh)
!!
!!! ----------------------------------------------------------------------------
    Use modAuxiliaryFunction
    Implicit None
    INTEGER, Parameter :: iUnit = 201
    Character(len=CHAR_LEN) :: filePath
    Type(typFieldMesh), intent(out) :: surfMesh

    Character(len=CHAR_LEN) :: tmpLine
    integer :: nArg
    character(len = CHAR_LEN), allocatable :: cArg(:), scArg(:), lcArg(:)
    integer, allocatable :: iArg(:)
    real(RP),allocatable :: rArg(:)
    logical, allocatable :: numFlag(:)
    logical :: iflag
    integer :: io
    integer :: ii
    real(rp) :: x, y, z
    integer, dimension(4) :: nodeLabel

    !! Log

    write(*,"(a,a)") "    Read .ucd file : ", trim(filePath)
    write(*,*) " "

    open(unit = iUnit, &
         file = trim(filePath), &
         status = "old")

    !! Dummy Line
    read(iUnit, "(a)" , IOSTAT=io) tmpLine
    read(iUnit, "(a)" , IOSTAT=io) tmpLine
    read(iUnit, "(a)" , IOSTAT=io) tmpLine

    !! Number of Node and Panel
    read(iUnit, "(a)" , IOSTAT=io) tmpLine
    !!... Split Line to Argument
    Call splitLine(tmpLine, nArg, cArg, scArg, lcArg, iArg, rArg, numFlag, iflag)

    surfMesh%nNode  = iArg(1)
    surfMesh%nPanel = iArg(2)

    write(*,"(a,i5,i5)") "      - Number of Nodes, Panels : ", surfMesh%nNode, surfMesh%nPanel

    allocate(surfMesh%node(surfMesh%nNode), &
             surfMesh%panel(surfMesh%nPanel))

    !! Read Node
    do ii = 1, surfMesh%nNode
        !! Node Information
        read(iUnit, "(a)" , IOSTAT=io) tmpLine

        !!... Split Line to Argument
        Call splitLine(tmpLine, nArg, cArg, scArg, lcArg, iArg, rArg, numFlag, iflag)

        x = rArg(2)
        y = rArg(3)
        z = rArg(4)

        Call surfMesh%node(ii)%set(ii, x, y, z)
    End do

    !! Read Panel
    do ii = 1, surfMesh%nPanel
        !! Node Information
        read(iUnit, "(a)" , IOSTAT=io) tmpLine

        !!... Split Line to Argument
        Call splitLine(tmpLine, nArg, cArg, scArg, lcArg, iArg, rArg, numFlag, iflag)

        nodeLabel(1) = iArg(4)
        nodeLabel(2) = iArg(5)
        nodeLabel(3) = iArg(6)
        nodeLabel(4) = iArg(7)

        Call surfMesh%panel(ii)%set(4,surfMesh%node,nodeLabel)

    enddo

    write(*,"(a,i5)") "                                                ... Done"
    write(*,*) " "

    close(iUnit)

End Subroutine
!!! ----------------------------------------------------------------------------
