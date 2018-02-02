!! ------------------------------------------------------------------ !!
!! Procedures : Type surfMesh
!! ------------------------------------------------------------------ !!

    Subroutine writeVTK(this, filePath, suffix)
        Implicit None
        class(typSurfMesh), intent(inout) :: this
        Character(len=CHAR_LEN),intent(in) :: filePath
        Character(len=CHAR_LEN),intent(in) :: suffix
        Character(len=CHAR_LEN) :: surfMeshPath, lineMeshPath

        surfMeshPath = trim(filePath)//"."//trim(suffix)
        lineMeshPath = trim(filePath)//"_line."//trim(suffix)

        Call writeVTKSurfMesh(this, surfMeshPath)

        if (this%isOpenTop) then
        Call writeVTKLineMesh(this, lineMeshPath)
        end if

    End Subroutine

    Subroutine writeVTKSurfMesh(this, filePath)
        Implicit None
        class(typSurfMesh), intent(inout) :: this
        Character(len=CHAR_LEN)           :: filePath
        integer :: ioUnit
        integer :: i,j
        Integer :: nConnect

        ioUnit = FIO_UNIT + 1

        Open(unit   = ioUnit,   &
             file   = trim(filePath), &
             status = "replace")

        write(ioUnit,1001) "# vtk DataFile Version 3.0"
        write(ioUnit,1001) "vtk output"
        write(ioUnit,1001) "ASCII"
        write(ioUnit,1001) "DATASET UNSTRUCTURED_GRID"

        write(ioUnit,1002) "POINTS ", this%nNode, " float"
        do i = 1, this%nNode
        write(ioUnit,1003) this%node(i)%x(), this%node(i)%y(), this%node(i)%z()
        enddo

        nConnect = 0
        do i =1, this%nPanel
            nConnect = nConnect + this%panel(i)%nNode() + 1
        enddo

        write(ioUnit,1004) "CELLS ", this%nPanel, nConnect
        do i = 1, this%nPanel
        write(ioUnit,1005) this%panel(i)%nNode(), &
                          (this%panel(i)%nodeLabel(j) - 1, j = 1,this%panel(i)%nNode())
        enddo

        write(ioUnit,1002) "CELL_TYPES ", this%nPanel
        do i = 1, this%nPanel
        write(ioUnit,1006) this%panel(i)%vtkCellType()
        enddo

        write(ioUnit,1002) "CELL_DATA ", this%nPanel
        write(ioUnit,1001) "SCALARS Panel_Area float"
        write(ioUnit,1001) "LOOKUP_TABLE default"
        do i = 1, this%nPanel
        write(ioUnit,1003) this%panel(i)%area()
        enddo
        write(ioUnit,1001) "NORMALS Panel_Normal float"
        do i = 1, this%nPanel
        write(ioUnit,1003) this%panel(i)%normal%x(), this%panel(i)%normal%y(), this%panel(i)%normal%z()
        enddo

        Close(ioUnit)

        1001 format(a)
        1002 format(a,i15,a)
        1003 format(3(e15.7))
        1004 format(a,2(i15))
        1005 format(i5,99(i8))
        1006 format(i5)

    End Subroutine

    Subroutine writeVTKLineMesh(this, filePath)
        Implicit None
        class(typSurfMesh), intent(inout) :: this
        Character(len=CHAR_LEN)           :: filePath
        integer :: ioUnit
        integer :: i,j
        Integer :: nConnect

        ioUnit = FIO_UNIT + 1

        Open(unit   = ioUnit,   &
             file   = trim(filePath), &
             status = "replace")

        write(ioUnit,1001) "# vtk DataFile Version 3.0"
        write(ioUnit,1001) "vtk output"
        write(ioUnit,1001) "ASCII"
        write(ioUnit,1001) "DATASET UNSTRUCTURED_GRID"

        write(ioUnit,1002) "POINTS ", this%nLineNode, " float"
        do i = 1, this%nLineNode
        write(ioUnit,1003) this%lineNode(i)%x(), this%lineNode(i)%y(), this%lineNode(i)%z()
        enddo

        write(ioUnit,1004) "CELLS ", this%nLine, this%nLine * 3
        do i = 1, this%nLine
        write(ioUnit,1005) this%linePanel(i)%nNode(),           &
                           (this%linePanel(i)%nodeLabel(j)-1, j = 1, this%linePanel(i)%nNode())
        enddo

        write(ioUnit,1002) "CELL_TYPES ", this%nLine
        do i = 1, this%nLine
        write(ioUnit,1006) this%linePanel(i)%vtkCellType()
        enddo

        write(ioUnit,1002) "CELL_DATA ", this%nLine
        write(ioUnit,1001) "SCALARS Panel_Area float"
        write(ioUnit,1001) "LOOKUP_TABLE default"
        do i = 1, this%nLine
        write(ioUnit,1003) this%linePanel(i)%area()
        enddo
        write(ioUnit,1001) "NORMALS Panel_Normal float"
        do i = 1, this%nLine
        write(ioUnit,1003) this%linePanel(i)%normal%x(), this%linePanel(i)%normal%y(), this%linePanel(i)%normal%z()
        enddo

        Close(ioUnit)

        1001 format(a)
        1002 format(a,i15,a)
        1003 format(3(e15.7))
        1004 format(a,2(i15))
        1005 format(i5,4(i8))
        1006 format(i5)

    End Subroutine

    Subroutine assignCopySurfMesh(to, from)
        class(typSurfMesh), intent(inout) :: to
        class(typSurfMesh), intent(in)    :: from

        to%isOpenTop = from%isOpenTop
        to%nNode     = from%nNode
        to%nLineNode = from%nLineNode
        to%nPanel    = from%nPanel
        to%nLine     = from%nLine
        to%nLineSub  = from%nLineSub

        if (to%nNode.gt.0) then
            allocate(to%node( to%nNode ))
            to%node = from%node
        end if

        if (to%nPanel.gt.0) then
            allocate(to%panel( to%nPanel ))
            to%panel = from%panel
        end if

        if (to%nLineNode.gt.0) then
            allocate(to%lineNode( to%nLineNode ))
            to%lineNode = from%lineNode
        endif

        if (to%nLine.gt.0) then
            allocate(to%linePanel( to%nLine ))
            to%linePanel = from%linePanel
        end if

    End Subroutine

    Subroutine destroySurfMesh(this)
        Implicit None
        Class(typSurfMesh), intent(inout) :: this
        integer :: i

        ! if (allocated(this%panel)) then
        !     do i = 1, this%nPanel
        !         Call this%panel(i)%destroy()
        !     enddo
        !     deallocate(this%panel)
        ! end if
        !
        ! if (allocated(this%linePanel)) then
        !     do i = 1, this%nPanel
        !         Call this%linePanel(i)%destroy()
        !     enddo
        !     deallocate(this%linePanel)
        ! end if

        this%isOpenTop = .FALSE.
        this%nNode     = 0;     this%nLineNode = 0;     this%nPanel     = 0;
        this%nLine = 0;         this%nLineSub = 0;

    End Subroutine

    Subroutine finalSurfMesh(this)
        Implicit None
        Type(typSurfMesh), intent(inout) :: this
        Call this%destroy()
    End Subroutine

!! ------------------------------------------------------------------ !!
!! End Procedures
!! ------------------------------------------------------------------ !!
