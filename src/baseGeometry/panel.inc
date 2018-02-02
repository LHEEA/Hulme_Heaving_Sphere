!! ------------------------------------------------------------------ !!
!! Procedures : Type Panel
!! ------------------------------------------------------------------ !!

    ! Subroutine setPanel(this, nNode, nodes, vec)
    Subroutine setPanel(this, nNode, nodes, nodeLabel, vec)
        Implicit None
        Class(typPanel), intent(inout)               :: this
        Integer, intent(in)                          :: nNode
        type(typPoint), intent(in)                   :: nodes(:)
        integer, dimension(nNode), intent(in)        :: nodeLabel
        real(rp),dimension(3), optional :: vec
        integer :: i

        if (nNode.le.1) then
            write(*,*) "    [ERROR] setPanel : nNode should be larger than 1."
            return
        endif

        this%nNode_ = nNode
        allocate( this%nodeLabel_(nNode) )

        this%vtkCellType_ = 7
        if (nNode.eq.2) this%vtkCellType_ = 3
        if (nNode.eq.3) this%vtkCellType_ = 5
        if (nNode.eq.4) this%vtkCellType_ = 9

        do i = 1, nNode
            this%nodeLabel_(i) = nodeLabel(i)
        enddo

        Call correctPanel(this, nodes, vec)

    End Subroutine

    Subroutine correctPanel(this, nodes, vec)
        Implicit None
        Class(typPanel), intent(inout)  :: this
        type(typPoint), intent(in)      :: nodes(:)
        real(rp),dimension(3), optional :: vec
        integer  :: i
        integer  :: iNode, jNode, kNode
        Real(RP) :: xc, yc, zc
        Real(RP),dimension(3) :: vec1, vec2, vec3, nVec, sumNvec
        Real(RP) :: magV, area
        Real(RP) :: sumArea

        xc = 0.0_RP; yc = 0.0_RP; zc = 0.0_RP

        do i = 1, this%nNode_
            xc = xc + nodes( this%nodeLabel_(i) )%x()
            yc = yc + nodes( this%nodeLabel_(i) )%y()
            zc = zc + nodes( this%nodeLabel_(i) )%z()
        enddo

        !! Panel Center
        xc = xc / real(this%nNode_, RP)
        yc = yc / real(this%nNode_, RP)
        zc = zc / real(this%nNode_, RP)

        Call this%center%set(-1, xc, yc, zc)

        if (this%nNode_.eq.2) then

            iNode = this%nodeLabel_(2)
            jNode = this%nodeLabel_(1)

            vec1(1) = nodes(jNode)%x() - nodes(iNode)%x()
            vec1(2) = nodes(jNode)%y() - nodes(iNode)%y()
            vec1(3) = nodes(jNode)%z() - nodes(iNode)%z()

            if (present(vec)) then
                vec2 = vec
            else
                write(*, *) "   [ERROR] setPanel: For line segment, "
                write(*, *) "           reference vector should be given."
            endif

            !! Cross Product
            vec3(1) = vec1(2) * vec2(3) - vec1(3) * vec2(2)
            vec3(2) = vec1(3) * vec2(1) - vec1(1) * vec2(3)
            vec3(3) = vec1(1) * vec2(2) - vec1(2) * vec2(1)

            magV = dsqrt(vec1(1)*vec1(1) + vec1(2)*vec1(2) + vec1(3)*vec1(3))

            !! Area
            this%area_ = magV

            magV = dsqrt(vec3(1)*vec3(1) + vec3(2)*vec3(2) + vec3(3)*vec3(3))
            vec3 = vec3 / magV
            Call this%normal%set(-1, vec3(1), vec3(2), vec3(3) )

        else if (this%nNode_.eq.3) then

            iNode = this%nodeLabel_(1)
            jNode = this%nodeLabel_(2)
            kNode = this%nodeLabel_(3)

            vec1(1) = nodes(jNode)%x() - nodes(iNode)%x()
            vec1(2) = nodes(jNode)%y() - nodes(iNode)%y()
            vec1(3) = nodes(jNode)%z() - nodes(iNode)%z()

            vec1(1) = nodes(kNode)%x() - nodes(jNode)%x()
            vec1(2) = nodes(kNode)%y() - nodes(jNode)%y()
            vec1(3) = nodes(kNode)%z() - nodes(jNode)%z()

            !! Cross Product
            vec3(1) = vec1(2) * vec2(3) - vec1(3) * vec2(2)
            vec3(2) = vec1(3) * vec2(1) - vec1(1) * vec2(3)
            vec3(3) = vec1(1) * vec2(2) - vec1(2) * vec2(1)

            !! Area
            magV = dsqrt(vec3(1)*vec3(1) + vec3(2)*vec3(2) + vec3(3)*vec3(3))
            this%area_ = 0.5_RP * magV

            !! Normal
            vec3 = vec3 / magV
            Call this%normal%set(-1, vec3(1) , vec3(2), vec3(3))

        else
        !! Panel normal vector and area
        sumNvec = 0.0_RP
        sumArea = 0.0_RP
        do i = 1, this%nNode_
            iNode = this%nodeLabel_(i)
            jNode = this%nodeLabel_(i + 1)
            if (i.eq.this%nNode_) jNode = this%nodeLabel_(1)

            vec1(1) = nodes(iNode)%x() - xc; vec1(2)  = nodes(iNode)%y() - yc; vec1(3)  = nodes(iNode)%z() - zc
            vec2(1) = nodes(jNode)%x() - xc; vec2(2)  = nodes(jNode)%y() - yc; vec2(3)  = nodes(jNode)%z() - zc

            !! Cross Product
            vec3(1) = vec1(2) * vec2(3) - vec1(3) * vec2(2)
            vec3(2) = vec1(3) * vec2(1) - vec1(1) * vec2(3)
            vec3(3) = vec1(1) * vec2(2) - vec1(2) * vec2(1)

            !! Area
            magV = dsqrt(vec3(1)*vec3(1) + vec3(2)*vec3(2) + vec3(3)*vec3(3))

            area = 0.5_RP * magV
            nVec = vec3 / magV

            sumArea = sumArea + area
            sumNvec = sumNvec + area * nVec

        enddo
        !! Area weighed normal
        sumNvec = sumNvec / sumArea
        !! Area
        this%area_   = sumArea

        Call this%normal%set(-1, sumNvec(1) , sumNvec(2), sumNvec(3))

        end if

    End Subroutine

    Integer Function nNodePanel(this)
        Implicit None
        class(typPanel), intent(in) :: this
        nNodePanel = this%nNode_
    End Function

    Integer Function panelNodeLabel(this, index)
        Implicit None
        class(typPanel), intent(in) :: this
        integer,intent(in) :: index
        panelNodeLabel = -1
        if ((index.le.0).or.(index.gt.this%nNode_)) then
            write(*,*) "    [Error] nodeLabel : Wrong Panel Node Index"
            write(*,*) "        Input index, nNode : ", index, this%nNode_
        else
            panelNodeLabel = this%nodeLabel_(index)
        end if
    End Function

    Real(RP) Function panelArea(this)
        Implicit None
        class(typPanel), intent(in) :: this
        panelArea = this%area_
    End Function

    Integer Function panelVTKCellType(this)
        Implicit None
        class(typPanel), intent(in) :: this
        panelVTKCellType = this%vtkCellType_
    End Function

    Subroutine assignCopyPanel(to, from)
        Implicit None
        class(typPanel), intent(inout) :: to
        class(typPanel), intent(in)    :: from

        to%nNode_ = from%nNode_

        if (to%nNode_.ge.1) then
            allocate(to%nodeLabel_(to%nNode_))
            to%nodeLabel_ = from%nodeLabel_
        end if

        to%vtkCellType_ = from%vtkCellType_
        to%area_        = from%area_
        to%center       = from%center
        to%normal       = from%normal

    End Subroutine

    Subroutine destroyPanel(this)
        Implicit None
        Class(typPanel), intent(inout) :: this
        if (allocated(this%nodeLabel_)) deallocate(this%nodeLabel_)
        this%nNode_       = 0
        this%vtkCellType_ = -1
        this%area_        = 0
    End Subroutine

    Subroutine finalPanel(this)
        Implicit None
        Type(typPanel), intent(inout) :: this
        Call this%destroy()
    End Subroutine

!! ------------------------------------------------------------------ !!
!! End Procedures
!! ------------------------------------------------------------------ !!
