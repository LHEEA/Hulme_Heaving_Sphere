    Subroutine initializePointDataVTK(this, nPoint)
        Implicit None
        Class(typPointDataVTK), intent(inout)   :: this
        integer, intent(in)                     :: nPoint

        this%nPoint = nPoint
        this%isScalarData = .FALSE.
        this%isVectorData = .FALSE.

    End Subroutine

    Subroutine setVTKPointScalarData(this, scalarData)
        Class(typPointDataVTK), intent(inout)   :: this
        Real(RP), dimension(:), intent(in)      :: scalarData
        integer :: nInput, i

        nInput = sizeof(scalarData) / sizeof(1.0_RP)

        if (nInput.eq.this%nPoint) then
            this%isScalarData = .TRUE.
            if (allocated(this%scalarData).EQV..FALSE.) then
                allocate(this%scalarData(this%nPoint))
            end if

            this%scalarData = scalarData

        else
            write(*,*) "    [WARNING] setVTKPointScalarData(scalarData)"
            write(*,*) "        Number of Data is not same with number of point."
            write(*,*) "        Given scalarData is not saved."
        end if

    End Subroutine

    Subroutine setVTKPointVectorData(this, vectorData)
        Class(typPointDataVTK), intent(inout)   :: this
        Real(RP), dimension(:,:), intent(in)    :: vectorData
        integer :: nInput, i

        nInput = sizeof(vectorData) / sizeof(1.0_RP) / 3

        if (nInput.eq.this%nPoint) then
            this%isVectorData = .TRUE.
            if (allocated(this%vectorData).EQV..FALSE.) then
                allocate(this%vectorData(this%nPoint, 3))
            end if

            this%vectorData = vectorData

        else
            write(*,*) "    [WARNING] setVTKPointVectorData(scalarData)"
            write(*,*) "        Number of Data is not same with number of point."
            write(*,*) "        Given vectorData is not saved."
        end if

    End Subroutine

    Subroutine removeScalarData(this)
        Implicit None
        Class(typPointDataVTK), intent(inout)   :: this
        this%isScalarData = .FALSE.
        if (allocated(this%scalarData)) deallocate(this%scalarData)
    End Subroutine

    Subroutine removeVectorData(this)
        Implicit None
        Class(typPointDataVTK), intent(inout)   :: this
        this%isVectorData = .FALSE.
        if (allocated(this%vectorData)) deallocate(this%vectorData)
    End Subroutine
