!! ------------------------------------------------------------------ !!
!! Procedures : Type Point
!! ------------------------------------------------------------------ !!

    Subroutine setPoint(this, pidx, x, y, z)
        Implicit None
        class(typPoint), intent(inout) :: this
        Integer, intent(in)            :: pidx
        Real(RP),intent(in)            :: x, y, z
        this%idx_ = pidx
        this%x_ = x;         this%y_ = y;         this%z_ = z
        this%vec_(1) = x;    this%vec_(2) = y;    this%vec_(3) = z
    End Subroutine

    Subroutine movePoint(this, x, y, z)
        Implicit None
        class(typPoint), intent(inout) :: this
        Real(RP),intent(in)            :: x, y, z
        this%x_ = x;         this%y_ = y;         this%z_ = z
        this%vec_(1) = x;    this%vec_(2) = y;    this%vec_(3) = z
    End Subroutine

    Real(RP) Function pointX(this)
        Implicit None
        class(typPoint), intent(in) :: this
        pointX = this%x_
    End Function

    Real(RP) Function pointY(this)
        Implicit None
        class(typPoint), intent(in) :: this
        pointY = this%y_
    End Function

    Real(RP) Function pointZ(this)
        Implicit None
        class(typPoint), intent(in) :: this
        pointZ = this%z_
    End Function

    Function pointVec(this) result(vec)
        Implicit None
        class(typPoint), intent(in) :: this
        Real(RP), Dimension(3) :: vec
        vec = this%vec_
    End Function

!! ------------------------------------------------------------------ !!
!! End Procedures
!! ------------------------------------------------------------------ !!
