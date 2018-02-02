!---------------------------------------------------------------------72
      Subroutine adaptGQ_dFunc_a(func, a, lb, ub, y, tol)
!---------------------------------------------------------------------72
!
!     SUBROUTINE ADAPT_GQ3
!     APPROXIMATE THE DEFINITE INTEGRAL OF AN ARBITRARY FUNCTION TO
!     WHITHIN A SPECIFIED ERROR TOLERANCE USING ADAPTIVE QUADRATURE
!     BASED ON THE THREE POINT GAUSSIAN QUADRATURE RULE
!
!     CALLING SEQUENCES :
!           CALL adaptGQ_dFunc_m('FUNC', args, A, B, TOL, Y)
!
!     INPUTS :
!           FUNC : STRING CONTAING NAME OF DEFINING INTEGRAND
!           args : EXTRA ARGUMENTS OF FUNCTION
!           A    : LOWER LIMIT OF INTEGRATION
!           B    : UPPER LIMIT OF INTEGRATION
!           TOL  : ABSOLUTE ERROR CONVERGENCE TOLERANCE
!
!     OUTPUT
!           Y    : APPROXIMATE VALUE OF THE DEFINITE INTEGRAL OF FUNC
!                  OVER THE INTERVAL [A B]
!
!---------------------------------------------------------------------72
    Implicit none
    double precision,intent(in)  :: a
    double precision,intent(in)  :: lb, ub
    double precision,intent(out) :: y
    double precision, optional   :: tol
    Double precision :: H2,EEST
    Integer          :: NFunc
    double precision :: Fm, Fc, Fp, Sab, approx
    double precision,external :: func
!---------------------------------------------------------------------72
      H2  = 0.5d0 * ( ub - lb )
      Y   = 0.d0
      Fm  = func( a, lb + H2 - auxGQ3W * H2 )
      Fc  = func( a, lb + H2 )
      Fp  = func( a, lb + H2 + auxGQ3W * H2 )
      SAB = H2 * ( 5.d0 * FM + 8.d0 * FC + 5.d0 * FP ) / 9.d0
      CALL subGQ3_dFunc_a(SAB, FUNC, a, lb, ub, TOL, APPROX, EEST, NFUNC)
      Y = APPROX
      End subroutine
!---------------------------------------------------------------------72

!---------------------------------------------------------------------72
      Recursive subroutine subGQ3_dFunc_a(S, FUN, a, lb, ub, TOL, AP, ES, N)
!---------------------------------------------------------------------72
      Implicit none
      double precision,intent(in) :: a
      double precision,intent(in) :: lb, ub, tol
      Integer,intent(out) :: N
      double precision,intent(out) :: ES
      double precision,intent(out) :: AP
      integer          :: N1,N2
      double precision :: H2, Errest, E1, E2
      double precision :: FML,FCL,FPL,FMR,FCR,FPR,SAC,SCB,A1,A2,S
      double precision,external :: fun
!---------------------------------------------------------------------72
      H2 = 0.25d0 * ( ub - lb )

      FML = FUN( a, lb + H2 - auxGQ3W * H2 )
      FCL = FUN( a, lb + H2 )
      FPL = FUN( a, lb + H2 + auxGQ3W * H2 )
      FMR = FUN( a, ub - H2 - auxGQ3W * H2 )
      FCR = FUN( a, ub - H2 )
      FPR = FUN( a, ub - H2 + auxGQ3W * H2 )

      SAC = H2 * ( 5.D0 * FML + 8.D0 * FCL + 5.D0 * FPL ) / 9.D0
      SCB = H2 * ( 5.D0 * FMR + 8.D0 * FCR + 5.D0 * FPR ) / 9.D0

      ERREST = S - SAC - SCB
      IF (ERREST.LT.42.d0 * TOL) THEN
        AP = SAC + SCB
        ES = ERREST
        ES = ES / 42.d0
        N  = 6
      ELSE
        CALL subGQ3_dFunc_a(SAC, FUN, a, lb, lb + 2.D0 * H2, TOL / 2.D0, A1, E1, N1)
        CALL subGQ3_dFunc_a(SCB, FUN, a, lb + 2.D0 * H2, ub, TOL / 2.D0, A2, E2, N2)
        AP = A1 + A2
        ES = E1 + E2
        N  = N1 + N2 + 6
      END IF
      END SUBROUTINE
!---------------------------------------------------------------------72

!---------------------------------------------------------------------72
      Subroutine adaptGQ_dFunc_ab(func, a, b, lb, ub, y, tol)
!---------------------------------------------------------------------72
!
!     SUBROUTINE ADAPT_GQ3
!     APPROXIMATE THE DEFINITE INTEGRAL OF AN ARBITRARY FUNCTION TO
!     WHITHIN A SPECIFIED ERROR TOLERANCE USING ADAPTIVE QUADRATURE
!     BASED ON THE THREE POINT GAUSSIAN QUADRATURE RULE
!
!     CALLING SEQUENCES :
!           CALL adaptGQ_dFunc_m('FUNC', args, A, B, TOL, Y)
!
!     INPUTS :
!           FUNC : STRING CONTAING NAME OF DEFINING INTEGRAND
!           args : EXTRA ARGUMENTS OF FUNCTION
!           A    : LOWER LIMIT OF INTEGRATION
!           B    : UPPER LIMIT OF INTEGRATION
!           TOL  : ABSOLUTE ERROR CONVERGENCE TOLERANCE
!
!     OUTPUT
!           Y    : APPROXIMATE VALUE OF THE DEFINITE INTEGRAL OF FUNC
!                  OVER THE INTERVAL [A B]
!
!---------------------------------------------------------------------72
    Implicit none
    double precision,intent(in)  :: a, b
    double precision,intent(in)  :: lb, ub
    double precision,intent(out) :: y
    double precision, optional   :: tol
    Double precision :: H2,EEST
    Integer          :: NFunc
    double precision :: Fm, Fc, Fp, Sab, approx
    double precision,external :: func
!---------------------------------------------------------------------72
      H2  = 0.5d0 * ( ub - lb )
      Y   = 0.d0
      Fm  = func( a, b, lb + H2 - auxGQ3W * H2 )
      Fc  = func( a, b, lb + H2 )
      Fp  = func( a, b, lb + H2 + auxGQ3W * H2 )
      SAB = H2 * ( 5.d0 * FM + 8.d0 * FC + 5.d0 * FP ) / 9.d0
      CALL subGQ3_dFunc_ab(SAB, FUNC, a, b, lb, ub, TOL, APPROX, EEST, NFUNC)
      Y = APPROX
      End subroutine
!---------------------------------------------------------------------72

!---------------------------------------------------------------------72
      Recursive subroutine subGQ3_dFunc_ab(S, FUN, a, b, lb, ub, TOL, AP, ES, N)
!---------------------------------------------------------------------72
      Implicit none
      double precision,intent(in) :: a, b
      double precision,intent(in) :: lb, ub, tol
      Integer,intent(out) :: N
      double precision,intent(out) :: ES
      double precision,intent(out) :: AP
      integer          :: N1,N2
      double precision :: H2, Errest, E1, E2
      double precision :: FML,FCL,FPL,FMR,FCR,FPR,SAC,SCB,A1,A2,S
      double precision,external :: fun
!---------------------------------------------------------------------72
      H2 = 0.25d0 * ( ub - lb )

      FML = FUN( a, b, lb + H2 - auxGQ3W * H2 )
      FCL = FUN( a, b, lb + H2 )
      FPL = FUN( a, b, lb + H2 + auxGQ3W * H2 )
      FMR = FUN( a, b, ub - H2 - auxGQ3W * H2 )
      FCR = FUN( a, b, ub - H2 )
      FPR = FUN( a, b, ub - H2 + auxGQ3W * H2 )

      SAC = H2 * ( 5.D0 * FML + 8.D0 * FCL + 5.D0 * FPL ) / 9.D0
      SCB = H2 * ( 5.D0 * FMR + 8.D0 * FCR + 5.D0 * FPR ) / 9.D0

      ERREST = S - SAC - SCB
      IF (ERREST.LT.42.d0 * TOL) THEN
        AP = SAC + SCB
        ES = ERREST
        ES = ES / 42.d0
        N  = 6
      ELSE
        CALL subGQ3_dFunc_ab(SAC, FUN, a, b, lb, lb + 2.D0 * H2, TOL / 2.D0, A1, E1, N1)
        CALL subGQ3_dFunc_ab(SCB, FUN, a, b, lb + 2.D0 * H2, ub, TOL / 2.D0, A2, E2, N2)
        AP = A1 + A2
        ES = E1 + E2
        N  = N1 + N2 + 6
      END IF
      END SUBROUTINE
!---------------------------------------------------------------------72

!---------------------------------------------------------------------72
      Subroutine adaptGQ_cFunc_a(func, a, lb, ub, y, tol)
!---------------------------------------------------------------------72
!
!     SUBROUTINE ADAPT_GQ3
!     APPROXIMATE THE DEFINITE INTEGRAL OF AN ARBITRARY FUNCTION TO
!     WHITHIN A SPECIFIED ERROR TOLERANCE USING ADAPTIVE QUADRATURE
!     BASED ON THE THREE POINT GAUSSIAN QUADRATURE RULE
!
!     CALLING SEQUENCES :
!           CALL adaptGQ_cFunc_m('FUNC', args, A, B, TOL, Y)
!
!     INPUTS :
!           FUNC : STRING CONTAING NAME DEFINING INTEGRAND
!           args : EXTRA ARGUMENTS OF FUNCTION
!           A    : LOWER LIMIT OF INTEGRATION
!           B    : UPPER LIMIT OF INTEGRATION
!           TOL  : ABSOLUTE ERROR CONVERGENCE TOLERANCE
!
!     OUTPUT
!           Y    : APPROXIMATE VALUE OF THE DEFINITE INTEGRAL OF FUNC
!                  OVER THE INTERVAL [A B]
!
!---------------------------------------------------------------------72
    Implicit none
    double precision, intent(in) :: a
    double precision, intent(in) :: lb, ub
    double complex,intent(out)   :: y
    double precision, optional   :: tol
    Double precision :: H2,EEST
    Integer          :: NFunc
    Double complex   :: Fm,Fc,Fp,Sab,approx
    Double complex,External :: func
!---------------------------------------------------------------------72
      H2  = 0.5d0 * ( ub - lb )
      Y   = ( 0.d0, 0.d0 )
      Fm  = func(a, lb + H2 - auxGQ3W * H2)
      Fc  = func(a, lb + H2)
      Fp  = func(a, lb + H2 + auxGQ3W * H2)
      SAB = H2 * ( 5.d0 * FM + 8.d0 * FC + 5.d0 * FP ) / 9.d0
      CALL subGQ3_cFunc_a(SAB, FUNC, a, lb, ub, TOL, APPROX, EEST, NFUNC)
      Y = APPROX
      End subroutine
!---------------------------------------------------------------------72
      Recursive subroutine subGQ3_cFunc_a(S, FUN, a, lb, ub, TOL, AP, ES, N3)
!---------------------------------------------------------------------72
      Implicit none
      double precision,intent(in)  :: a
      double precision,intent(in)  :: lb, ub, tol
      Integer,intent(out)          :: N3
      double precision,intent(out) :: ES
      double complex,intent(out)   :: AP
      integer          :: N1, N2
      double precision :: H2, Errest1, Errest2, ES1, ES2, E1, E2
      double complex          :: FML, FCL, FPL, FMR, FCR, FPR, SAC, SCB, A1, A2, S
      double complex,external :: fun
!---------------------------------------------------------------------72
      H2 = 0.25d0 * ( ub - lb )

      FML = FUN( a, lb + H2 - auxGQ3W * H2 )
      FCL = FUN( a, lb + H2 )
      FPL = FUN( a, lb + H2 + auxGQ3W * H2 )

      FMR = FUN( a, ub - H2 - auxGQ3W * H2 )
      FCR = FUN( a, ub - H2 )
      FPR = FUN( a, ub - H2 + auxGQ3W * H2 )

      SAC = H2 * ( 5.D0 * FML + 8.D0 * FCL + 5.D0 * FPL ) / 9.D0
      SCB = H2 * ( 5.D0 * FMR + 8.D0 * FCR + 5.D0 * FPR ) / 9.D0

      ERREST1 = DABS( DREAL(S) - DREAL(SAC) - DREAL(SCB) )
      ERREST2 = DABS( DIMAG(S) - DIMAG(SAC) - DIMAG(SCB) )

      IF (ERREST1.LT.42.d0*TOL.AND.ERREST2.LT.42.d0*TOL) THEN
        AP  = SAC + SCB
        ES1 = ERREST1
        ES2 = ERREST2
        ES  = MAX(ES1, ES2) / 42.d0
        N3  = 6
      ELSE
        CALL subGQ3_cFunc_a(SAC, FUN, a, lb, lb + 2.D0 * H2, TOL / 2.D0, A1, E1, N1)
        CALL subGQ3_cFunc_a(SCB, FUN, a, lb + 2.D0 * H2, ub, TOL / 2.D0, A2, E2, N2)
        AP = A1 + A2
        ES = E1 + E2
        N3 = N1 + N2 + 6
      END IF
      END SUBROUTINE
!---------------------------------------------------------------------72

!---------------------------------------------------------------------72
      Subroutine adaptGQ_dFunc_mn(func, m, n, lb, ub, y, tol)
!---------------------------------------------------------------------72
!
!     SUBROUTINE ADAPT_GQ3
!     APPROXIMATE THE DEFINITE INTEGRAL OF AN ARBITRARY FUNCTION TO
!     WHITHIN A SPECIFIED ERROR TOLERANCE USING ADAPTIVE QUADRATURE
!     BASED ON THE THREE POINT GAUSSIAN QUADRATURE RULE
!
!     CALLING SEQUENCES :
!           CALL adaptGQ_dFunc_m('FUNC', args, A, B, TOL, Y)
!
!     INPUTS :
!           FUNC : STRING CONTAING NAME OF DEFINING INTEGRAND
!           args : EXTRA ARGUMENTS OF FUNCTION
!           A    : LOWER LIMIT OF INTEGRATION
!           B    : UPPER LIMIT OF INTEGRATION
!           TOL  : ABSOLUTE ERROR CONVERGENCE TOLERANCE
!
!     OUTPUT
!           Y    : APPROXIMATE VALUE OF THE DEFINITE INTEGRAL OF FUNC
!                  OVER THE INTERVAL [A B]
!
!---------------------------------------------------------------------72
    Implicit none
    integer,intent(in)  :: m, n
    double precision,intent(in)  :: lb, ub
    double precision,intent(out) :: y
    double precision, optional   :: tol
    Double precision :: H2,EEST
    Integer          :: NFunc
    double precision :: Fm, Fc, Fp, Sab, approx
    double precision,external :: func
!---------------------------------------------------------------------72
      H2  = 0.5d0 * ( ub - lb )
      Y   = 0.d0
      Fm  = func( m, n, lb + H2 - auxGQ3W * H2 )
      Fc  = func( m, n, lb + H2 )
      Fp  = func( m, n, lb + H2 + auxGQ3W * H2 )
      SAB = H2 * ( 5.d0 * FM + 8.d0 * FC + 5.d0 * FP ) / 9.d0
      CALL subGQ3_dFunc_mn(SAB, FUNC, m, n, lb, ub, TOL, APPROX, EEST, NFUNC)
      Y = APPROX
      End subroutine
!---------------------------------------------------------------------72

!---------------------------------------------------------------------72
      Recursive subroutine subGQ3_dFunc_mn(S, FUN, m, n, lb, ub, TOL, AP, ES, Ncount)
!---------------------------------------------------------------------72
      Implicit none
      integer,intent(in)  :: m, n
      double precision,intent(in) :: lb, ub, tol
      Integer,intent(out) :: Ncount
      double precision,intent(out) :: ES
      double precision,intent(out) :: AP
      integer          :: N1,N2
      double precision :: H2, Errest, E1, E2
      double precision :: FML,FCL,FPL,FMR,FCR,FPR,SAC,SCB,A1,A2,S
      double precision,external :: fun
!---------------------------------------------------------------------72
      H2 = 0.25d0 * ( ub - lb )

      FML = FUN( m, n, lb + H2 - auxGQ3W * H2 )
      FCL = FUN( m, n, lb + H2 )
      FPL = FUN( m, n, lb + H2 + auxGQ3W * H2 )
      FMR = FUN( m, n, ub - H2 - auxGQ3W * H2 )
      FCR = FUN( m, n, ub - H2 )
      FPR = FUN( m, n, ub - H2 + auxGQ3W * H2 )

      SAC = H2 * ( 5.D0 * FML + 8.D0 * FCL + 5.D0 * FPL ) / 9.D0
      SCB = H2 * ( 5.D0 * FMR + 8.D0 * FCR + 5.D0 * FPR ) / 9.D0

      ERREST = S - SAC - SCB
      IF (ERREST.LT.42.d0 * TOL) THEN
        AP = SAC + SCB
        ES = ERREST
        ES = ES / 42.d0
        Ncount  = 6
      ELSE
        CALL subGQ3_dFunc_mn(SAC, FUN, m, n, lb, lb + 2.D0 * H2, TOL / 2.D0, A1, E1, N1)
        CALL subGQ3_dFunc_mn(SCB, FUN, m, n, lb + 2.D0 * H2, ub, TOL / 2.D0, A2, E2, N2)
        AP = A1 + A2
        ES = E1 + E2
        Ncount  = N1 + N2 + 6
      END IF
      END SUBROUTINE
!---------------------------------------------------------------------72
