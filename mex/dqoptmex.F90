!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! File dqoptmex.F90
! Mex function for DQOPT7.
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include "fintrf.h"

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine mexFunction( nlhs, plhs, nrhs, prhs )
  use mxdnWork

  implicit none

  integer*4  :: nlhs, nrhs
  mwPointer  :: prhs(*), plhs(*)
  !=====================================================================
  ! Mex function for DQOPT7
  !
  ! Option      Action
  !    1        Solve the QP (quadprog-style)
  !    2        Set option
  !    3        Set integer option
  !    4        Set real option
  !    5        Get option
  !    6        Get character option
  !    7        Get integer option
  !    8        Get real option
  !    9        Read specs file
  !    10       Open print file
  !    11       Open summary file
  !    12       Close print file
  !    13       Close summary file
  !    14
  !    15       Screen on
  !    16       Screen off
  !
  ! 26 Aug 2013: First version.
  !=====================================================================
  ! Matlab
  mwPointer        :: mxGetN, mxGetPr
  integer          :: mxIsChar
  double precision :: mxGetScalar

  ! DNOPT
  character        :: filename*80
  integer          :: info, iOpt, strlen
  double precision :: rOpt
  external         :: dqBegin


  ! Get option.
  if ( nrhs < 1 ) call mexErrMsgTxt( 'Need an option input argument' )
  rOpt = mxGetScalar(prhs(1))
  iOpt = rOpt


  ! Files
  if ( iOpt == dnOpenP ) then

     if ( nrhs /= 2 ) call mexErrMsgTxt( 'Wrong number of input arguments' )

     info = mxIsChar(prhs(2))
     if ( info /= 1 ) call mexErrMsgTxt( 'Need filename string' )

     strlen = mxGetN(prhs(2))
     if ( strlen > 80 ) call mexErrMsgTxt( 'Print filename is too long' )

     if ( strlen > 0 ) then
        call mxGetString( prhs(2), filename, strlen )
     else
        call mexErrMsgTxt( 'Empty print filename' )
     end if

     if ( printOpen ) close( iPrint )

     open( iPrint, file=filename, status='unknown' )
     printOpen = .true.

  else if ( iOpt == dnOpenS ) then

     if ( nrhs /= 2 ) call mexErrMsgTxt( 'Wrong number of input arguments' )

     info = mxIsChar(prhs(2))
     if ( info /= 1 ) call mexErrMsgTxt( 'Need filename string' )

     strlen = mxGetN(prhs(2))
     if ( strlen > 80 ) call mexErrMsgTxt( 'Summary filename is too long' )

     if ( strlen > 0 ) then
        call mxGetString( prhs(2), filename, strlen )
     else
        call mexErrMsgTxt( 'Empty summary filename' )
     end if

     if ( summOpen ) close( iSumm )

     open( iSumm, file=filename, status='unknown' )
     summOpen = .true.

  else if ( iOpt == dnClosP ) then
     if ( printOpen ) close( iPrint )
     printOpen = .false.

  else if ( iOpt == dnClosS ) then
     if ( summOpen ) close( iSumm )
     summOpen = .false.

  else if ( iOpt == dnscrnOn ) then
     screenOn = .true.

  else if ( iOpt == dnscrnOff ) then
     screenOn = .false.

  else if ( iOpt == dnEnd ) then
     if ( printOpen ) close( iPrint )
     printOpen = .false.

     if ( summOpen ) close ( iSumm )
     summOpen  = .false.
     firstCall = .true.

     if ( allocated(cw) ) deallocate( cw )
     if ( allocated(iw) ) deallocate( iw )
     if ( allocated(rw) ) deallocate( rw )

  else

     if ( firstCall ) then
        if ( allocated(cw) ) deallocate( cw )
        if ( allocated(iw) ) deallocate( iw )
        if ( allocated(rw) ) deallocate( rw )
        allocate( cw(lencw), iw(leniw), rw(lenrw) )

        call dqBegin( iPrint, iSumm, cw, lencw, iw, leniw, rw, lenrw )

        firstCall = .false.
     end if

     ! Do whatever we need to do.
     if ( iOpt == dnSolve ) then

        callType = userCall
        call dqmxSolve( nlhs, plhs, nrhs, prhs )
        callType = systemCall

     else if ( iOpt == dnSetX .or. &
               iOpt == dnSetI .or. &
               iOpt == dnSetR .or. &
               iOpt == dnGetX .or. &
               iOpt == dnGetC .or. &
               iOpt == dnGetI .or. &
               iOpt == dnGetR ) then

        callType = userCall
        call dqmxOptions( iOpt, nlhs, plhs, nrhs, prhs )
        callType = systemCall

     else if ( iOpt == dnSpecs ) then

        callType = userCall
        call dqmxSpecs( nlhs, plhs, nrhs, prhs )
        callType = systemCall

     else if ( iOpt == dnSolveN ) then

        callType = userCall
        call dqmxSolveN( nlhs, plhs, nrhs, prhs )
        callType = systemCall

     end if
  end if

  return

end subroutine mexFunction

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine dqmxSolve( nlhs, plhs, nrhs, prhs )
  use mxdnWork

  implicit none
  integer*4  :: nlhs, nrhs
  mwPointer  :: prhs(*), plhs(*)
  !---------------------------------------------------------------------
  ! Solve the quadratic problem
  ! The matlab call (based on quadprog) is
  !   [x, fval, exit, itn, y ] =
  !       dqsolve ( H, f, A, b, Aeq, beq, lb, ub, x0 )
  !
  ! where
  !   H, f      are the Hessian and linear terms of the objective
  !   x0        is the initial point
  !   A, b      are the linear inequality constraints A*x <= b
  !   Aeq, beq  are the linear equality constraints Aeq*x = beq
  !   lb, ub    are the lower and upper bounds on x
  !---------------------------------------------------------------------
  ! Matlab
  mwPointer :: mxGetM, mxGetN, mxGetPr, mxDuplicateArray, &
               mxCreateDoubleMatrix, mxCreateDoubleScalar
  integer   :: mxIsClass
  mwSize    :: tmp, mm, nn

  ! DNOPT
  character*8      :: probName
  integer          :: Errors, i1, i2, info, j
  integer          :: Start, iObj, ldA, ldH, m, &
                      mincw, miniw, minrw, nb, ncObj, nInf, &
                      nleq, nlin, nNames, nnH
  double precision :: rtmp, Obj, ObjAdd, sInf
  external         :: dqKernel, dqLog, matlabHx

  integer,          parameter   :: nItn   = 421
  double precision, parameter   :: infBnd = 1.0d+20

  character*8,      allocatable :: Names(:)
  integer,          allocatable :: state(:), Etype(:)
  double precision, allocatable :: x(:), bl(:), bu(:), y(:), &
                                   H(:,:), A(:,:), cObj(:)


  ! Check number of input and output arguments.
  if ( nrhs /= 10 ) call mexErrMsgTxt( 'Wrong number of input arguments' )


  !-----------------------------------------------------------------------------
  ! Hessian matrix
  !-----------------------------------------------------------------------------
  nnH = mxGetM(prhs(2))
  ldH = nnH

  if ( nnH > 0 ) then
     allocate( H(ldH,ldH) )
     call mxCopyPtrToReal8( mxGetPr(prhs(2)), H(1:nnH,1:nnH), nnH*nnH )
  else
     ldH = 1
     allocate( H(ldH,ldH) )
  end if


  !-----------------------------------------------------------------------------
  ! Linear term of objective
  !-----------------------------------------------------------------------------
  ncObj = mxGetM(prhs(3))

  if ( ncObj > 0 ) then
     allocate( cObj(ncObj) )

     call checkRow( prhs(3), ncObj, 'cObj' )
     call checkCol( prhs(3),     1, 'cObj' )

     call mxCopyPtrToReal8( mxGetPr(prhs(3)), cObj(1:ncObj), ncObj )
  else
     ncObj = 0
     allocate( cObj(1) )
  end if

  ! Set number of variables
  n = max(nnH,ncObj)
  if ( n == 0 ) call mexErrMsgTxt( 'No objective given' )

  !-----------------------------------------------------------------------------
  ! Compute number of linear constraints
  ! Check dimension of some of the input
  !-----------------------------------------------------------------------------
  ! Get number of linear inequality constraint Ax <= b
  nlin = mxGetM(prhs(4))
  if ( nlin > 0 ) call checkCol( prhs(4), n, 'A' )


  ! Get number of linear equality constraints Aeq x = beq
  nleq = mxGetM(prhs(6))
  if ( nleq > 0 ) call checkCol( prhs(6), n, 'Aeq' )

  ! Set the total number of linear constraints
  m = nlin + nleq


  !-----------------------------------------------------------------------------
  ! Copy the linear constraint matrix
  !-----------------------------------------------------------------------------
  if ( m > 0 ) then
     ldA = m
     allocate( A(ldA,n) )

     if ( nlin > 0 ) &
          call mxCopyPtrToReal8( mxGetPr(prhs(4)), A(1:nlin,1:n), nlin*n )

     if ( nleq > 0  ) &
          call mxCopyPtrToReal8( mxGetPr(prhs(6)), A(nlin+1:m,1:n), nleq*n )
  else
     ldA = 1
     allocate( A(ldA,n) )
  end if



  !-----------------------------------------------------------------------------
  ! Allocate space for DNOPT
  !-----------------------------------------------------------------------------
  nNames = 1

  allocate( bl(n+m), bu(n+m), x(n+m), y(n+m) )
  allocate( state(n+m), Names(nNames), Etype(n+m) )


  ! Set initial x
  x = 0.0
  y = 0.0

  tmp = mxGetM(prhs(10))
  if ( tmp > 0 ) then
     call checkRow( prhs(10), n, 'x0' )
     call checkCol( prhs(10), 1, 'x0' )
     call mxCopyPtrToReal8( mxGetPr(prhs(10)), x(1:n), n )
  end if


  !-----------------------------------------------------------------------------
  ! Set bounds on variables and constraints
  !-----------------------------------------------------------------------------
  ! Lower and upper bounds on x
  ! Check dimensions of lower and upper bounds on x
  tmp = mxGetM(prhs(8))

  if ( tmp > 0 ) then
     call checkRow( prhs(8), n, 'lb' )
     call checkCol( prhs(8), 1, 'lb' )
     call mxCopyPtrToReal8( mxGetPr(prhs(8)), bl(1:n), n )
  else
     bl(1:n) = -infBnd
  end if

  tmp = mxGetM(prhs(9))
  if ( tmp > 0 ) then
     call checkRow( prhs(9), n, 'ub' )
     call checkCol( prhs(9), 1, 'ub' )
     call mxCopyPtrToReal8( mxGetPr(prhs(9)), bu(1:n), n )
  else
     bu(1:n) = infBnd
  end if


  ! Linear inequality constraint bounds
  if ( nlin > 0 ) then
     i1 = 1+n
     i2 = i1-1+nlin

     ! Check dimension of b
     tmp = mxGetM(prhs(5))
     if ( tmp > 0 ) then
        call checkRow( prhs(5), nlin, 'b' )
        call checkCol( prhs(5), 1,    'b' )
        call mxCopyPtrToReal8( mxGetPr(prhs(5)), bu(i1:i2), nlin )
     else
        bu(i1:i2) = infBnd
     end if

     bl(i1:i2) = -infBnd
  end if


  ! Linear equality constraint bounds
  if ( nleq > 0 ) then
     i1 = 1+n+nlin
     i2 = i1-1+nleq

     tmp = mxGetM(prhs(7))
     if ( tmp > 0 ) then
        call checkRow( prhs(7), nleq, 'beq' )
        call checkCol( prhs(7),    1, 'beq' )
        call mxCopyPtrToReal8( mxGetPr(prhs(7)), bu(i1:i2), nleq )
     else
        call mexErrMsgTxt( 'Missing linear equality constraint rhs beq' )
     end if

     bl(i1:i2) = bu(i1:i2)
  end if



  !-----------------------------------------------------------------------------
  ! Solve the problem
  !-----------------------------------------------------------------------------
  Start    = 0   ! cold start
  iObj     = 0
  ObjAdd   = 0.0

  Etype    = 0
  state    = 0
  probName = 'matlabMx'

  nb = n + m

100 call dqKernel &
         ( start, m, n, nb, nnH, nNames, ldH, &
           ncObj, iObj, objAdd, probName, &
           A, ldA, bl, bu, cObj, H, Names, &
           matlabHx, dqLog, &
           Etype, state, x, y, &
           INFO, mincw, miniw, minrw, &
           Obj, nInf, sInf, &
           cw, lencw, iw, leniw, rw, lenrw, &
           cw, lencw, iw, leniw, rw, lenrw )

  if ( INFO == 83 ) then
     ! Not enough integer space
     leniw = miniw
     allocate( iw0(leniw) )
     iw0(1:500) = iw(1:500)

     call move_alloc( from=iw0, to=iw )
     call dqSetInt &
          ( 'Total integer   workspace', leniw, 0, 0, Errors, &
             cw, lencw, iw, leniw, rw, lenrw )
     go to 100
  end if

  if ( INFO == 84 ) then
    ! Not enough real space
     lenrw = minrw
     allocate( rw0(lenrw) )
     rw0(1:500) = rw(1:500)

     call move_alloc( from=rw0, to=rw )
     call dqSetInt &
          ( 'Total real      workspace', lenrw, 0, 0, Errors, &
            cw, lencw, iw, leniw, rw, lenrw )
     go to 100
  end if


  !-----------------------------------------------------------------------------
  ! Set output
  !-----------------------------------------------------------------------------
  if ( nlhs > 0 ) then
     plhs(1) = mxCreateDoubleMatrix ( n, 1, 0 )
     call mxCopyReal8ToPtr( x(1:n), mxGetPr(plhs(1)), n )
  end if


  ! Final objective
  if ( nlhs > 1 ) plhs(2) = mxCreateDoubleScalar( Obj )


  ! Exit flag
  rtmp = info
  if ( nlhs > 2 ) plhs(3) = mxCreateDoubleScalar( rtmp )


  ! Iterations
  rtmp = iw(nItn)
  if ( nlhs > 3 ) plhs(4) = mxCreateDoubleScalar( rtmp )


  ! Multipliers for bounds
  if ( nlhs > 4 ) then
     plhs(5) = mxCreateDoubleMatrix( n, 1, 0 )
     call mxCopyReal8ToPtr( y(1:n), mxGetPr(plhs(5)), n )
  end if


  ! Multipliers for linear inequalities
  if ( nlhs > 5 ) then
     if ( nlin > 0 ) then
        plhs(6) = mxCreateDoubleMatrix( nlin, 1, 0 )
        i1 = 1+n
        i2 = i1-1+nlin
        call mxCopyReal8ToPtr( y(i1:i2), mxGetPr(plhs(6)), nlin )
     else
        plhs(6) = mxCreateDoubleScalar(0.0)
     end if
  end if


  ! Multipliers for linear equalities
  if ( nlhs > 6 ) then
     if ( nleq > 0 ) then
        plhs(7) = mxCreateDoubleMatrix( nleq, 1, 0 )
        i1 = 1+n+nlin
        i2 = i1-1+nleq
        call mxCopyReal8ToPtr( y(i1:i2), mxGetPr(plhs(7)), nleq )
     else
        plhs(7) = mxCreateDoubleScalar(0.0)
     end if
  end if


  ! Deallocate memory
  if ( allocated(Names) ) deallocate( Names )
  if ( allocated(state) ) deallocate( state )
  if ( allocated(Etype) ) deallocate( Etype )

  if ( allocated(x) )     deallocate( x )
  if ( allocated(bl) )    deallocate( bl )
  if ( allocated(bu) )    deallocate( bu )
  if ( allocated(y) )     deallocate( y )

  if ( allocated(H) )     deallocate( H )
  if ( allocated(A) )     deallocate( A )
  if ( allocated(cObj) )  deallocate( cObj )

end subroutine dqmxSolve

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine dqmxSolveN( nlhs, plhs, nrhs, prhs )
  use mxdnWork
  implicit none
  integer*4  :: nlhs, nrhs
  mwPointer  :: prhs(*), plhs(*)
  !---------------------------------------------------------------------
  ! Solve the quadratic problem
  ! The matlab call is
  !   [x, fval, exit, itn, y ] =
  !       dqsolve ( name, H, f, x0, xl, xu, A, al, au )
  !
  ! where
  !   name      is the problem name
  !   H, f      are the Hessian and linear terms of the objective
  !   x0        is the initial point
  !   lb, ub    are the lower and upper bounds on x
  !   A, al, au are the linear inequality constraints al <= A*x <= au
  !---------------------------------------------------------------------
  ! Matlab
  mwPointer :: mxGetM, mxGetN, mxGetPr, &
               mxCreateDoubleMatrix, mxCreateDoubleScalar
  integer   :: mxIsChar
  mwSize    :: tmp, mm, nn

  ! DNOPT
  character*8      :: probName
  integer          :: i1, i2, info, j, strlen
  integer          :: Start, iObj, ldA, ldH, m, &
                      mincw, miniw, minrw, nb, ncObj, nInf, nNames, nnH
  double precision :: rtmp, Obj, ObjAdd, sInf
  external         :: dqKernel, dqLog, matlabHx

  integer,          parameter   :: nItn   = 421
  double precision, parameter   :: infBnd = 1.0d+20

  character*8,      allocatable :: Names(:)
  integer,          allocatable :: state(:), Etype(:)
  double precision, allocatable :: x(:), bl(:), bu(:), y(:), &
                                   H(:,:), A(:,:), cObj(:)


  ! Check number of input and output arguments.
  if ( nrhs /= 10 ) call mexErrMsgTxt( 'Wrong number of input arguments' )


  !-----------------------------------------------------------------------------
  ! Problem name
  !-----------------------------------------------------------------------------
  info = mxIsChar( prhs(2) )
  if ( info /= 1 ) call mexErrMsgTxt( 'Wrong input type for problem name' )

  strlen = mxGetN(prhs(2))
  if ( strlen > 8 ) strlen = 8

  probName = ''
  call mxGetString( prhs(2), probName, strlen )


  !-----------------------------------------------------------------------------
  ! Hessian matrix
  !-----------------------------------------------------------------------------
  nnH = mxGetM(prhs(3))
  ldH = nnH

  if ( nnH > 0 ) then
     allocate( H(ldH,ldH) )
     call mxCopyPtrToReal8( mxGetPr(prhs(3)), H(1:nnH,1:nnH), nnH*nnH )
  else
     ldH = 1
     allocate( H(ldH,ldH) )
  end if


  !-----------------------------------------------------------------------------
  ! Linear term of objective
  !-----------------------------------------------------------------------------
  ncObj = mxGetM(prhs(4))

  if ( ncObj > 0 ) then
     allocate( cObj(ncObj) )

     call checkRow( prhs(4), ncObj, 'cObj' )
     call checkCol( prhs(4),     1, 'cObj' )

     call mxCopyPtrToReal8( mxGetPr(prhs(4)), cObj(1:ncObj), ncObj )
  else
     ncObj = 0
     allocate( cObj(1) )
  end if

  ! Set number of variables
  n = max(nnH,ncObj)
  if ( nnH == 0 .and. ncObj == 0 ) call mexErrMsgTxt( 'No objective given' )


  !-----------------------------------------------------------------------------
  ! Compute number of variables and linear constraints
  !-----------------------------------------------------------------------------
  ! Get number of linear inequality constraint al <= Ax <= au
  m = mxGetM(prhs(8))
  if ( m > 0 ) call checkCol( prhs(8), n, 'A' )

  allocate( bl(n+m), bu(n+m), x(n+m), y(n+m) )
  allocate( state(n+m), Etype(n+m) )

  nNames = 1
  allocate( Names(nNames) )

  x = 0.0
  y = 0.0

  ! Get initial x0
  tmp = mxGetM(prhs(5))
  if ( tmp > 0 ) then
     call checkRow( prhs(5), n, 'x0' )
     call checkCol( prhs(5), 1, 'x0' )
     call mxCopyPtrToReal8( mxGetPr(prhs(5)), x(1:n), n )
  end if


  !-----------------------------------------------------------------------------
  ! Set bounds on variables and constraints
  !-----------------------------------------------------------------------------
  ! Lower and upper bounds on x
  ! Check dimensions of lower and upper bounds on x
  tmp = mxGetM(prhs(6))

  if ( tmp > 0 ) then
     call checkRow( prhs(6), n, 'lb' )
     call checkCol( prhs(6), 1, 'lb' )
     call mxCopyPtrToReal8( mxGetPr(prhs(6)), bl(1:n), n )
  else
     bl(1:n) = -infBnd
  end if

  tmp = mxGetM(prhs(7))
  if ( tmp > 0 ) then
     call checkRow( prhs(7), n, 'ub' )
     call checkCol( prhs(7), 1, 'ub' )
     call mxCopyPtrToReal8( mxGetPr(prhs(7)), bu(1:n), n )
  else
     bu(1:n) = infBnd
  end if


  !-----------------------------------------------------------------------------
  ! Copy the linear constraint matrix
  !-----------------------------------------------------------------------------
  if ( m > 0 ) then
     ldA = m
     allocate( A(ldA,n) )

     call mxCopyPtrToReal8( mxGetPr(prhs(8)), A(1:m,1:n), m*n )

  else
     ldA = 1
     allocate( A(ldA,n) )
  end if

  ! Linear constraint bounds
  if ( m > 0 ) then
     i1 = 1+n
     i2 = m+n

     ! Check dimension of lA and uA
     if ( mxGetM(prhs(9)) > 0 ) then
        call checkRow( prhs(9), m, 'al' )
        call checkCol( prhs(9), 1, 'al' )
        call mxCopyPtrToReal8( mxGetPr(prhs(9)), bl(i1:i2), m )
     else
        bl(i1:i2) = -infBnd
     end if

     if ( mxGetM(prhs(10)) > 0 ) then
        call checkRow( prhs(10), m, 'au' )
        call checkCol( prhs(10), 1, 'au' )
        call mxCopyPtrToReal8( mxGetPr(prhs(10)), bu(i1:i2), m )
     else
        bu(i1:i2) = infBnd
     end if
  end if


  !-----------------------------------------------------------------------------
  ! Solve the problem
  !-----------------------------------------------------------------------------
  Start    = 0   ! cold start
  iObj     = 0
  ObjAdd   = 0.0

  Etype    = 0
  state    = 0
  probName = 'matlabMx'

  nb = n + m

100 call dqKernel &
         ( start, m, n, nb, nnH, nNames, ldH, &
           ncObj, iObj, objAdd, probName, &
           A, ldA, bl, bu, cObj, H, Names, &
           matlabHx, dqLog, &
           Etype, state, x, y, &
           INFO, mincw, miniw, minrw, &
           Obj, nInf, sInf, &
           cw, lencw, iw, leniw, rw, lenrw, &
           cw, lencw, iw, leniw, rw, lenrw )

  if ( INFO == 83 ) then
     ! Not enough integer space
     leniw = miniw
     allocate( iw0(leniw) )
     iw0(1:500) = iw(1:500)

     call move_alloc( from=iw0, to=iw )
     call dqBegin( iPrint, iSumm, cw, lencw, iw, leniw, rw, lenrw )
     go to 100
  end if

  if ( INFO == 84 ) then
    ! Not enough real space
     lenrw = minrw
     allocate( rw0(lenrw) )
     rw0(1:500) = rw(1:500)

     call move_alloc( from=rw0, to=rw )
     call dqBegin( iPrint, iSumm, cw, lencw, iw, leniw, rw, lenrw )
     go to 100
  end if


  !-----------------------------------------------------------------------------
  ! Set output
  !-----------------------------------------------------------------------------
  if ( nlhs > 0 ) then
     plhs(1) = mxCreateDoubleMatrix( n, 1, 0 )
     call mxCopyReal8ToPtr( x(1:n), mxGetPr(plhs(1)), n )
  end if


  ! Final objective
  if ( nlhs > 1 ) plhs(2) = mxCreateDoubleScalar( Obj )


  ! Exit flag
  rtmp = info
  if ( nlhs > 2 ) plhs(3) = mxCreateDoubleScalar( rtmp )


  ! Iterations
  rtmp = iw(nItn)
  if ( nlhs > 3 ) plhs(4) = mxCreateDoubleScalar( rtmp )


  ! Multipliers for bounds
  if ( nlhs > 4 ) then
     plhs(5) = mxCreateDoubleMatrix( n, 1, 0 )
     call mxCopyReal8ToPtr( y(1:n), mxGetPr(plhs(5)), n )
  end if


  ! Multipliers for linear constraints
  if ( nlhs > 5 ) then
     if ( m > 0 ) then
        plhs(6) = mxCreateDoubleMatrix( m, 1, 0 )
        i1 = 1+n
        i2 = m+n
        call mxCopyReal8ToPtr( y(i1:i2), mxGetPr(plhs(6)), m )
     else
        plhs(6) = mxCreateDoubleScalar(0.0)
     end if
  end if


  ! Deallocate memory
  if ( allocated(Names) ) deallocate( Names )
  if ( allocated(state) ) deallocate( state )
  if ( allocated(Etype) ) deallocate( Etype )

  if ( allocated(x) )     deallocate( x )
  if ( allocated(bl) )    deallocate( bl )
  if ( allocated(bu) )    deallocate( bu )
  if ( allocated(y) )     deallocate( y )

  if ( allocated(H) )     deallocate( H )
  if ( allocated(A) )     deallocate( A )
  if ( allocated(cObj) )  deallocate( cObj )

end subroutine dqmxSolveN

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine dqmxOptions( iOpt, nlhs, plhs, nrhs, prhs )
  use mxdnWork
  implicit none
  integer    :: iOpt
  integer*4  :: nlhs, nrhs
  mwPointer  :: prhs(*), plhs(*)
  !---------------------------------------------------------------------
  ! Set/get options.
  !---------------------------------------------------------------------
  ! Matlab
  mwPointer        :: mxGetN, mxGetPr, mxCreateDoubleScalar, mxCreateString
  integer          :: mxIsChar
  double precision :: mxGetScalar

  character        :: buffer*50, cvalue*8
  integer          :: Errors, info, ivalue, strlen
  double precision :: rvalue

  integer          :: dqGet
  external         :: dqSet, dqSetInt, dqSetReal, &
                      dqGet, dqGetChar, dqGetInt, dqGetReal


  if ( iOpt == dnSetI .or. iOpt == dnSetR ) then
     if ( nrhs /= 3 ) call mexErrMsgTxt( 'Wrong number of input arguments' )
  else
     if ( nrhs /= 2 ) call mexErrMsgTxt( 'Wrong number of input arguments' )
  end if


  ! Get string
  info = mxIsChar(prhs(2))
  if ( info /= 1 ) call mexErrMsgTxt( 'Need filename string' )

  strlen = mxGetN(prhs(2))
  if ( strlen > 50 ) call mexErrMsgTxt( 'Option string is too long' )

  if ( strlen > 0 ) then
     call mxGetString( prhs(2), buffer, strlen )
  else
     call mexErrMsgTxt( 'Empty option string' )
  end if


  if      ( iOpt == dnSetX  ) then
     call dqSet( buffer, iPrint, iSumm, Errors, &
                 cw, lencw, iw, leniw, rw, lenrw )

  else if ( iOpt == dnSetI  ) then

     rvalue = mxGetScalar(prhs(3))
     ivalue = rvalue

     call dqSetInt( buffer, ivalue, iPrint, iSumm, Errors, &
                    cw, lencw, iw, leniw, rw, lenrw )

  else if ( iOpt == dnSetR  ) then

     rvalue = mxGetScalar(prhs(3))

     call dqSetReal( buffer, rvalue, iPrint, iSumm, Errors, &
                     cw, lencw, iw, leniw, rw, lenrw )

  else if ( iOpt == dnGetX  ) then

     ivalue  = dqGet( buffer, Errors, cw, lencw, iw, leniw, rw, lenrw )

     rvalue  = ivalue
     plhs(1) = mxCreateDoubleScalar( rvalue )

  else if ( iOpt == dnGetC  ) then

     call dqGetChar( buffer, cvalue, Errors, &
                     cw, lencw, iw, leniw, rw, lenrw )

     plhs(1) = mxCreateString(cvalue)

  else if ( iOpt == dnGetI  ) then

     call dqGetInt( buffer, ivalue, Errors, &
                    cw, lencw, iw, leniw, rw, lenrw )

     rvalue = ivalue
     plhs(1) = mxCreateDoubleScalar( rvalue )

  else if ( iOpt == dnGetR  ) then

     call dqGetReal( buffer, rvalue, Errors, &
                     cw, lencw, iw, leniw, rw, lenrw )

     plhs(1) = mxCreateDoubleScalar( rvalue )

  end if

end subroutine dqmxOptions

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine dqmxSpecs( nlhs, plhs, nrhs, prhs )
  use mxdnWork

  implicit none
  integer*4  :: nlhs, nrhs
  mwPointer  :: prhs(*), plhs(*)
  !---------------------------------------------------------------------
  ! Read specs file.
  !---------------------------------------------------------------------
  ! Matlab
  mwPointer        :: mxCreateDoubleScalar, mxGetN
  integer          :: mxIsChar

  character        :: filename*120
  integer          :: info, strlen
  double precision :: rvalue

  external         :: dqSpec


  if ( nrhs /= 2 ) call mexErrMsgTxt( 'Wrong number of input arguments' )
  if ( nlhs /= 1 ) call mexErrMsgTxt( 'Wrong number of output arguments' )


  info = mxIsChar(prhs(2))
  if ( info /= 1 ) call mexErrMsgTxt( 'Need filename string' )

  strlen = mxGetN(prhs(2))
  if ( strlen > 120 ) call mexErrMsgTxt( 'Specs filename is too long' )

  if ( strlen > 0 ) then
     call mxGetString( prhs(2), filename, strlen )
  else
     call mexErrMsgTxt( 'Empty spc filename' )
  end if


  open( iSpecs, file=filename, status='unknown' )
  call dqSpec( iSpecs, info, cw, lencw, iw, leniw, rw, lenrw )
  rewind( iSpecs )
  close( iSpecs )

  ! dqSpec will return info == 101 or 107 if successful
  ! The matlab version returns 0 if successful
  if ( info == 101 .or. info == 107 ) then
     rvalue = 0
  else
     rvalue = 1
  end if

  plhs(1) = mxCreateDoubleScalar( rvalue )

end subroutine dqmxSpecs

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine matlabHx( nnH, H, ldH, x, Hx, qpState, &
                     cu, lencu, iu, leniu, ru, lenru )
  use mxdnWork
  implicit none
  integer          :: ldH, nnH, qpState, lencu, leniu, lenru, iu(leniu)
  double precision :: H(ldH,*), x(nnH), Hx(nnH), ru(lenru)
  character*8      :: cu(lencu)

  !---------------------------------------------------------------------
  ! Callback to compute H*x for a given x.
  !---------------------------------------------------------------------
  integer          :: i, j
  double precision :: sum

  if ( qpState == 1 ) then
     ! First call.
  end if

  if ( nnH <= 0 ) return

  Hx = 0.0

  do i = 1, nnH
     sum = 0.0
     do j = 1, nnH
        sum = sum + H(i,j)*x(j)
     end do
     Hx(i) = sum
  end do

  if ( qpState >= 2 ) then
     ! Last call.
  end if

end subroutine matlabHx

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
