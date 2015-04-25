!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! File dnoptmex.F90
! Mex function for DNOPT7.
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
  ! Mex function for DNOPT7
  !
  ! Option      Action
  !    1        Solve the problem (fmincon-style)
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
  !    17       Solve the problem (DNOPT-style)
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
  external         :: dnBegin


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

     open ( iPrint, file=filename, status='unknown' )
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

     open ( iSumm, file=filename, status='unknown' )
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

     if ( summOpen ) close( iSumm )
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

        call dnBegin( iPrint, iSumm, cw, lencw, iw, leniw, rw, lenrw )

        firstCall = .false.
     end if

     ! Do whatever we need to do.
     if      ( iOpt == dnSolve ) then

        callType = userCall
        call dnmxSolve( nlhs, plhs, nrhs, prhs )
        callType = systemCall

     else if ( iOpt == dnSetX .or. &
               iOpt == dnSetI .or. &
               iOpt == dnSetR .or. &
               iOpt == dnGetX .or. &
               iOpt == dnGetC .or. &
               iOpt == dnGetI .or. &
               iOpt == dnGetR ) then

        callType = userCall
        call dnmxOptions( iOpt, nlhs, plhs, nrhs, prhs )
        callType = systemCall

     else if ( iOpt == dnSpecs ) then

        callType = userCall
        call dnmxSpecs( nlhs, plhs, nrhs, prhs )
        callType = systemCall

     else if ( iOpt == dnSolveN ) then

        callType = userCall
        call dnmxSolveN( nlhs, plhs, nrhs, prhs )
        callType = systemCall

     end if
  end if

  return

end subroutine mexFunction

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine dnmxSolve( nlhs, plhs, nrhs, prhs )
  use mxdnWork
  implicit none
  integer*4  :: nlhs, nrhs
  mwPointer  :: prhs(*), plhs(*)
  !---------------------------------------------------------------------
  ! Solve the problem
  ! The matlab call (based on fmincon) is
  !   [x, fval, exit, itn, y, yA | yAeq | yC, yCeq] =
  !       dnsolve ( obj, x0, A, b | Aeq, beq | xl, xu | nonlcon )
  !
  ! where
  !   obj       is the name of a Matlab function that computes the
  !             objective and its gradient at a given point
  !               [f,g] = obj(x)
  !   x0        is the initial point
  !   A, b      are the linear inequality constraings A*x <= b
  !   Aeq, beq  are the linear equality constraints Aeq*x = beq
  !   xl, xu    are the lower and upper bounds on x
  !   nonlcon   is the name of a Matlab function that computes the
  !             nonlinear inequality constraints and the nonlinear
  !             inequality constraints and the gradients of the
  !             constraints
  !             The constraints are of the form
  !               c(x) <= 0   and ceq(x) = 0
  !               [c,ceq,G,Geq] = nonlcon(x)
  !---------------------------------------------------------------------
  ! Matlab
  mwPointer :: mxGetM, mxGetPr, mxDuplicateArray, &
               mxCreateDoubleMatrix, mxCreateDoubleScalar
  integer   :: mxIsClass
  mwSize    :: tmp, mm, nn

  ! DNOPT
  character*8      :: probName
  integer          :: Errors, i1, i2, info, j
  integer          :: Start, iObj, ldA, ldH, ldJ, m, &
                      mincw, miniw, minrw, nb, nInf, &
                      nlCon, nleq, nlin, nnCon, nnJac, &
                      nnObj, nNames
  double precision :: rtmp, fObj, Obj, ObjAdd, sInf
  external         :: dnKernel, dnLog, dnLog2, &
                      matlabCon, matlabObj, matlabSTOP

  integer,          parameter   :: nItn   = 421, &
                                   nfCon1 = 209, &
                                   nfObj1 = 214
  double precision, parameter   :: infBnd = 1.0d+20

  character*8,      allocatable :: Names(:)
  integer,          allocatable :: state(:)
  double precision, allocatable :: x(:), bl(:), bu(:), y(:), &
                                   H(:,:), A(:,:), Jcon(:,:), &
                                   fCon(:), gObj(:)


  ! Check number of input and output arguments.
  if ( nrhs /= 5 .and. nrhs /= 7 .and. nrhs /= 9 .and. &
       nrhs /= 12 .and. nrhs /= 13  ) &
       call mexErrMsgTxt( 'Wrong number of input arguments' )


  !-----------------------------------------------------------------------------
  ! Objective function
  !-----------------------------------------------------------------------------
  info = mxIsClass( prhs(2), 'function_handle')
  if ( info /= 1 ) call mexErrMsgTxt( 'Wrong input type for obj' )
  objHandle = mxDuplicateArray( prhs(2) )


  !-----------------------------------------------------------------------------
  ! Compute number of variables, constraints, etc
  ! Check dimension of some of the input
  !-----------------------------------------------------------------------------
  ! Get number of variables
  n = mxGetM(prhs(3))


  ! Get number of linear inequality constraint Ax <= b
  nlin = mxGetM(prhs(4))

  if ( nlin > 0 ) call checkCol( prhs(4), n, 'A' )


  ! Get number of linear equality constraints Aeq x = beq
  if ( nrhs > 5 ) then
     nleq = mxGetM(prhs(6))

     if ( nleq > 0 ) call checkCol( prhs(6), n, 'Aeq' )

  else
     nleq = 0
  end if

  ! Set the total number of linear constraints
  nlCon = nlin + nleq


  !-----------------------------------------------------------------------------
  ! Set the linear constraint matrix
  !-----------------------------------------------------------------------------
  if ( nlCon > 0 ) then
     ldA = nlCon
     allocate( A(ldA,n) )

     if ( nlin > 0 ) &
          call mxCopyPtrToReal8( mxGetPr(prhs(4)), A(1:nlin,1:n), nlin*n )

     if ( nleq > 0  ) &
          call mxCopyPtrToReal8( mxGetPr(prhs(6)), A(nlin+1:nlCon,1:n), nleq*n )
  else
     ldA = 1
     allocate( A(ldA,n) )
  end if

  !-----------------------------------------------------------------------------
  ! Get the number of nonlinear constraints
  !-----------------------------------------------------------------------------
  if ( nrhs > 9 ) then
     info = mxIsClass( prhs(10), 'function_handle' )
     if ( info /= 1 ) call mexErrMsgTxt( 'Wrong input type for nonlcon' )

     conHandle = mxDuplicateArray( prhs(10) )


     ! Compute the number of nonlinear constraints nnCon,
     ! the number of nonlinear inequalities nnin and
     ! the number of nonlinear equalities nneq.
     nnin = mxGetM(prhs(11))
     nneq = mxGetM(prhs(12))

  else
     nnin  = 0
     nneq  = 0
  end if

  ! Set the total number of nonlinear constraints
  nnCon = nneq + nnin


  !-----------------------------------------------------------------------------
  ! Set the nonlinear Jacobian matrix
  !-----------------------------------------------------------------------------
  if ( nnCon > 0 ) then

     ldJ = nnCon
     allocate( Jcon(ldJ,n) )

     if ( nnin > 0 ) then
        call checkCol( prhs(11), n, 'J' )
        call mxCopyPtrToReal8( mxGetPr(prhs(11)), Jcon(1:nnin,1:n), nnin*n )
     end if

     if ( nneq > 0 ) then
        call checkCol( prhs(12), n, 'Jeq' )
        call mxCopyPtrToReal8( mxGetPr(prhs(12)), Jcon(nnin+1:nnCon,1:n), nneq*n )
     end if

  else
     ldJ = 1
     allocate( Jcon(1,1) )
  end if


  !-----------------------------------------------------------------------------
  ! Allocate space for DNOPT
  !-----------------------------------------------------------------------------
  m     = nlCon + nnCon   ! total number of constraints
  allocate( bl(n+m), bu(n+m), x(n+m), y(n+m) )

  ! Get initial x
  x = 0.0
  y = 0.0
  call mxCopyPtrToReal8( mxGetPr(prhs(3)), x(1:n), n )


  !-----------------------------------------------------------------------------
  ! Set bounds on variables and constraints
  !-----------------------------------------------------------------------------
  ! Lower and upper bounds on x
  if ( nrhs > 7 ) then

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

  else
     bl(1:n) = -infBnd
     bu(1:n) =  infBnd
  end if


  ! Nonlinear inequality constraint bounds
  if ( nnin > 0 ) then
     i1 = 1+n
     i2 = i1-1+nnin
     bl(i1:i2) = -infBnd
     bu(i1:i2) = 0.0
  end if


  if ( nneq > 0 ) then
     i1 = 1+n+nnin
     i2 = i1-1+nneq
     bl(i1:i2) = 0.0
     bu(i1:i2) = 0.0
  end if

  ! Linear inequality constraint bounds
  i1 = 1+n+nnCon
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


  ! Linear equality constraint bounds
  if ( nleq > 0 ) then
     i1 = 1+n+nnCon+nlin
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
  ! STOP function
  !-----------------------------------------------------------------------------
  if ( nrhs > 12 ) then
     info = mxIsClass( prhs(13), 'function_handle' )
     if ( info /= 1 ) then
        stopHandle = 0
     else
        stopHandle = mxDuplicateArray( prhs(13) )
     end if

  else
     stopHandle = 0
  end if


  !-----------------------------------------------------------------------------
  ! Solve the problem
  !-----------------------------------------------------------------------------
  Start    = 0   ! cold start
  iObj     = 0
  ObjAdd   = 0.0
  nNames   = 1
  nnObj    = n
  nnJac    = 0
  if ( nnCon > 0 ) nnJac = n

  ldH      = n

  allocate( gObj(n), fCon(ldJ), H(ldH,n) )
  allocate( state(n+m), Names(nNames) )

  state    = 0
  probName = 'matlabMx'
  nb       = n + nlCon + nnCon

100 call dnKernel &
         ( start, n, nb, nlCon, nnCon, nnJac, nnObj, &
           probName, Names, nNames, &
           matlabCon, matlabObj, &
           dnLog, dnLog2, matlabSTOP, &
           state, A, ldA, bl, bu, iObj, objAdd, &
           fObj, gObj, fCon, Jcon, ldJ, H, ldH, &
           Obj, nInf, sInf, x, y, &
           INFO, mincw, miniw, minrw, &
           cw, lencw, iw, leniw, rw, lenrw, &
           cw, lencw, iw, leniw, rw, lenrw )

  if ( INFO == 83 ) then
     ! Not enough integer space
     leniw = miniw
     allocate( iw0(leniw) )
     iw0(1:500) = iw(1:500)

     call move_alloc( from=iw0, to=iw )
     call dnSetInt &
          ( 'Total integer workspace', leniw, 0, 0, Errors, &
             cw, lencw, iw, leniw, rw, lenrw )
     go to 100
  end if

  if ( INFO == 84 ) then
    ! Not enough real space
     lenrw = minrw
     allocate ( rw0(lenrw) )
     rw0(1:500) = rw(1:500)

     call move_alloc( from=rw0, to=rw )
     call dnSetInt &
          ( 'Total real workspace', lenrw, 0, 0, Errors, &
            cw, lencw, iw, leniw, rw, lenrw )
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


  ! Function evaluations
  rtmp = iw(nfObj1) + iw(nfCon1)
  if ( nlhs > 4 ) plhs(5) = mxCreateDoubleScalar( rtmp )


  ! Multipliers for bounds
  if ( nlhs > 5 ) then
     plhs(6) = mxCreateDoubleMatrix( n, 1, 0 )
     call mxCopyReal8ToPtr( y(1:n), mxGetPr(plhs(6)), n )
  end if


  ! Multipliers for linear inequalities
  if ( nlhs > 6 ) then
     if ( nlin > 0 ) then
        plhs(7) = mxCreateDoubleMatrix( nlin, 1, 0 )
        i1 = 1+n+nnCon
        i2 = i1-1+nlin
        call mxCopyReal8ToPtr( y(i1:i2), mxGetPr(plhs(7)), nlin )
     else
        plhs(7) = mxCreateDoubleScalar(0.0)
     end if
  end if


  ! Multipliers for linear equalities
  if ( nlhs > 7 ) then
     if ( nleq > 0 ) then
        plhs(8) = mxCreateDoubleMatrix( nleq, 1, 0 )
        i1 = 1+n+nnCon+nlin
        i2 = i1-1+nleq
        call mxCopyReal8ToPtr( y(i1:i2), mxGetPr(plhs(8)), nleq )
     else
        plhs(8) = mxCreateDoubleScalar(0.0)
     end if
  end if


  ! Multipliers for nonlinear inequalities
  if ( nlhs > 8 ) then
     if ( nnin > 0 ) then
        plhs(9) = mxCreateDoubleMatrix( nnin, 1, 0 )
        i1 = 1+n
        i2 = i1-1+nnin
        call mxCopyReal8ToPtr( y(i1:i2), mxGetPr(plhs(9)), nnin )
     else
        plhs(9) = mxCreateDoubleScalar(0.0)
     end if
  end if


  ! Multipliers for nonlinear equalities
  if ( nlhs > 9 ) then
     if ( nneq > 0 ) then
        plhs(10) = mxCreateDoubleMatrix( nneq, 1, 0 )
        i1 = 1+n+nnin
        i2 = i1-1+nneq
        call mxCopyReal8ToPtr( y(i1:i2), mxGetPr(plhs(10)), nneq )
     else
        plhs(10) = mxCreateDoubleScalar(0.0)
     end if
  end if


  ! Deallocate memory
  if ( allocated(Names) ) deallocate( Names )
  if ( allocated(state) ) deallocate( state )

  if ( allocated(x) )     deallocate( x )
  if ( allocated(bl) )    deallocate( bl )
  if ( allocated(bu) )    deallocate( bu )
  if ( allocated(y) )     deallocate( y )

  if ( allocated(H) )     deallocate( H )
  if ( allocated(A) )     deallocate( A )
  if ( allocated(Jcon) )  deallocate( Jcon )

  if ( allocated(fCon) )  deallocate( fCon )
  if ( allocated(gObj) )  deallocate( gObj )

end subroutine dnmxSolve

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine dnmxOptions ( iOpt, nlhs, plhs, nrhs, prhs )
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

  integer          :: dnGet
  external         :: dnSet, dnSetInt, dnSetReal, &
                      dnGet, dnGetChar, dnGetInt, dnGetReal


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
     call dnSet( buffer, iPrint, iSumm, Errors, &
                 cw, lencw, iw, leniw, rw, lenrw )

  else if ( iOpt == dnSetI  ) then

     rvalue = mxGetScalar(prhs(3))
     ivalue = rvalue

     call dnSetInt( buffer, ivalue, iPrint, iSumm, Errors, &
                    cw, lencw, iw, leniw, rw, lenrw )

  else if ( iOpt == dnSetR  ) then

     rvalue = mxGetScalar(prhs(3))

     call dnSetReal( buffer, rvalue, iPrint, iSumm, Errors, &
                     cw, lencw, iw, leniw, rw, lenrw )

  else if ( iOpt == dnGetX  ) then

     ivalue  = dnGet( buffer, Errors, cw, lencw, iw, leniw, rw, lenrw )

     rvalue  = ivalue
     plhs(1) = mxCreateDoubleScalar( rvalue )

  else if ( iOpt == dnGetC  ) then

     call dnGetChar( buffer, cvalue, Errors, &
                     cw, lencw, iw, leniw, rw, lenrw )

     plhs(1) = mxCreateString(cvalue)

  else if ( iOpt == dnGetI  ) then

     call dnGetInt( buffer, ivalue, Errors, &
                    cw, lencw, iw, leniw, rw, lenrw )

     rvalue = ivalue
     plhs(1) = mxCreateDoubleScalar( rvalue )

  else if ( iOpt == dnGetR  ) then

     call dnGetReal( buffer, rvalue, Errors, &
                     cw, lencw, iw, leniw, rw, lenrw )

     plhs(1) = mxCreateDoubleScalar( rvalue )

  end if

end subroutine dnmxOptions

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine dnmxSpecs( nlhs, plhs, nrhs, prhs )
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

  external         :: dnSpec


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
  call dnSpec( iSpecs, info, cw, lencw, iw, leniw, rw, lenrw )
  rewind( iSpecs )
  close( iSpecs )

  ! dnSpec will return info == 101 or 107 if successful
  ! The matlab version returns 0 if successful
  if ( info == 101 .or. info == 107 ) then
     rvalue = 0
  else
     rvalue = 1
  end if

  plhs(1) = mxCreateDoubleScalar( rvalue )

end subroutine dnmxSpecs

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine dnmxSolveN( nlhs, plhs, nrhs, prhs )
  use mxdnWork
  implicit none
  integer*4  :: nlhs, nrhs
  mwPointer  :: prhs(*), plhs(*)
  !---------------------------------------------------------------------
  ! Solve the problem
  ! The matlab call is
  !   [x,fval,exit] =
  !     dnsolve ( name, obj, x0, xl, xu | A, lA, uA | nonlcon, lc, uc )
  !           optional: [A, lA, uA], [nonlcon, lc, uc]
  !
  ! where
  !   name      is the problem name
  !   obj       is the name of a Matlab function that computes the
  !             objective and its gradient at a given point
  !               [f,g] = obj(x)
  !   x0        is the initial point
  !   xl, xu    are the lower and upper bounds on x
  !   A, lA, uA are the linear inequality constraings lA <= A*x <= uA
  !   nonlcon, is the name of a Matlab function that computes the
  !             nonlinear inequality constraints and the nonlinear
  !             inequality constraints and the associated Jacobian
  !             matrices
  !   lc, uc    are the lower and upper bounds on the nonlinear
  !             constraints
  !             The constraints are of the form
  !               lc <= c(x) <= uc
  !               [c,J] = nonlcon(x)
  !
  !---------------------------------------------------------------------
  ! Matlab
  mwPointer :: mxGetM, mxGetN, mxGetPr, mxDuplicateArray, &
               mxCreateDoubleMatrix, mxCreateDoubleScalar
  integer   :: mxIsChar, mxIsClass

  ! DNOPT
  character*8      :: probName
  integer          :: Errors, i1, i2, info, strlen
  integer          :: Start, iObj, ldA, ldH, ldJ, m, nb, &
                      mincw, miniw, minrw, nInf, nlCon, &
                      nnCon, nnJac, nnObj, nNames
  double precision :: rtmp, fObj, Obj, ObjAdd, sInf
  external         :: dnopt, dnKernel, dnLog, dnLog2, &
                      matlabConN, matlabObj, matlabSTOP

  integer,          parameter   :: nItn   = 421, &
                                   nfCon1 = 209, &
                                   nfObj1 = 214
  double precision, parameter   :: infBnd = 1.0d+20

  character*8,      allocatable :: Names(:)
  integer,          allocatable :: state(:)
  double precision, allocatable :: x(:), bl(:), bu(:), y(:), &
                                   H(:,:), A(:,:), Jcon(:,:), &
                                   fCon(:), gObj(:)


  ! Check number of input and output arguments.
  if ( nrhs /= 7 .and. nrhs /= 10 .and. nrhs /= 14 ) &
       call mexErrMsgTxt( 'Wrong number of input arguments' )


  !-----------------------------------------------------------------------------
  ! stopFunction handle
  !-----------------------------------------------------------------------------
  info = mxIsClass( prhs(2), 'function_handle')
  if ( info /= 1 ) then
     stopHandle = 0
  else
     stopHandle = mxDuplicateArray( prhs(2) )
  end if


  !-----------------------------------------------------------------------------
  ! Problem name
  !-----------------------------------------------------------------------------
  info = mxIsChar( prhs(3) )
  if ( info /= 1 ) call mexErrMsgTxt( 'Wrong input type for problem name' )

  strlen = mxGetN(prhs(3))
  if ( strlen > 8 ) strlen = 8

  probName = ''
  call mxGetString( prhs(3), probName, strlen )


  !-----------------------------------------------------------------------------
  ! Objective function
  !-----------------------------------------------------------------------------
  info = mxIsClass( prhs(4), 'function_handle')
  if ( info /= 1 ) call mexErrMsgTxt( 'Wrong input type for obj' )
  objHandle = mxDuplicateArray( prhs(4) )


  !-----------------------------------------------------------------------------
  ! Compute number of variables, constraints, etc
  ! Check dimension of some of the input
  !-----------------------------------------------------------------------------
  ! Get number of variables
  n = mxGetM(prhs(5))


  ! Get number of linear inequality constraint lA <= Ax <= uA
  if ( nrhs > 7 ) then
     nlCon = mxGetM(prhs(8))
     if ( nlCon > 0 ) call checkCol( prhs(8), n, 'A' )

  else
     nlCon = 0
  end if


  !-----------------------------------------------------------------------------
  ! Set the linear constraint matrix
  !-----------------------------------------------------------------------------
  if ( nlCon > 0 ) then
     ldA = nlCon
     allocate( A(ldA,n) )

     call mxCopyPtrToReal8( mxGetPr(prhs(8)), A(1:nlCon,1:n), nlCon*n )
  else
     ldA = 1
     allocate( A(ldA,n) )
  end if


  !-----------------------------------------------------------------------------
  ! Get the number of nonlinear constraints
  !-----------------------------------------------------------------------------
  if ( nrhs > 10 ) then
     info = mxIsClass( prhs(11), 'function_handle' )
     if ( info /= 1 ) call mexErrMsgTxt( 'Wrong input type for nonlcon' )

     conHandle = mxDuplicateArray( prhs(11) )

     ! Set the total number of nonlinear constraints
     nnCon = mxGetM(prhs(14))

  else
     nnCon = 0
  end if


  !-----------------------------------------------------------------------------
  ! Set the nonlinear Jacobian matrix
  !-----------------------------------------------------------------------------
  if ( nnCon > 0 ) then
     ldJ = nnCon
     allocate( Jcon(ldJ,n) )

     call checkCol( prhs(14), n, 'J' )
     call mxCopyPtrToReal8( mxGetPr(prhs(14)), Jcon(1:nnCon,1:n), nnCon*n )

  else
     ldJ = 1
     allocate( Jcon(1,1) )
  end if


  !-----------------------------------------------------------------------------
  ! Allocate space for DNOPT
  !-----------------------------------------------------------------------------
  m     = nlCon + nnCon   ! total number of constraints
  allocate( bl(n+m), bu(n+m), x(n+m), y(n+m) )

  ! Get initial x
  x = 0.0
  y = 0.0
  call mxCopyPtrToReal8( mxGetPr(prhs(5)), x(1:n), n )


  !-----------------------------------------------------------------------------
  ! Set bounds on variables and constraints
  !-----------------------------------------------------------------------------
  ! Lower and upper bounds on x
  if ( nrhs > 5 ) then

     ! Check dimension of xl and xu
     if ( mxGetM(prhs(6)) > 0 ) then
        call checkRow( prhs(6), n, 'xl' )
        call checkCol( prhs(6), 1, 'xl' )
        call mxCopyPtrToReal8( mxGetPr(prhs(6)), bl(1:n), n )
     else
        bl(1:n) = -infBnd
     end if

     if ( mxGetM(prhs(7)) > 0 ) then
        call checkRow( prhs(7), n, 'xu' )
        call checkCol( prhs(7), 1, 'xu' )
        call mxCopyPtrToReal8( mxGetPr(prhs(7)), bu(1:n), n )
     else
        bu(1:n) = infBnd
     end if

  else
     bl(1:n) = -infBnd
     bu(1:n) =  infBnd
  end if


  ! Nonlinear constraint bounds
  if ( nnCon > 0 ) then
     i1 = 1+n
     i2 = i1-1+nnCon

     if ( mxGetM(prhs(12)) > 0 ) then
        call checkRow( prhs(12), nnCon, 'cl' )
        call checkCol( prhs(12),     1, 'cl' )
        call mxCopyPtrToReal8( mxGetPr(prhs(12)), bl(i1:i2), nnCon )
     else
        bl(i1:i2) = -infBnd
     end if

     if ( mxGetM(prhs(13)) > 0 ) then
        call checkRow( prhs(13), nnCon, 'cu' )
        call checkCol( prhs(13),     1, 'cu' )
        call mxCopyPtrToReal8( mxGetPr(prhs(13)), bu(i1:i2), nnCon )
     else
        bu(i1:i2) = infBnd
     end if

  end if


  ! Linear constraint bounds
  if ( nlCon > 0 ) then
     i1 = 1+n+nnCon
     i2 = i1-1+nlCon

     ! Check dimension of lA and uA
     if ( mxGetM(prhs(9)) > 0 ) then
        call checkRow( prhs(9), nlCon, 'al' )
        call checkCol( prhs(9),     1, 'al' )
        call mxCopyPtrToReal8( mxGetPr(prhs(9)), bl(i1:i2), nlCon )
     else
        bl(i1:i2) = -infBnd
     end if

     if ( mxGetM(prhs(10)) > 0 ) then
        call checkRow( prhs(10), nlCon, 'au' )
        call checkCol( prhs(10),     1, 'au' )
        call mxCopyPtrToReal8( mxGetPr(prhs(10)), bu(i1:i2), nlCon )
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
  nNames   = 1
  nnObj    = n
  nnJac    = 0
  if ( nnCon > 0 ) nnJac = n

  ldH      = n

  allocate( gObj(n), fCon(ldJ), H(ldH,n) )
  allocate( state(n+m), Names(nNames) )

  state    = 0

  nb = n + nlCon + nnCon
100 call dnKernel &
         ( start, n, nb, nlCon, nnCon, nnJac, nnObj, &
           probName, Names, nNames, &
           matlabConN, matlabObj, &
           dnLog, dnLog2, matlabSTOP, &
           state, A, ldA, bl, bu, iObj, objAdd, &
           fObj, gObj, fCon, Jcon, ldJ, H, ldH, &
           Obj, nInf, sInf, x, y, &
           INFO, mincw, miniw, minrw, &
           cw, lencw, iw, leniw, rw, lenrw, &
           cw, lencw, iw, leniw, rw, lenrw )

  if ( INFO == 83 ) then
     ! Not enough integer space
     leniw = miniw
     allocate( iw0(leniw) )
     iw0(1:500) = iw(1:500)

     call move_alloc( from=iw0, to=iw )
     call dnSetInt &
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
     call dnSetInt &
          ( 'Total real      workspace', lenrw, 0, 0, Errors, &
            cw, lencw, iw, leniw, rw, lenrw )
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


  ! Function evaluations
  rtmp = iw(nfObj1) + iw(nfCon1)
  if ( nlhs > 4 ) plhs(5) = mxCreateDoubleScalar( rtmp )


  ! Multipliers for bounds
  if ( nlhs > 5 ) then
     plhs(6) = mxCreateDoubleMatrix( n, 1, 0 )
     call mxCopyReal8ToPtr( y(1:n), mxGetPr(plhs(6)), n )
  end if


  ! Multipliers for linear constraints
  if ( nlhs > 6 ) then
     if ( nlCon > 0 ) then
        plhs(7) = mxCreateDoubleMatrix( nlCon, 1, 0 )
        i1 = 1+n+nnCon
        i2 = i1-1+nlCon
        call mxCopyReal8ToPtr( y(i1:i2), mxGetPr(plhs(7)), nlCon )
     else
        plhs(7) = mxCreateDoubleScalar(0.0)
     end if
  end if


  ! Multipliers for nonlinear inequalities
  if ( nlhs > 7 ) then
     if ( nnCon > 0 ) then
        plhs(8) = mxCreateDoubleMatrix( nnCon, 1, 0 )
        i1 = 1+n
        i2 = i1-1+nnCon
        call mxCopyReal8ToPtr( y(i1:i2), mxGetPr(plhs(8)), nnCon )
     else
        plhs(8) = mxCreateDoubleScalar(0.0)
     end if
  end if


  ! Deallocate memory
  if ( allocated(Names) ) deallocate( Names )
  if ( allocated(state) ) deallocate( state )

  if ( allocated(x) )     deallocate( x )
  if ( allocated(bl) )    deallocate( bl )
  if ( allocated(bu) )    deallocate( bu )
  if ( allocated(y) )     deallocate( y )

  if ( allocated(H) )     deallocate( H )
  if ( allocated(A) )     deallocate( A )
  if ( allocated(Jcon) )  deallocate( Jcon )

  if ( allocated(fCon) )  deallocate( fCon )
  if ( allocated(gObj) )  deallocate( gObj )

end subroutine dnmxSolveN

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine matlabObj( mode, nnObj, x, fObj, gObj, Status, &
                      cu, lencu, iu, leniu, ru, lenru )
  use mxdnWork
  implicit none

  integer          :: mode, nnObj, Status, lencu, leniu, lenru, &
                      iu(leniu)
  double precision :: fObj, x(nnObj), gObj(nnObj), ru(lenru)
  character*8      :: cu(lencu)

  !---------------------------------------------------------------------
  ! Matlab callback to evaluate objective function and gradient at x.
  !---------------------------------------------------------------------
  integer*4        :: nlhs, nrhs
  mwPointer        :: prhs(2), plhs(2)
  mwPointer        :: mxGetM, mxGetN, mxGetPr, mxCreateDoubleMatrix, &
                      mxDuplicateArray
  double precision :: mxGetScalar
  mwSize           :: mm, nn

  nlhs = 2
  nrhs = 2

  prhs(1) = mxDuplicateArray( objHandle )
  prhs(2) = mxCreateDoubleMatrix( nnObj, 1, 0 )

  call mxCopyReal8ToPtr( x, mxGetPr(prhs(2)), nnObj )

  ! Call Matlab: [f,g] = obj(x)
  !  dncon calls conHandle(x), then transposes gradients to get the Jacobian
  call mexCallMatlab( nlhs, plhs, nrhs, prhs, 'dnobj' )

  if ( nlhs < 2 ) &
       call mexErrMsgTxt( 'User must provide the objective gradient in obj' )
  if ( nlhs /= 2 ) &
       call mexErrMsgTxt( 'Incorrect number of output arguments in obj' )


  ! Set fObj and gObj
  if ( mode == 0 .or. mode == 2 ) then
     fObj = mxGetScalar(plhs(1))
  end if

  if ( mode == 1 .or. mode == 2 ) then
     fObj = mxGetScalar(plhs(1))

     mm = mxGetM( plhs(2) )
     nn = mxGetN( plhs(2) )

     if ( (mm == 1 .and. nn == nnObj) .or. (mm == nnObj .and. nn == 1) ) then
        call mxCopyPtrToReal8( mxGetPr(plhs(2)), gObj, nnObj )
     else if ( mm == 0 .and. nn == 0 ) then
     else
        call mexErrMsgTxt ( 'gObj has incorrect dimensions ' )
     end if
  end if


  ! Destroy arrays
  call mxDestroyArray( plhs(1) )
  call mxDestroyArray( plhs(2) )

  call mxDestroyArray( prhs(1) )
  call mxDestroyArray( prhs(2) )

end subroutine matlabObj

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine matlabCon( mode, nnCon, nnJac, x, fCon, Jcon, ldJ, Status, &
                      cu, lencu, iu, leniu, ru, lenru )
  use mxdnWork
  implicit none

  integer          :: mode, nnCon, nnJac, ldJ, Status, &
                      lencu, leniu, lenru, iu(leniu)
  double precision :: fCon(nnCon), x(nnJac), JCon(ldJ,*), ru(lenru)
  character*8      :: cu(lencu)

  !---------------------------------------------------------------------
  ! Matlab callback to evaluate nonlinear constraints and Jacobian at x.
  !
  ! The call is
  !   [c,ceq,J,Jeq] = funcon(x)
  ! where c(x) <= 0 and ceq(x) = 0 are the nonlinear inequality and
  ! equality constraints and J and Jeq are the respective Jacobian
  ! matrices.
  !---------------------------------------------------------------------
  integer*4  :: nlhs, nrhs
  mwPointer  :: prhs(2), plhs(4)
  mwPointer  :: mxGetM, mxGetN, mxGetPr, &
                mxCreateDoubleMatrix, mxDuplicateArray
  mwSize     :: mm, nn

  if ( status .ge. 2 ) return

  nlhs = 4
  nrhs = 2

  if ( nnCon > 0 ) then
     prhs(1) = mxDuplicateArray( conHandle )
     prhs(2) = mxCreateDoubleMatrix( nnJac, 1, 0 )
     call mxCopyReal8ToPtr( x, mxGetPr(prhs(2)), nnJac )


     ! Call Matlab:  [c,ceq,J,Jeq] = dncon(conHandle,x)
     !  dncon calls conHandle(x), then transposes gradients to get the Jacobian
     call mexCallMatlab( nlhs, plhs, nrhs, prhs, 'dncon' )

     if ( nlhs < 4 ) &
          call mexErrMsgTxt( 'User must provide the Jacobian matrix in nonlcon' )
     if ( nlhs /= 4 ) &
          call mexErrMsgTxt( 'Incorrect number of output arguments in nonlcon' )

     ! Set output
     if ( mode == 0 .or. mode == 2 ) then
        if ( nnin > 0 ) then
           call checkRow( plhs(1), nnin, 'c' )
           call checkCol( plhs(1),    1, 'c' )
           call mxCopyPtrToReal8( mxGetPr(plhs(1)), fCon(1:nnin),       nnin )
        end if

        if ( nneq > 0 ) then
           call checkRow( plhs(2), nneq, 'ceq' )
           call checkCol( plhs(2),    1, 'ceq' )
           call mxCopyPtrToReal8( mxGetPr(plhs(2)), fCon(1+nnin:nnCon), nneq )
        end if
     end if

     if ( mode == 1 .or. mode == 2 ) then
        if ( nnin > 0 ) then
           mm = mxGetM( plhs(3) )
           nn = mxGetN( plhs(3) )

           if ( mm /= 0 .or. nn /= 0 ) then
              call checkRow( plhs(3), nnin, 'J' )
              call checkCol( plhs(3),    n, 'J' )
              call mxCopyPtrToReal8( mxGetPr(plhs(3)), Jcon(1:nnin,1:n), nnin*n )
           end if
        end if

        if ( nneq > 0 ) then
           mm = mxGetM( plhs(4) )
           nn = mxGetN( plhs(4) )

           if ( mm /= 0 .or. nn /= 0 ) then
              call checkRow( plhs(4), nneq, 'Jeq' )
              call checkCol( plhs(4),    n, 'Jeq' )
              call mxCopyPtrToReal8( mxGetPr(plhs(4)), Jcon(1+nnin:nnCon,1:n), nneq*n )
           end if
        end if
     end if


     ! Destroy arrays
     call mxDestroyArray( plhs(1) )
     call mxDestroyArray( plhs(2) )
     call mxDestroyArray( plhs(3) )
     call mxDestroyArray( plhs(4) )

     call mxDestroyArray( prhs(1) )
     call mxDestroyArray( prhs(2) )
  end if

end subroutine matlabCon

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine matlabConN( mode, nnCon, nnJac, x, fCon, Jcon, ldJ, Status, &
                       cu, lencu, iu, leniu, ru, lenru )
  use mxdnWork
  implicit none
  integer          :: mode, nnCon, nnJac, ldJ, Status, &
                      lencu, leniu, lenru, iu(leniu)
  double precision :: fCon(nnCon), x(nnJac), JCon(ldJ,*), ru(lenru)
  character*8      :: cu(lencu)

  !---------------------------------------------------------------------
  ! Matlab callback to evaluate nonlinear constraints and Jacobian at x.
  !
  ! The call is
  !   [c,J] = funcon(x)
  ! where lc <= c(x) <= uc are the nonlinear constraints (lc and uc are
  ! defined elsewhere) and J is the Jacobian matrix.
  !---------------------------------------------------------------------
  integer*4  :: nlhs, nrhs
  mwPointer  :: prhs(2), plhs(2)
  mwPointer  :: mxGetM, mxGetN, mxGetPr, &
                mxCreateDoubleMatrix, mxDuplicateArray
  mwSize     :: mm, nn

  if ( status .ge. 2 ) return

  if ( nnCon > 0 ) then
     nlhs = 2
     nrhs = 2

     prhs(1) = mxDuplicateArray( conHandle )
     prhs(2) = mxCreateDoubleMatrix( nnJac, 1, 0 )
     call mxCopyReal8ToPtr( x, mxGetPr(prhs(2)), nnJac )


     ! Call Matlab: [c,J] = dnconN(nonlcon,x)
     !  dnconN calls conHandle(x), then transposes gradients to get the Jacobian
     call mexCallMatlab( nlhs, plhs, nrhs, prhs, 'dnconN' )

     if ( nlhs < 2 ) &
          call mexErrMsgTxt( 'User must provide the Jacobian matrix in nonlcon' )
     if ( nlhs /= 2 ) &
          call mexErrMsgTxt( 'Incorrect number of output arguments in nonlcon' )


     ! Set output
     if ( mode == 0 .or. mode == 2 ) then
        call checkRow( plhs(1), nnCon, 'c' )
        call checkCol( plhs(1),     1, 'c' )

        call mxCopyPtrToReal8( mxGetPr(plhs(1)), fCon(1:nnCon), nnCon )
     end if

     if ( mode == 1 .or. mode == 2 ) then
        mm = mxGetM( plhs(2) )
        nn = mxGetN( plhs(2) )

        if ( mm /= 0 .or. nn /= 0 ) then
           call checkRow( plhs(2), nnCon, 'J' )
           call checkCol( plhs(2),     n, 'J' )

           call mxCopyPtrToReal8( mxGetPr(plhs(2)), Jcon(1:nnCon,1:n), nnCon*n )
        end if
     end if

     ! Destroy arrays
     call mxDestroyArray( plhs(1) )
     call mxDestroyArray( plhs(2) )

     call mxDestroyArray( prhs(1) )
     call mxDestroyArray( prhs(2) )
  end if

end subroutine matlabConN

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine matlabSTOP &
     ( iAbort, &
       KTcond, minimize, mCon0, mCon, &
       maxZ, n, nb, nnCon0, nnCon, nnObj0, nnObj, nZ, &
       itn, nMajor, nMinor, &
       condHz, ObjAdd, fMerit, penParm, step, &
       primalInf, dualInf, maxVi, maxViRel, state, &
       Scales, bl, bu, fObj, gObj, fCon, fmul, &
       JQP, ldJQP, gQ, x, yCon, &
       cu, lencu, iu, leniu, ru, lenru, &
       cw, lencw, iw, leniw, rw, lenrw )
  use mxdnWork, only : stopHandle
  implicit none

  logical          :: KTcond(2)
  integer          :: iAbort, itn, ldJQP, &
                      lencu, lencw, leniu, leniw, lenru, lenrw, &
                      minimize, m, mCon0, mCon, maxZ, n, nb, &
                      nnCon0, nnCon, nnObj0, nnObj, nMajor, nMinor, nZ, nZwap, &
                      state(nb), iu(leniu), iw(leniw)
  double precision :: condHz, ObjAdd, fMerit, fObj, penParm(4), &
                      maxVIRel, maxVi, primalInf, dualInf, step, &
                      Scales(nb), bl(nb), bu(nb),fCon(nnCon0), &
                      fmul(nnCon0), gObj(nnObj0), gQ(n), &
                      JQP(ldJQP,*), x(nb), yCon(mCon0), &
                      ru(lenru), rw(lenrw)
  character        :: cu(lencu)*8, cw(lencw)*8

  !---------------------------------------------------------------------
  ! This is the mex version of dnSTOP for DNOPT7.   It is called every
  ! major iteration.
  ! If iAbort > 0 on exit, the run is terminated.
  !---------------------------------------------------------------------
  integer*4  :: nlhs, nrhs
  mwPointer  :: prhs(2), plhs(1)
  mwPointer  :: mxGetPr, mxCreateDoubleMatrix, mxDuplicateArray
  double precision :: rAbort, mxGetScalar

  iAbort = 0
  ! If iAbort > 0 on exit, the run is terminated.

  if ( stopHandle /= 0 ) then
     nlhs    = 1
     nrhs    = 2

     prhs(1) = mxDuplicateArray( stopHandle )
     prhs(2) = mxCreateDoubleMatrix( n, 1, 0 )
     call mxCopyReal8ToPtr( x(1:n), mxGetPr(prhs(2)), n )

     ! Call Matlab: stop = stopFunction(x)
     call mexCallMatlab( nlhs, plhs, nrhs, prhs, 'feval' )


     ! Get iAbort siginal
     rAbort = mxGetScalar(plhs(1))
     iAbort = rAbort

  end if

end subroutine matlabSTOP

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
