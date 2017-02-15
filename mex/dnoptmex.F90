!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! File dnoptmex.F90
! Mex function for DNOPT
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include "fintrf.h"

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine mexFunction(nlhs, plhs, nrhs, prhs)
  use mxdnWork
  implicit none

  integer*4  :: nlhs, nrhs
  mwPointer  :: prhs(*), plhs(*)
  !=====================================================================
  ! Mex function for DNOPT
  !
  ! Option      Action
  !    1        Solve the problem
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
  !    14       Set workspace
  !    15       Screen on
  !    16       Screen off
  !
  ! 11 Feb 2017: Current version.
  !=====================================================================
  ! Matlab
  mwPointer        :: mxGetN, mxGetPr
  mwSize           :: dim
  integer*4        :: mxIsChar
  double precision :: mxGetScalar

  ! DNOPT
  character        :: filename*80
  integer          :: info, iOpt, strlen
  double precision :: rOpt, rleniw, rlenrw
  external         :: dnBegin


  ! Get option.
  if (nrhs < 1) call mexErrMsgIdAndTxt('DNOPT:InputArgs','Need an option input argument')
  rOpt = mxGetScalar(prhs(1))
  iOpt = rOpt

  ! Register exit function
  call mexAtExit( resetDNOPT )

  ! Files
  if (iOpt == dnOpenP) then

     if (nrhs /= 2) &
          call mexErrMsgIdAndTxt('DNOPT:InputArgs','Wrong number of input arguments')

     if (mxIsChar(prhs(2)) /= 1) &
          call mexErrMsgIdAndTxt('DNOPT:FileArgs','Need filename string')

     strlen = mxGetN(prhs(2))
     if (strlen > 80) call mexErrMsgIdAndTxt('DNOPT:FileArgs','Print filename is too long')

     if (strlen > 0) then
        dim = strlen
        call mxGetString(prhs(2), filename, dim)
     else
        call mexErrMsgIdAndTxt('DNOPT:FileArgs','Empty print filename')
     end if

     if (printOpen) close(iPrint)

     open (iPrint, file=filename, status='unknown')
     printOpen = .true.
     return

  else if (iOpt == dnOpenS) then

     if (nrhs /= 2) call mexErrMsgIdAndTxt('DNOPT:InputArgs','Wrong number of input arguments')

     if (mxIsChar(prhs(2)) /= 1) &
          call mexErrMsgIdAndTxt('DNOPT:FileArgs','Need filename string')

     strlen = mxGetN(prhs(2))
     if (strlen > 80) call mexErrMsgIdAndTxt('DNOPT:FileArgs','Summary filename is too long')

     if (strlen > 0) then
        dim = strlen
        call mxGetString(prhs(2), filename, dim)
     else
        call mexErrMsgIdAndTxt('DNOPT:FileArgs','Empty summary filename')
     end if

     if (summOpen) close(iSumm)

     open (iSumm, file=filename, status='unknown')
     summOpen = .true.
     return

  else if (iOpt == dnClosP) then
     if (printOpen) close(iPrint)
     printOpen = .false.
     return

  else if (iOpt == dnClosS) then
     if (summOpen) close(iSumm)
     summOpen = .false.
     return

  else if (iOpt == dnscrnOn) then
     screenOn = .true.
     return

  else if (iOpt == dnscrnOff) then
     screenOn = .false.
     return

  else if (iOpt == dnSetWork) then
     rleniw = mxGetScalar(prhs(2))
     rlenrw = mxGetScalar(prhs(3))
     leniw  = rleniw
     lenrw  = rlenrw

     if (leniw < 500 .or. lenrw < 500) &
          call mexErrMsgIdAndTxt('DNOPT:Workspace','Workspace size must be at least 500')
     return
  end if

  ! What calls get to this point:
  !  dnopt, dnSolve, dnGet, dnSet, dnSpecs, dnEnd

  if (firstCall) then
     if (allocated(cw)) deallocate(cw)
     if (allocated(iw)) deallocate(iw)
     if (allocated(rw)) deallocate(rw)
     allocate(cw(lencw), iw(leniw), rw(lenrw))

     callType = userCall
     call dnBegin(iPrint, iSumm, cw, lencw, iw, leniw, rw, lenrw)
     callType = systemCall

     memCall   = .false.
     firstCall = .false.
  end if

  ! Do whatever we need to do.
  if      (iOpt == dnSolve) then

     callType = userCall
     call dnmxSolve(nlhs, plhs, nrhs, prhs, iOpt)
     callType = systemCall

  else if (iOpt == dnSetXX .or. &
           iOpt == dnSetIX .or. &
           iOpt == dnSetRX .or. &
           iOpt == dnGetXX .or. &
           iOpt == dnGetCX .or. &
           iOpt == dnGetIX .or. &
           iOpt == dnGetRX) then

     callType = userCall
     call dnmxOptions(iOpt, nlhs, plhs, nrhs, prhs)
     callType = systemCall

  else if (iOpt == dnSpecs) then

     callType = userCall
     call dnmxSpecs(nlhs, plhs, nrhs, prhs)
     callType = systemCall

  else if (iOpt == dnEnd) then

     call resetDNOPT

  end if

  return

end subroutine mexFunction

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine dnmxSolve(nlhs, plhs, nrhs, prhs, iOpt)
  use mxdnWork
  implicit none

  integer*4  :: nlhs, nrhs
  mwPointer  :: prhs(*), plhs(*)
  integer    :: iOpt

  !---------------------------------------------------------------------
  ! Solve the problem
  ! [x,fval,exitFlag,itn,nEval,y,istate] =
  !          dnoptmex(solveOpt, istart, stopFun, probName, ...
  !                   objFun, x0, xl, xu, xstate, xmul,...
  !                   A, al, au, astate, amul, ...
  !                   nonlcon, cl, cu, cstate, cmul, J);
  !---------------------------------------------------------------------
  ! Matlab
  mwPointer        :: mxDuplicateArray, mxGetM, mxGetN, mxGetPr, &
                      mxCreateDoubleMatrix, mxCreateDoubleScalar
  mwSize           :: dimx, dimy
  integer*4        :: mxIsChar, mxIsClass, mxIsNumeric
  double precision :: mxGetScalar

  ! DNOPT
  character*8      :: probName, Names(1)
  integer          :: Errors, i1, i2, info
  integer          :: Start, iObj, ldA, ldH, ldJ, m, nb, &
                      mincw, miniw, minrw, nInf, nlCon, &
                      nnCon, nnJac, nnObj, nNames
  double precision :: rtmp, fObj, Obj, ObjAdd, sInf
  external         :: dnoptKernel, dnSetInt, dnLog, dnLogQP, &
                      dnoptInterfaceB, dnfunHxNull, &
                      matlabCon, matlabObj, matlabSTOP

  integer,          parameter   :: nItn   = 421, &
                                   nfCon1 = 209, &
                                   nfObj1 = 214, &
                                   izero  = 0
  double precision, parameter   :: zero = 0.0d+0, infBnd = 1.0d+20


  ! Check number of input and output arguments.
  if (nrhs /= 10 .and. nrhs /= 15 .and. nrhs /= 21 ) &
       call mexErrMsgIdAndTxt('DNOPT:InputArgs','Wrong number of input arguments')


  !-----------------------------------------------------------------------------
  ! Compute number of variables, constraints, etc
  !-----------------------------------------------------------------------------
  ! Get number of variables
  n = mxGetM(prhs(6))

  ! Get number of linear inequality constraint al <= Ax <= au
  if (nrhs > 10) then
     nlCon = mxGetM(prhs(11))
  else
     nlCon = 0
  end if

  ! Get the number of nonlinear constraints
  if (nrhs > 20) then
     nnCon = mxGetM(prhs(21))
  else
     nnCon = 0
  end if


  !-----------------------------------------------------------------------------
  ! Allocate space for DNOPT
  !-----------------------------------------------------------------------------
  m        = nlCon + nnCon   ! total number of constraints
  nnObj    = n
  ldH      = n
  nnJac    = 0
  if (nnCon > 0) nnJac = n

  call allocDNOPT(n, m, nlCon, nnCon)


  !-----------------------------------------------------------------------------
  ! Start
  !-----------------------------------------------------------------------------
  start = mxGetScalar(prhs(2))


  !-----------------------------------------------------------------------------
  ! STOP function
  !-----------------------------------------------------------------------------
  stopHandle = 0
  if (mxIsNumeric(prhs(3)) /= 1) then
     ! Check if STOP functions is actually a function
     if (mxIsClass(prhs(3), 'function_handle') /= 1) &
          call mexErrMsgIdAndTxt('DNOPT:INputArgs','Wrong input type for dnSTOP')
     stopHandle = mxDuplicateArray(prhs(3))
  end if


  !-----------------------------------------------------------------------------
  ! Problem name
  !-----------------------------------------------------------------------------
  probName = ''
  if (mxIsChar(prhs(4)) /= 1) &
       call mexErrMsgIdAndTxt('DNOPT:InputArg','Wrong input type for problem name')

  dimx = min(8,mxGetN(prhs(4)))
  call mxGetString(prhs(4), probName, dimx)


  !-----------------------------------------------------------------------------
  ! Constraint function
  !-----------------------------------------------------------------------------
  if (nrhs > 15) then
     if (mxIsClass(prhs(16), 'function_handle') /= 1) &
          call mexErrMsgIdAndTxt('DNOPT:InputArgs','Wrong input type for nonlcon')
     conHandle = mxDuplicateArray(prhs(16))
  end if


  !-----------------------------------------------------------------------------
  ! Objective function
  !-----------------------------------------------------------------------------
  if (mxIsClass(prhs(5), 'function_handle') /= 1) &
       call mexErrMsgIdAndTxt('DNOPT:InputArgs','Wrong input type for obj')
  objHandle = mxDuplicateArray(prhs(5))


  !-----------------------------------------------------------------------------
  ! Copy x info
  !-----------------------------------------------------------------------------
  x(n+1:n+m) = zero
  call copyMxArrayR( 'x0', n, prhs(6), x(1:n), zero )

  ! Lower and upper bounds on x
  call copyMxArrayR( 'xl', n, prhs(7), bl(1:n), -infBnd )
  call copyMxArrayR( 'xu', n, prhs(8), bu(1:n),  infBnd )

  ! Initial x states
  call copyMxArrayI( 'xstate', n, prhs(9), state(1:n), izero )

  ! Initial multipliers
  call copyMxArrayR( 'xmul', n, prhs(10), y(1:n),  zero )


  !-----------------------------------------------------------------------------
  ! Set the linear constraint matrix and bounds
  !-----------------------------------------------------------------------------
  if (nlCon > 0) then
     call checkCol(prhs(11), n, 'A')
     ldA  = nlCon
     dimx = nlCon*n
     call mxCopyPtrToReal8(mxGetPr(prhs(11)), A(1:nlCon,1:n), dimx)

     i1 = 1+n+nnCon
     i2 = i1-1+nlCon

     ! Lower and upper bounds on linear constraints
     call copyMxArrayR( 'al', nlCon, prhs(12), bl(i1:i2), -infBnd )
     call copyMxArrayR( 'au', nlCon, prhs(13), bu(i1:i2),  infBnd )

     ! States and multipliers of linear constraints
     call copyMxArrayI( 'astate', nlCon, prhs(14), state(i1:i2), izero )
     call copyMxArrayR( 'amul',   nlCon, prhs(15), y(i1:i2), zero )
  else
     ldA = 1
  end if


  !-----------------------------------------------------------------------------
  ! Set the nonlinear Jacobian matrix and bounds
  !-----------------------------------------------------------------------------
  if (nnCon > 0) then
     call checkCol(prhs(21), n, 'J')
     ldJ  = nnCon
     dimx = nnCon*n
     call mxCopyPtrToReal8(mxGetPr(prhs(21)), Jcon(1:nnCon,1:n), dimx)

     i1 = 1+n
     i2 = i1-1+nnCon

     ! Lower and upper bounds on nonlinear constraints
     call copyMxArrayR( 'cl', nnCon, prhs(17), bl(i1:i2), -infBnd )
     call copyMxArrayR( 'cu', nnCon, prhs(18), bu(i1:i2),  infBnd )

     ! States and multipliers of nonlinear constraints
     call copyMxArrayI( 'cstate', nnCon, prhs(19), state(i1:i2), izero )
     call copyMxArrayR( 'cmul',   nnCon, prhs(20), y(i1:i2), zero )
  else
     ldJ = 1
  end if


  !---------------------------------------------------------------------
  ! Set workspace
  !---------------------------------------------------------------------
  iObj     = 0
  ObjAdd   = zero
  nNames   = 1

100 if (.not. memCall) then
     call dnMem &
          (INFO, nlCon, nnCon, n, nnJac, nnObj, iObj, &
           mincw, miniw, minrw, &
           cw, lencw, iw, leniw, rw, lenrw)
     memCall = .true.

     if (leniw .le. miniw) then
        ! Not enough integer space
        leniw = miniw
        allocate(iw0(leniw))
        iw0(1:500) = iw(1:500)

        call move_alloc(from=iw0, to=iw)
     end if

     if (lenrw .le. minrw) then
        ! Not enough real space
        lenrw = minrw
        allocate(rw0(lenrw))
        rw0(1:500) = rw(1:500)

        call move_alloc(from=rw0, to=rw)
     end if

     if (lencw .le. mincw) then
        ! Not enough character space
        lencw = mincw
        allocate(cw0(lencw))
        cw0(1:500) = cw(1:500)

        call move_alloc(from=cw0, to=cw)
     end if

     call dnSetInt &
          ('Total character workspace', lencw, 0, 0, Errors, &
             cw, lencw, iw, leniw, rw, lenrw)
     call dnSetInt &
          ('Total integer   workspace', leniw, 0, 0, Errors, &
             cw, lencw, iw, leniw, rw, lenrw)
     call dnSetInt &
          ('Total real      workspace', lenrw, 0, 0, Errors, &
             cw, lencw, iw, leniw, rw, lenrw)
  end if

  !-----------------------------------------------------------------------------
  ! Solve the problem
  !-----------------------------------------------------------------------------
  nb = n + nlCon + nnCon

  call dnoptKernel                                &
       (start, 'DNOPT   ',                        &
        n, nb, nlCon, nnCon, nnJac, nnObj,        &
        probName, Names, nNames,                  &
        matlabCon, matlabObj,                     &
        matlabObj, dnfunHxNull,                   &
        dnoptInterfaceB,                          &
        dnLog, dnLogQP, matlabSTOP,               &
        state, A, ldA, bl, bu, iObj, objAdd,      &
        fObj, gObj, fCon, Jcon, ldJ, H, ldH,      &
        Obj, nInf, sInf, x, y,                    &
        INFO, mincw, miniw, minrw,                &
        cw, lencw, iw, leniw, rw, lenrw,          &
        cw, lencw, iw, leniw, rw, lenrw)

  if (INFO == 83 .or. INFO == 84 .or. INFO == 85) then
     memCall = .false.
     go to 100
  end if


  !-----------------------------------------------------------------------------
  ! Set output
  !-----------------------------------------------------------------------------
  if (nlhs > 0) then
     dimx = n
     dimy = 1
     plhs(1) = mxCreateDoubleMatrix(dimx, dimy, mxREAL)
     call mxCopyReal8ToPtr(x(1:n), mxGetPr(plhs(1)), dimx)
  end if


  ! Final objective
  if (nlhs > 1) plhs(2) = mxCreateDoubleScalar(Obj)


  ! Exit flag
  rtmp = info
  if (nlhs > 2) plhs(3) = mxCreateDoubleScalar(rtmp)


  ! Iterations
  rtmp = iw(nItn)
  if (nlhs > 3) plhs(4) = mxCreateDoubleScalar(rtmp)


  ! Function evaluations
  rtmp = iw(nfObj1) + iw(nfCon1)
  if (nlhs > 4) plhs(5) = mxCreateDoubleScalar(rtmp)


  ! Multipliers
  if (nlhs > 5) then
     dimx = n+m
     dimy = 1
     plhs(6) = mxCreateDoubleMatrix(dimx, dimy, mxREAL)
     call mxCopyReal8ToPtr(y(1:n+m), mxGetPr(plhs(6)), dimx)
  end if


  ! States
  if (nlhs > 6) then
     dimx = n+m
     dimy = 1
     plhs(7) = mxCreateDoubleMatrix(dimx, dimy, mxREAL)
     !     call mxCopyInteger4ToPtr(state(1:n+m), mxGetPr(plhs(7)), dimx)
     call mxCopyReal8ToPtr(state(1:n+m), mxGetPr(plhs(7)), dimx)
  end if


  ! Deallocate memory
  call deallocDNOPT

end subroutine dnmxSolve

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine dnmxOptions (iOpt, nlhs, plhs, nrhs, prhs)
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
  mwSize           :: dim
  integer*4        :: mxIsChar
  double precision :: mxGetScalar

  character        :: buffer*50, cvalue*8
  integer          :: Errors, ivalue, strlen
  double precision :: rvalue

  integer          :: dnGet
  external         :: dnSet, dnSetInt, dnSetReal, &
                      dnGet, dnGetChar, dnGetInt, dnGetReal


  if (iOpt == dnSetIX .or. iOpt == dnSetRX) then
     if (nrhs /= 3) &
          call mexErrMsgIdAndTxt('DNOPT:InputArgs','Wrong number of input arguments')
  else
     if (nrhs /= 2) &
          call mexErrMsgIdAndTxt('DNOPT:InputArgs','Wrong number of input arguments')
  end if


  ! Get string
  if (mxIsChar(prhs(2)) /= 1) &
       call mexErrMsgIdAndTxt('DNOPT:FileArgs','Need filename string')

  strlen = mxGetN(prhs(2))
  if (strlen > 50) &
       call mexErrMsgIdAndTxt('DNOPT:InputArgs','Option string is too long')

  if (strlen > 0) then
     dim = strlen
     call mxGetString(prhs(2), buffer, dim)
  else
     call mexErrMsgIdAndTxt('DNOPT:InputArgs','Empty option string')
  end if


  if      (iOpt == dnSetXX) then
     call dnSet(buffer, iPrint, iSumm, Errors, &
                 cw, lencw, iw, leniw, rw, lenrw)

  else if (iOpt == dnSetIX) then

     rvalue = mxGetScalar(prhs(3))
     ivalue = rvalue

     call dnSetInt(buffer, ivalue, iPrint, iSumm, Errors, &
                    cw, lencw, iw, leniw, rw, lenrw)

  else if (iOpt == dnSetRX) then

     rvalue = mxGetScalar(prhs(3))

     call dnSetReal(buffer, rvalue, iPrint, iSumm, Errors, &
                     cw, lencw, iw, leniw, rw, lenrw)

  else if (iOpt == dnGetXX) then

     ivalue  = dnGet(buffer, Errors, cw, lencw, iw, leniw, rw, lenrw)

     rvalue  = ivalue
     plhs(1) = mxCreateDoubleScalar(rvalue)

  else if (iOpt == dnGetCX) then

     call dnGetChar(buffer, cvalue, Errors, &
                     cw, lencw, iw, leniw, rw, lenrw)

     plhs(1) = mxCreateString(cvalue)

  else if (iOpt == dnGetIX) then

     call dnGetInt(buffer, ivalue, Errors, &
                    cw, lencw, iw, leniw, rw, lenrw)

     rvalue = ivalue
     plhs(1) = mxCreateDoubleScalar(rvalue)

  else if (iOpt == dnGetRX) then

     call dnGetReal(buffer, rvalue, Errors, &
                     cw, lencw, iw, leniw, rw, lenrw)

     plhs(1) = mxCreateDoubleScalar(rvalue)

  end if

end subroutine dnmxOptions

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine dnmxSpecs(nlhs, plhs, nrhs, prhs)
  use mxdnWork
  implicit none

  integer*4  :: nlhs, nrhs
  mwPointer  :: prhs(*), plhs(*)

  !---------------------------------------------------------------------
  ! Read specs file.
  !---------------------------------------------------------------------
  ! Matlab
  mwPointer        :: mxCreateDoubleScalar, mxGetN
  mwSize           :: dim
  integer*4        :: mxIsChar

  character        :: filename*120
  integer          :: info, strlen
  double precision :: rvalue

  external         :: dnSpec


  if (nrhs /= 2) call mexErrMsgIdAndTxt('DNOPT:InputArgs','Wrong number of input arguments')
  if (nlhs /= 1) call mexErrMsgIdAndTxt('DNOPT:InputArgs','Wrong number of output arguments')

  if (mxIsChar(prhs(2)) /= 1) &
       call mexErrMsgIdAndTxt('DNOPT:FileArgs','Need filename string')

  strlen = mxGetN(prhs(2))
  if (strlen > 120) call mexErrMsgIdAndTxt('DNOPT:FileArgs','Specs filename is too long')

  if (strlen > 0) then
     dim = strlen
     call mxGetString(prhs(2), filename, dim)
  else
     call mexErrMsgIdAndTxt('DNOPT:FileArgs','Empty spc filename')
  end if


  open(iSpecs, file=filename, status='unknown')
  call dnSpec(iSpecs, info, cw, lencw, iw, leniw, rw, lenrw)
  rewind(iSpecs)
  close(iSpecs)

  ! dnSpec will return info == 101 or 107 if successful
  ! The matlab version returns 0 if successful
  if (info == 101 .or. info == 107) then
     rvalue = 0
  else
     rvalue = 1
  end if

  plhs(1) = mxCreateDoubleScalar(rvalue)

end subroutine dnmxSpecs

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine matlabObj(mode, nnObj, x, fObj, gObj, Status, &
                      cu, lencu, iu, leniu, ru, lenru)
  use mxdnWork, only : objHandle, mxREAL
  implicit none

  integer          :: mode, nnObj, Status, lencu, leniu, lenru, &
                      iu(leniu)
  double precision :: fObj, x(nnObj), gObj(nnObj), ru(lenru)
  character*8      :: cu(lencu)

  !---------------------------------------------------------------------
  ! Matlab callback to evaluate objective function and gradient at x.
  !---------------------------------------------------------------------
  mwSize           :: dimx, dimy
  mwPointer        :: mxGetPr, mxDuplicateArray, mxCreateDoubleMatrix, &
                      plhs(2), prhs(2)
  integer*4        :: nlhs, nrhs
  double precision :: mxGetScalar

  nlhs = 2
  nrhs = 2

  ! Setup for matlabObj
  prhs(1) = mxDuplicateArray(objHandle)

  dimx    = nnObj
  dimy    = 1
  prhs(2) = mxCreateDoubleMatrix(dimx, dimy, mxREAL)
  call mxCopyReal8ToPtr(x, mxGetPr(prhs(2)), dimx)


  ! Call Matlab: [f,g] = obj(x)
  call mexCallMatlab(nlhs, plhs, nrhs, prhs, 'dnobj')


  ! Set fObj and gObj
  if (mode == 0 .or. mode == 2) then
     fObj = mxGetScalar(plhs(1))
  end if

  if (mode == 1 .or. mode == 2) then
     fObj = mxGetScalar(plhs(1))
     call mxCopyPtrToReal8(mxGetPr(plhs(2)), gObj, dimx)
  end if

  call mxDestroyArray(plhs(1))
  call mxDestroyArray(plhs(2))
  call mxDestroyArray(prhs(1))
  call mxDestroyArray(prhs(2))

end subroutine matlabObj

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine matlabCon(mode, nnCon, nnJac, x, fCon, Jcon, ldJ, Status, &
                     cu, lencu, iu, leniu, ru, lenru)
  use mxdnWork, only : n, conHandle, mxREAL
  implicit none

  integer          :: mode, nnCon, nnJac, ldJ, Status, &
                      lencu, leniu, lenru, iu(leniu)
  double precision :: fCon(nnCon), x(nnJac), JCon(ldJ,*), ru(lenru)
  character*8      :: cu(lencu)

  !---------------------------------------------------------------------
  ! Matlab callback to evaluate nonlinear constraints and Jacobian at x.
  !---------------------------------------------------------------------
  mwPointer  :: mxGetPr, mxDuplicateArray, mxCreateDoubleMatrix, &
                plhs(2), prhs(2)
  mwSize     :: dimx, dimy, mxGetM, mxGetN
  integer*4  :: mxIsEmpty, nlhs, nrhs

  if (status .ge. 2) return

  if (nnCon > 0) then
     nlhs = 2
     nrhs = 2

     ! Setup input arguments
     prhs(1) = mxDuplicateArray(conHandle)
     dimx      = nnJac
     dimy      = 1
     prhs(2) = mxCreateDoubleMatrix(dimx, dimy, mxREAL)
     call mxCopyReal8ToPtr(x, mxGetPr(prhs(2)), dimx)

     ! dncon/dnconN are wrappers around the user-defined nonlcon:
     !   [c,J]         = nonlcon(x)
     !   [c,ceq,J,Jeq] = nonlcon(x)
     call mexCallMatlab(nlhs, plhs, nrhs, prhs, 'dncon')


     ! Set output
     if (mode == 0 .or. mode == 2) then
        dimx = nnCon
        call mxCopyPtrToReal8(mxGetPr(plhs(1)), fCon(1:nnCon), dimx)
     end if

     if (mode == 1 .or. mode == 2) then
        if (mxIsEmpty(plhs(2)) == 0) then
           dimx = nnCon*n
           call mxCopyPtrToReal8(mxGetPr(plhs(2)), Jcon(1:nnCon,1:n), dimx)
        end if
     end if

     call mxDestroyArray(plhs(1))
     call mxDestroyArray(plhs(2))
     call mxDestroyArray(prhs(1))
     call mxDestroyArray(prhs(2))
  end if


end subroutine matlabCon

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine matlabSTOP &
     (iAbort, &
       KTcond, minimize, mCon0, mCon, &
       maxZ, n, nb, nnCon0, nnCon, nnObj0, nnObj, nZ, &
       itn, nMajor, nMinor, &
       condHz, ObjAdd, fMerit, penParm, step, &
       primalInf, dualInf, maxVi, maxViRel, state, &
       Scales, bl, bu, fObj, gObj, fCon, fmul, &
       JQP, ldJQP, gQ, x, yCon, &
       cu, lencu, iu, leniu, ru, lenru, &
       cw, lencw, iw, leniw, rw, lenrw)
  use mxdnWork, only : stopHandle, mxREAL
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
  integer*4        :: nlhs, nrhs
  mwPointer        :: prhs(2), plhs(1), &
                      mxGetPr, mxCreateDoubleMatrix, mxDuplicateArray
  mwSize           :: dimx, dimy
  double precision :: mxGetScalar
  double precision :: rAbort

  iAbort = 0
  ! If iAbort > 0 on exit, the run is terminated.

  if (stopHandle /= 0) then
     nlhs    = 1
     nrhs    = 2

     prhs(1) = mxDuplicateArray(stopHandle)

     dimx = n
     dimy = 1
     prhs(2) = mxCreateDoubleMatrix(dimx, dimy, mxREAL)
     call mxCopyReal8ToPtr(x(1:n), mxGetPr(prhs(2)), dimx)

     ! Call Matlab: stop = stopFunction(x)
     call mexCallMatlab(nlhs, plhs, nrhs, prhs, 'feval')


     ! Get iAbort siginal
     rAbort = mxGetScalar(plhs(1))
     iAbort = rAbort

  end if

end subroutine matlabSTOP

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
