!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! File dqoptmex.F90
! Mex function for DQOPT
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include "fintrf.h"

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine mexFunction(nlhs, plhs, nrhs, prhs)
  use mxQP
  implicit none

  integer*4  :: nlhs, nrhs
  mwPointer  :: prhs(*), plhs(*)
  !=====================================================================
  ! Mex function for DQOPT
  !
  ! Option      Action
  !    1        Solve the QP
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

  ! DQOPT
  character        :: filename*80
  integer          :: info, iOpt, strlen
  double precision :: rOpt, rleniw, rlenrw
  external         :: dqBegin


  ! Get option.
  if (nrhs < 1) call mexErrMsgIdAndTxt('DQOPT:InputArgs','Need an option input argument')
  rOpt = mxGetScalar(prhs(1))
  iOpt = rOpt

  ! Register exit function
  call mexAtExit(resetDQOPT)

  ! Files
  if (iOpt == dnOpenP) then

     if (nrhs /= 2) call mexErrMsgIdAndTxt('DQOPT:InputArgs','Wrong number of input arguments')

     info = mxIsChar(prhs(2))
     if (info /= 1) call mexErrMsgIdAndTxt('DQOPT:FileArgs','Need filename string')

     strlen = mxGetN(prhs(2))
     if (strlen > 80) call mexErrMsgIdAndTxt('DQOPT:FileArgs','Print filename is too long')

     if (strlen > 0) then
        dim = strlen
        call mxGetString(prhs(2), filename, dim)
     else
        call mexErrMsgIdAndTxt('DQOPT:FileArgs','Empty print filename')
     end if

     if (printOpen) close(iPrint)

     open (iPrint, file=filename, status='unknown')
     printOpen = .true.
     return

  else if (iOpt == dnOpenS) then

     if (nrhs /= 2) call mexErrMsgIdAndTxt('DQOPT:InputArgs','Wrong number of input arguments')

     info = mxIsChar(prhs(2))
     if (info /= 1) call mexErrMsgIdAndTxt('DQOPT:FileArgs','Need filename string')

     strlen = mxGetN(prhs(2))
     if (strlen > 80) call mexErrMsgIdAndTxt('DQOPT:FileArgs','Summary filename is too long')

     if (strlen > 0) then
        dim = strlen
        call mxGetString(prhs(2), filename, dim)
     else
        call mexErrMsgIdAndTxt('DQOPT:FileArgs','Empty summary filename')
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
          call mexErrMsgIdAndTxt('DQOPT:Workspace','Workspace size must be at least 500')
     return
  end if


  ! What calls get to this point:
  !  dqopt, dqSolve, dqGet, dqSet, dqSpecs, dqEnd

  if (firstCall) then
     if (allocated(cw)) deallocate(cw)
     if (allocated(iw)) deallocate(iw)
     if (allocated(rw)) deallocate(rw)
     allocate(cw(lencw), iw(leniw), rw(lenrw))

     callType = userCall
     call dqBegin(iPrint, iSumm, cw, lencw, iw, leniw, rw, lenrw)
     callType = systemCall

     memCall   = .false.
     firstCall = .false.
  end if

  ! Do whatever we need to do.
  if (iOpt == dnSolve) then

     callType = userCall
     call dqmxSolve(nlhs, plhs, nrhs, prhs)
     callType = systemCall

  else if (iOpt == dnSetXX .or. &
           iOpt == dnSetIX .or. &
           iOpt == dnSetRX .or. &
           iOpt == dnGetXX .or. &
           iOpt == dnGetCX .or. &
           iOpt == dnGetIX .or. &
           iOpt == dnGetRX) then

     callType = userCall
     call dqmxOptions(iOpt, nlhs, plhs, nrhs, prhs)
     callType = systemCall

  else if (iOpt == dnSpecs) then

     callType = userCall
     call dqmxSpecs(nlhs, plhs, nrhs, prhs)
     callType = systemCall

  else if (iOpt == dnEnd) then

     call resetDQOPT

  end if

  return

end subroutine mexFunction

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine dqmxSolve(nlhs, plhs, nrhs, prhs)
  use mxQP
  implicit none

  integer*4  :: nlhs, nrhs
  mwPointer  :: prhs(*), plhs(*)
  !---------------------------------------------------------------------
  ! Solve the quadratic problem
  !    [x,fval,exitFlag,itn,y,state] =
  !             dqoptmex(solveOpt, istart, name, H, c, ...
  !                      x0, xl, xu, xstate, xmul, ...
  !                      A,  al, au, astate, amul);
  !---------------------------------------------------------------------
  ! Matlab
  mwPointer        :: mxDuplicateArray, mxGetM, mxGetN, mxGetPr, &
                      mxCreateDoubleMatrix, mxCreateDoubleScalar
  mwSize           :: dimx, dimy
  integer*4        :: mxIsChar, mxIsClass
  double precision :: mxGetScalar

  character*8      :: probName, Names(1)
  integer          :: Errors, i1, i2, info, j
  integer          :: Start, iObj, ldA, ldH, m, &
                      mincw, miniw, minrw, n, nb, ncObj, nInf, &
                      nleq, nlin, nNames, nnH
  double precision :: rtmp, Obj, ObjAdd, sInf
  external         :: dqoptKernel, dqSetInt, dqLog, matlabHx

  integer,          parameter   :: nItn   = 421, izero = 0
  double precision, parameter   :: zero = 0.0d+0, infBnd = 1.0d+20


  ! Check number of input and output arguments.
  if (nrhs /= 15) &
       call mexErrMsgIdAndTxt('DQOPT:InputArgs','Wrong number of input arguments')


  !-----------------------------------------------------------------------------
  ! Start option
  !-----------------------------------------------------------------------------
  start = mxGetScalar(prhs(2))


  !-----------------------------------------------------------------------------
  ! Problem name
  !-----------------------------------------------------------------------------
  if (mxIsChar(prhs(3)) /= 1) &
       call mexErrMsgIdAndTxt('DQOPT:InputArg','Wrong input type for problem name')
  probName = '        '
  dimx = min(8,mxGetN(prhs(3)))
  call mxGetString(prhs(3), probName, dimx)


  !-----------------------------------------------------------------------------
  ! Compute number of variables and linear constraints
  ! Allocate DQOPT arrays
  !-----------------------------------------------------------------------------
  nnH    = mxGetM(prhs(4))
  ncObj  = mxGetM(prhs(5))
  m      = mxGetM(prhs(11))
  n      = max(nnH,ncObj)
  nNames = 1
  if (n == 0) call mexErrMsgIdAndTxt('DQOPT:InputArgs','No objective given')

  call allocDQOPT(n, m, nnH, ncObj)


  !-----------------------------------------------------------------------------
  ! Set Hessian and linear objective term
  !-----------------------------------------------------------------------------
  if (nnH > 0) then
     ldH  = nnH
     dimx = nnH*nnH
     call mxCopyPtrToReal8(mxGetPr(prhs(4)), H(1:nnH,1:nnH), dimx)
  else
     ldH = 1
  end if

  if (ncObj > 0) then
     call copyMxArrayR('cObj', ncObj, prhs(5), cObj(1:ncObj), zero)
  end if


  !-----------------------------------------------------------------------------
  ! Initial x
  !-----------------------------------------------------------------------------
  x(n+1:n+m) = zero
  call copyMxArrayR('x0', n, prhs(6), x(1:n), zero)


  !-----------------------------------------------------------------------------
  ! Set bounds on variables and constraints
  ! Get initial states
  !-----------------------------------------------------------------------------
  call copyMxArrayR('xl',     n, prhs(7), bl(1:n), -infBnd)
  call copyMxArrayR('xu',     n, prhs(8), bu(1:n),  infBnd)
  call copyMxArrayI('xstate', n, prhs(9), state(1:n), izero)
  call copyMxArrayR('xmul',   n, prhs(10), y(1:n), zero)


  !-----------------------------------------------------------------------------
  ! Linear constraints
  !-----------------------------------------------------------------------------
  if (m > 0) then
     call checkCol(prhs(11), n, 'A')
     ldA  = m
     dimx = m*n
     call mxCopyPtrToReal8(mxGetPr(prhs(11)), A(1:m,1:n), dimx)

     i1 = 1+n
     i2 = m+n

     call copyMxArrayR('al',     m, prhs(12), bl(i1:i2), -infBnd)
     call copyMxArrayR('au',     m, prhs(13), bu(i1:i2),  infBnd)
     call copyMxArrayI('astate', m, prhs(14), state(i1:i2), izero)
     call copyMxArrayR('amul',   m, prhs(15), y(i1:i2), zero)
  else
     ldA = 1
  end if


  !-----------------------------------------------------------------------------
  ! Solve the problem
  !-----------------------------------------------------------------------------
  nb     = n + m
  iObj   = 0
  ObjAdd = zero
  Etype(1:n+m) = izero

100 call dqoptKernel                         &
         (start, m, n, nb, nnH, nNames, ldH, &
          ncObj, iObj, objAdd, probName,     &
          A, ldA, bl, bu, cObj, H, Names,    &
          matlabHx, dqLog,                   &
          Etype, state, x, y,                &
          INFO, mincw, miniw, minrw,         &
          Obj, nInf, sInf,                   &
          cw, lencw, iw, leniw, rw, lenrw,   &
          cw, lencw, iw, leniw, rw, lenrw)

  if (INFO == 83) then
     ! Not enough integer space
     leniw = miniw
     allocate(iw0(leniw))
     iw0(1:500) = iw(1:500)

     call move_alloc(from=iw0, to=iw)
     call dqSetInt &
          ('Total integer   workspace', leniw, 0, 0, Errors, &
             cw, lencw, iw, leniw, rw, lenrw)
     go to 100
  end if

  if (INFO == 84) then
    ! Not enough real space
     lenrw = minrw
     allocate(rw0(lenrw))
     rw0(1:500) = rw(1:500)

     call move_alloc(from=rw0, to=rw)
     call dqSetInt &
          ('Total real      workspace', lenrw, 0, 0, Errors, &
            cw, lencw, iw, leniw, rw, lenrw)
     go to 100
  end if


  !-----------------------------------------------------------------------------
  ! Set output
  !-----------------------------------------------------------------------------
  if (nlhs > 0) then
     dimx  = n
     dimy  = 1
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

  ! Multipliers
  if (nlhs > 4) then
     dimx = n+m
     dimy = 1
     plhs(5) = mxCreateDoubleMatrix(dimx, dimy, mxREAL)
     call mxCopyReal8ToPtr(y(1:n+m), mxGetPr(plhs(5)), dimx)
  end if

  ! States
  if (nlhs > 5) then
     dimx = n+m
     dimy = 1
     plhs(6) = mxCreateDoubleMatrix(dimx, dimy, mxREAL)
     !call mxCopyInteger4ToPtr(state(1:n+m), mxGetPr(plhs(6)), dimx)
     call mxCopyReal8ToPtr(dble(state(1:n+m)), mxGetPr(plhs(6)), dimx)
  end if

  ! Deallocate memory
  call deallocDQOPT

end subroutine dqmxSolve

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine dqmxOptions(iOpt, nlhs, plhs, nrhs, prhs)
  use mxQP
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

  integer          :: dqGet
  external         :: dqSet, dqSetInt, dqSetReal, &
                      dqGet, dqGetChar, dqGetInt, dqGetReal


  if (iOpt == dnSetIX .or. iOpt == dnSetRX) then
     if (nrhs /= 3) call mexErrMsgIdAndTxt('Wrong number of input arguments')
  else
     if (nrhs /= 2) call mexErrMsgIdAndTxt('Wrong number of input arguments')
  end if


  ! Get string
  if (mxIsChar(prhs(2)) /= 1) call mexErrMsgIdAndTxt('Need filename string')

  strlen = mxGetN(prhs(2))
  if (strlen > 50) call mexErrMsgIdAndTxt('Option string is too long')

  if (strlen > 0) then
     dim = strlen
     call mxGetString(prhs(2), buffer, dim)
  else
     call mexErrMsgIdAndTxt('Empty option string')
  end if


  if      (iOpt == dnSetXX) then
     call dqSet(buffer, iPrint, iSumm, Errors, &
                 cw, lencw, iw, leniw, rw, lenrw)

  else if (iOpt == dnSetIX) then

     rvalue = mxGetScalar(prhs(3))
     ivalue = rvalue

     call dqSetInt(buffer, ivalue, iPrint, iSumm, Errors, &
                    cw, lencw, iw, leniw, rw, lenrw)

  else if (iOpt == dnSetRX) then

     rvalue = mxGetScalar(prhs(3))

     call dqSetReal(buffer, rvalue, iPrint, iSumm, Errors, &
                     cw, lencw, iw, leniw, rw, lenrw)

  else if (iOpt == dnGetXX) then

     ivalue  = dqGet(buffer, Errors, cw, lencw, iw, leniw, rw, lenrw)

     rvalue  = ivalue
     plhs(1) = mxCreateDoubleScalar(rvalue)

  else if (iOpt == dnGetCX) then

     call dqGetChar(buffer, cvalue, Errors, &
                     cw, lencw, iw, leniw, rw, lenrw)

     plhs(1) = mxCreateString(cvalue)

  else if (iOpt == dnGetIX) then

     call dqGetInt(buffer, ivalue, Errors, &
                    cw, lencw, iw, leniw, rw, lenrw)

     rvalue = ivalue
     plhs(1) = mxCreateDoubleScalar(rvalue)

  else if (iOpt == dnGetRX) then

     call dqGetReal(buffer, rvalue, Errors, &
                     cw, lencw, iw, leniw, rw, lenrw)

     plhs(1) = mxCreateDoubleScalar(rvalue)

  end if

end subroutine dqmxOptions

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine dqmxSpecs(nlhs, plhs, nrhs, prhs)
  use mxQP
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

  external         :: dqSpec


  if (nrhs /= 2) call mexErrMsgIdAndTxt('Wrong number of input arguments')
  if (nlhs /= 1) call mexErrMsgIdAndTxt('Wrong number of output arguments')


  if (mxIsChar(prhs(2)) /= 1) call mexErrMsgIdAndTxt('Need filename string')

  strlen = mxGetN(prhs(2))
  if (strlen > 120) call mexErrMsgIdAndTxt('Specs filename is too long')

  if (strlen > 0) then
     dim = strlen
     call mxGetString(prhs(2), filename, dim)
  else
     call mexErrMsgIdAndTxt('Empty spc filename')
  end if


  open(iSpecs, file=filename, status='unknown')
  call dqSpec(iSpecs, info, cw, lencw, iw, leniw, rw, lenrw)
  rewind(iSpecs)
  close(iSpecs)

  ! dqSpec will return info == 101 or 107 if successful
  ! The matlab version returns 0 if successful
  if (info == 101 .or. info == 107) then
     rvalue = 0
  else
     rvalue = 1
  end if

  plhs(1) = mxCreateDoubleScalar(rvalue)

end subroutine dqmxSpecs

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine matlabHx(nnH, H, ldH, x, Hx, qpState, &
                    cu, lencu, iu, leniu, ru, lenru)
  implicit none
  integer          :: ldH, nnH, qpState, lencu, leniu, lenru, iu(leniu)
  double precision :: H(ldH,*), x(nnH), Hx(nnH), ru(lenru)
  character*8      :: cu(lencu)

  !---------------------------------------------------------------------
  ! Callback to compute H*x for a given x.
  !---------------------------------------------------------------------
  integer          :: i, j
  double precision :: sum
  double precision, parameter :: zero = 0.0d+0

  if (qpState == 1) then
     ! First call.
  end if

  if (nnH <= 0) return

  Hx(1:nnH) = zero

  do i = 1, nnH
     sum = zero
     do j = 1, nnH
        sum = sum + H(i,j)*x(j)
     end do
     Hx(i) = sum
  end do

  if (qpState >= 2) then
     ! Last call.
  end if

end subroutine matlabHx

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
