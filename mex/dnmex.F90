!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! mxdnWork module for DNOPT Fortran mex
!
! 13 Sep 2013: First version.
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include "fintrf.h"

module mxdnWork

  ! DNOPT workspace
  integer           :: leniw = 5000, lenrw = 5000, lencw = 500
  integer,          allocatable :: iw(:), iw0(:)
  double precision, allocatable :: rw(:), rw0(:)
  character*8,      allocatable :: cw(:), cw0(:)

  ! Matlab
  integer*4, parameter :: mxREAL = 0

  ! For mex callback functions
  mwPointer          :: objHandle, conHandle, stopHandle
  integer            :: n

  ! DNOPT mex variables
  logical            :: firstCall = .true.,  &
                        memCall   = .false., &
                        printOpen = .false., &
                        summOpen  = .false., &
                        screenON  = .false.

  integer            :: callType = 0

  integer, parameter :: iPrint     = 9, iSpecs   = 4, iSumm    = 55, &
                        systemCall = 0, userCall = 1
  integer, parameter :: dnSolve    = 1,  &
                        dnSetXX    = 2,  &
                        dnSetIX    = 3,  &
                        dnSetRX    = 4,  &
                        dnGetXX    = 5,  &
                        dnGetCX    = 6,  &
                        dnGetIX    = 7,  &
                        dnGetRX    = 8,  &
                        dnSpecs    = 9,  &
                        dnOpenP    = 10, &
                        dnOpenS    = 11, &
                        dnClosP    = 12, &
                        dnClosS    = 13, &
                        dnSetWork  = 14, &
                        dnscrnON   = 15, &
                        dnscrnOff  = 16, &
                        dnEnd      = 999
  ! DNOPT arrays
  integer,          allocatable :: state(:)
  double precision, allocatable :: x(:), bl(:), bu(:), y(:), &
                                   H(:,:), A(:,:), Jcon(:,:), &
                                   fCon(:), gObj(:)

  ! DQOPT arrays
  integer,          allocatable :: Etype(:)
  double precision, allocatable :: cObj(:)

  public  :: resetDNOPT, allocDNOPT, deallocDNOPT, &
             allocDQOPT, deallocDQOPT, &
             checkCol, checkRow
  private :: deallocI, deallocR, deallocR2

contains

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine resetDNOPT()
    !---------------------------------------------------------------------------
    ! resetDNOPT for new problem.
    ! Reset variables, deallocate all arrays.
    ! (Also registered with mexAtExit to deallocate workspace and close files)
    !---------------------------------------------------------------------------

    !    if (printOpen) close(iPrint)
    close(iPrint)
    printOpen = .false.

    !    if (summOpen) close(iSumm)
    close(iSumm)
    summOpen  = .false.

    close(iSpecs)

    firstCall = .true.
    memCall   = .false.

    leniw     = 5000
    lenrw     = 5000
    lencw     = 500

    call deallocDQOPT()
    call deallocDNOPT()

    if (allocated(cw)) deallocate(cw)
    if (allocated(iw)) deallocate(iw)
    if (allocated(rw)) deallocate(rw)

    if (allocated(cw0)) deallocate(cw0)
    if (allocated(iw0)) deallocate(iw0)
    if (allocated(rw0)) deallocate(rw0)

  end subroutine resetDNOPT

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine allocDNOPT(n, m, nlCon, nnCon)
    integer, intent(in) :: n, m, nlCon, nnCon
    !---------------------------------------------------------------------------
    ! Allocate space for DNOPT solve.
    !---------------------------------------------------------------------------

    call deallocDNOPT()

    allocate(state(n+m))
    allocate(bl(n+m), bu(n+m), x(n+m), y(n+m))
    allocate(gObj(n))
    allocate(H(n,n))

    if (nlCon > 0) then
       allocate(A(nlCon,n))
    else
       allocate(A(1,n))
    end if

    if (nnCon > 0) then
       allocate(Jcon(nnCon,n))
       allocate(fCon(nnCon))
    else
       allocate(Jcon(1,1))
       allocate(fCon(1))
    end if

  end subroutine allocDNOPT

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine deallocDNOPT()
    !---------------------------------------------------------------------------
    ! Deallocate x,F arrays involved in solve routine.
    !---------------------------------------------------------------------------
    if (objHandle  /= 0) call mxDestroyArray(objHandle)
    if (conHandle  /= 0) call mxDestroyArray(conHandle)
    if (stopHandle /= 0) call mxDestroyArray(stopHandle)

    objHandle  = 0
    conHandle  = 0
    stopHandle = 0

    call deallocI(state)

    call deallocR(bl)
    call deallocR(bu)
    call deallocR(x)
    call deallocR(y)

    call deallocR2(A)
    call deallocR2(Jcon)

    call deallocR2(H)
    call deallocR(fCon)
    call deallocR(gObj)

  end subroutine deallocDNOPT

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine allocDQOPT(n, m, nnH, ncObj)
    integer, intent(in) :: n, m, nnH, ncObj
    !---------------------------------------------------------------------------
    ! Allocate space for DQOPT solve.
    !---------------------------------------------------------------------------

    call deallocDQOPT()

    allocate(state(n+m),Etype(n+m))
    allocate(bl(n+m), bu(n+m), x(n+m), y(n+m))

    if (nnH > 0) then
       allocate(H(nnH,nnH))
    else
       allocate(H(1,1))
    end if

    if (ncObj > 0) then
       allocate(cObj(ncObj))
    else
       allocate(cObj(1))
    end if

    if (m > 0) then
       allocate(A(m,n))
    else
       allocate(A(1,n))
    end if

  end subroutine allocDQOPT

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine deallocDQOPT()
    !---------------------------------------------------------------------------
    ! Deallocate arrays involved in solve routine.
    !---------------------------------------------------------------------------

    call deallocR(x)
    call deallocR(y)
    call deallocR(bl)
    call deallocR(bu)
    call deallocR(cObj)

    call deallocI(state)
    call deallocI(Etype)

    call deallocR2(H)
    call deallocR2(A)

  end subroutine deallocDQOPT

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine deallocI(array)
    integer, allocatable :: array(:)

    if (allocated(array)) deallocate(array)

  end subroutine deallocI

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine deallocR(array)
    double precision, allocatable :: array(:)

    if (allocated(array)) deallocate(array)

  end subroutine deallocR

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine deallocR2(array)
    double precision, allocatable :: array(:,:)

    if (allocated(array)) deallocate(array)

  end subroutine deallocR2

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine checkCol (pm, n, name)
    mwPointer     :: pm
    integer       :: n
    character*(*) :: name
    !---------------------------------------------------------------------------
    ! Check column dimension of pm is equal to n.
    !---------------------------------------------------------------------------
    character*80 :: str
    mwSize       :: m, mxGetN

    m = mxGetN(pm)
    if (m /= n) then
       write(str,100) name, m, n
       call mexErrMsgTxt (str)
    end if

    return

100 format (a, ' has incorrect column dimension ', i5, &
                '.  Should be length ', i5)

  end subroutine checkCol

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine checkRow (pm, n, name)
    character*(*) :: name
    mwPointer     :: pm
    integer       :: n
    !---------------------------------------------------------------------------
    ! Check row dimension of pm is equal to n.
    !---------------------------------------------------------------------------
    character*80 :: str
    mwSize       :: m, mxGetM

    m = mxGetM(pm)
    if (m /= n) then
       write(str,100) name, m, n
       call mexErrMsgTxt (str)
    end if

    return

100 format (a, ' has incorrect row dimension ', i5, &
                '.  Should be length ', i5)

  end subroutine checkRow

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine copyMxArrayR(name, n, mxarray, array, defval)
    character*(*)    :: name
    integer          :: n
    mwPointer        :: mxarray
    double precision :: defval, array(n)
    !---------------------------------------------------------------------
    ! Check if matlab array is empty.
    ! If not empty, copy to real array.
    ! Else, set array to default value.
    !---------------------------------------------------------------------
    mwPointer :: mxGetPr
    mwSize    :: dim
    integer*4 :: mxIsEmpty

    if (mxIsEmpty(mxarray) > 0) then
       array(1:n) = defval
    else
       call checkRow(mxarray, n, name)
       call checkCol(mxarray, 1, name)

       dim = n
       call mxCopyPtrToReal8(mxGetPr(mxarray), array(1:n), dim)
    end if

  end subroutine copyMxArrayR

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine copyMxArrayI(name, n, mxarray, array, defval)
    character*(*)    :: name
    integer          :: n, defval, array(n)
    mwPointer        :: mxarray
    !---------------------------------------------------------------------
    ! Check if matlab array is empty.
    ! If not empty, copy to real array.
    ! Else, set array to default value.
    !---------------------------------------------------------------------
    mwPointer :: mxGetPr
    mwSize    :: dim
    integer*4 :: mxIsEmpty

    if (mxIsEmpty(mxarray) > 0) then
       array(1:n) = defval
    else
       call checkRow(mxarray, n, name)
       call checkCol(mxarray, 1, name)

       dim = n
       call mxCopyPtrToInteger4(mxGetPr(mxarray), array(1:n), dim)
    end if

  end subroutine copyMxArrayI

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
end module mxdnWork
