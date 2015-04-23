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


  ! DNOPT mex variables
  logical            :: firstCall = .true.,  &
                        printOpen = .false., &
                        summOpen  = .false., &
                        screenON  = .false.

  integer            :: callType = 0

  mwPointer          :: objHandle, conHandle, stopHandle
  integer            :: n, nneq, nnin

  integer, parameter :: iPrint     = 9, iSpecs   = 4, iSumm    = 55, &
                        systemCall = 0, userCall = 1
  integer, parameter :: dnSolve    = 1,  &
                        dnSetX     = 2,  &
                        dnSetI     = 3,  &
                        dnSetR     = 4,  &
                        dnGetX     = 5,  &
                        dnGetC     = 6,  &
                        dnGetI     = 7,  &
                        dnGetR     = 8,  &
                        dnSpecs    = 9,  &
                        dnOpenP    = 10, &
                        dnOpenS    = 11, &
                        dnClosP    = 12, &
                        dnClosS    = 13, &
                        dnscrnON   = 15, &
                        dnscrnOff  = 16, &
                        dnSolveN   = 17, &
                        dnEnd      = 999

  integer, parameter :: dqSolve    = 1

contains

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine checkCol ( pm, n, name )
    mwPointer     :: pm
    integer       :: n
    character*(*) :: name
    !---------------------------------------------------------------------------
    ! Check column dimension of pm is equal to n.
    !---------------------------------------------------------------------------
    character*80 :: str
    mwSize       :: m, mxGetN

    m = mxGetN(pm)
    if ( m /= n ) then
       write(str,100) name, m, n
       call mexErrMsgTxt ( str )
    end if

    return

100 format ( a, ' has incorrect column dimension ', i5, &
                '.  Should be length ', i5 )

  end subroutine checkCol

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine checkRow ( pm, n, name )
    character*(*) :: name
    mwPointer     :: pm
    integer       :: n
    !---------------------------------------------------------------------------
    ! Check row dimension of pm is equal to n.
    !---------------------------------------------------------------------------
    character*80 :: str
    mwSize       :: m, mxGetM

    m = mxGetM(pm)
    if ( m /= n ) then
       write(str,100) name, m, n
       call mexErrMsgTxt ( str )
    end if

    return

100 format ( a, ' has incorrect row dimension ', i5, &
                '.  Should be length ', i5 )

  end subroutine checkRow

end module mxdnWork
