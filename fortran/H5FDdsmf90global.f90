! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! Project                 : H5FDdsm
! Module                  : H5FDdsmf90global.f90
!
! Authors:
!    John Biddiscombe     Jerome Soumagne
!    biddisco@cscs.ch     soumagne@cscs.ch
!
! Copyright (C) CSCS - Swiss National Supercomputing Centre.
! You may use modify and and distribute this code freely providing
! 1) This copyright notice appears on all copies of source code
! 2) An acknowledgment appears with any substantial usage of the code
! 3) If this code is contributed to any other open source project, it
! must not be reformatted such that the indentation, bracketing or
! overall style is modified significantly.
!
! This software is distributed WITHOUT ANY WARRANTY; without even the
! implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!
! This work has received funding from the European Community's Seventh
! Framework Programme (FP7/2007-2013) under grant agreement 225967 “NextMuSE”
!
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
MODULE H5FD_DSM_GLOBAL
  !
  ! H5FD DSM flags declaration
  !
  INTEGER, PARAMETER :: H5FD_DSM_FLAGS_LEN = 8
  INTEGER H5FD_dsm_flags(H5FD_DSM_FLAGS_LEN)
  !DEC$if defined(BUILD_H5FDdsm_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: /H5FD_DSM_FLAGS/
  !DEC$endif
  COMMON /H5FD_DSM_FLAGS/ H5FD_dsm_flags

  INTEGER :: H5FD_DSM_LOCK_SYNCHRONOUS_F
  INTEGER :: H5FD_DSM_LOCK_ASYNCHRONOUS_F
  INTEGER :: H5FD_DSM_MODE_SERIAL_F
  INTEGER :: H5FD_DSM_MODE_PARALLEL_F
  INTEGER :: H5FD_DSM_NOTIFY_NONE_F
  INTEGER :: H5FD_DSM_NOTIFY_DATA_F
  INTEGER :: H5FD_DSM_NOTIFY_INFORMATION_F
  INTEGER :: H5FD_DSM_NOTIFY_USER_F

  EQUIVALENCE(H5FD_dsm_flags(1), H5FD_DSM_LOCK_SYNCHRONOUS_F)
  EQUIVALENCE(H5FD_dsm_flags(2), H5FD_DSM_LOCK_ASYNCHRONOUS_F)
  EQUIVALENCE(H5FD_dsm_flags(3), H5FD_DSM_MODE_SERIAL_F)
  EQUIVALENCE(H5FD_dsm_flags(4), H5FD_DSM_MODE_PARALLEL_F)
  EQUIVALENCE(H5FD_dsm_flags(5), H5FD_DSM_NOTIFY_NONE_F)
  EQUIVALENCE(H5FD_dsm_flags(6), H5FD_DSM_NOTIFY_DATA_F)
  EQUIVALENCE(H5FD_dsm_flags(7), H5FD_DSM_NOTIFY_INFORMATION_F)
  EQUIVALENCE(H5FD_dsm_flags(8), H5FD_DSM_NOTIFY_USER_F)

END MODULE H5FD_DSM_GLOBAL
