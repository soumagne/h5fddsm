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
MODULE H5FDDSM_GLOBAL
  !
  ! H5FD DSM flags declaration
  !
  INTEGER, PARAMETER :: H5FD_DSM_FLAGS_LEN = 6
  INTEGER H5FD_dsm_flags(H5FD_DSM_FLAGS_LEN)
  !DEC$if defined(BUILD_H5FD_DSM_DLL)
  !DEC$ATTRIBUTES DLLEXPORT :: /H5FD_DSM_FLAGS/
  !DEC$endif
  COMMON /H5FD_DSM_FLAGS/ H5FD_dsm_flags

  INTEGER :: H5FD_DSM_MANUAL_SERVER_UPDATE_F
  INTEGER :: H5FD_DSM_UPDATE_LEVEL_0_F
  INTEGER :: H5FD_DSM_UPDATE_LEVEL_1_F
  INTEGER :: H5FD_DSM_UPDATE_LEVEL_2_F
  INTEGER :: H5FD_DSM_UPDATE_LEVEL_3_F
  INTEGER :: H5FD_DSM_UPDATE_LEVEL_4_F

  EQUIVALENCE(H5FD_dsm_flags(1), H5FD_DSM_MANUAL_SERVER_UPDATE_F)
  EQUIVALENCE(H5FD_dsm_flags(2), H5FD_DSM_UPDATE_LEVEL_0_F)
  EQUIVALENCE(H5FD_dsm_flags(3), H5FD_DSM_UPDATE_LEVEL_1_F)
  EQUIVALENCE(H5FD_dsm_flags(4), H5FD_DSM_UPDATE_LEVEL_2_F)
  EQUIVALENCE(H5FD_dsm_flags(5), H5FD_DSM_UPDATE_LEVEL_3_F)
  EQUIVALENCE(H5FD_dsm_flags(6), H5FD_DSM_UPDATE_LEVEL_4_F)

END MODULE H5FDDSM_GLOBAL
