! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! Project                 : H5FDdsm
! Module                  : H5FDdsmToolsff.f90
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
!
! This file contains Fortran90 interfaces for H5FDdsmTools functions.
!
MODULE H5FDDSM_TOOLS
  USE H5FD_DSM_GLOBAL
  USE H5GLOBAL

  CONTAINS
         
!----------------------------------------------------------------------
! Name:     h5fd_dsm_dump_f
!
! Purpose:  Display the content of the DSM (Debug only)
!
! Inputs:
!               NONE
! Outputs:
!       hdferr:     - error code
!                   Success:  0
!                   Failure: -1
! Optional parameters:
!               NONE
!
!----------------------------------------------------------------------
  SUBROUTINE h5fd_dsm_dump_f(hdferr)
    !DEC$if defined(BUILD_H5FDdsm_DLL)
    !DEC$ ATTRIBUTES DLLEXPORT :: h5fd_dsm_dump_f
    !DEC$endif
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: hdferr       ! Error code

    INTEGER, EXTERNAL :: h5fd_dsm_dump_c
    hdferr = h5fd_dsm_dump_c()
  END SUBROUTINE h5fd_dsm_dump_f

END MODULE H5FDDSM_TOOLS
