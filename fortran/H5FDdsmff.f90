! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! Project                 : H5FDdsm
! Module                  : H5FDdsmff.f90
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
! This file contains Fortran90 interfaces for H5FDdsm functions.
!
MODULE H5FDDSM
  USE H5FDDSM_GLOBAL
  USE H5GLOBAL
  CONTAINS

!----------------------------------------------------------------------
! Name:        h5pset_fapl_dsm_f
!
! Purpose:     Set the File access property for DSM VFD usage
!
! Inputs:
!        prp_id        - file access property list identifier
!        comm        - Communicator used by the IO nodes
! Outputs:
!        hdferr:        - error code
!                     Success:  0
!                     Failure: -1
! Optional parameters:
!                NONE
!
!----------------------------------------------------------------------
  SUBROUTINE h5pset_fapl_dsm_f(prp_id, comm, hdferr)
    !DEC$if defined(BUILD_H5FD_DSM_DLL)
    !DEC$ ATTRIBUTES DLLEXPORT :: h5pset_fapl_dsm_f
    !DEC$endif
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
    INTEGER, INTENT(IN)  :: comm         ! Default communicator
    INTEGER, INTENT(OUT) :: hdferr       ! Error code

    INTERFACE
       INTEGER FUNCTION h5pset_fapl_dsm_c(prp_id, comm)
         USE H5GLOBAL
         !DEC$IF DEFINED(H5FD_DSM_F90_WIN32)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FAPL_DSM_C'::h5pset_fapl_dsm_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(IN) :: comm
       END FUNCTION h5pset_fapl_dsm_c
    END INTERFACE

    hdferr = h5pset_fapl_dsm_c(prp_id, comm)
  END SUBROUTINE h5pset_fapl_dsm_f

!----------------------------------------------------------------------
! Name:        h5pget_fapl_dsm_f
!
! Purpose:     Returns MPI communicator information.
!
! Inputs:
!        prp_id        - file access property list identifier
! Outputs:
!        comm        - Communicator used by the IO nodes
!        hdferr:        - error code
!                     Success:  0
!                     Failure: -1
! Optional parameters:
!                NONE
!
!----------------------------------------------------------------------
  SUBROUTINE h5pget_fapl_dsm_f(prp_id, comm, hdferr)
    !DEC$if defined(BUILD_H5FD_DSM_DLL)
    !DEC$ ATTRIBUTES DLLEXPORT :: h5pget_fapl_dsm_f
    !DEC$endif
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
    INTEGER, INTENT(OUT) :: comm ! buffer to return communicator
    INTEGER, INTENT(OUT) :: hdferr  ! Error code

    INTERFACE
       INTEGER FUNCTION h5pget_fapl_dsm_c(prp_id, comm)
         USE H5GLOBAL
         !DEC$IF DEFINED(H5FD_DSM_F90_WIN32)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_FAPL_DSM_C'::h5pget_fapl_dsm_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER, INTENT(OUT) :: comm
       END FUNCTION h5pget_fapl_dsm_c
    END INTERFACE

    hdferr = h5pget_fapl_dsm_c(prp_id, comm)
  END SUBROUTINE h5pget_fapl_dsm_f

!----------------------------------------------------------------------
! Name:     h5fd_dsm_set_mode_f
!
! Purpose:  Set the DSM operating mode
!
! Inputs:
!       mode        - specific modes are:
!                       - H5FD_DSM_MANUAL_SERVER_UPDATE_F
! Outputs:
!       hdferr:     - error code
!                   Success:  0
!                   Failure: -1
! Optional parameters:
!               NONE
!
!----------------------------------------------------------------------
  SUBROUTINE h5fd_dsm_set_mode_f(mode, hdferr)
    !DEC$if defined(BUILD_H5FD_DSM_DLL)
    !DEC$ ATTRIBUTES DLLEXPORT :: h5fd_dsm_set_mode_f
    !DEC$endif
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: mode         ! DSM mode to use
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
    INTEGER :: err_0, err_1

    INTERFACE
       INTEGER FUNCTION h5fd_dsm_set_mode_c(mode)
         USE H5GLOBAL
         !DEC$IF DEFINED(H5FD_DSM_F90_WIN32)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FD_DSM_SET_MODE_C'::h5fd_dsm_set_mode_c
         !DEC$ENDIF
       INTEGER, INTENT(IN)  :: mode
       END FUNCTION h5fd_dsm_set_mode_c
    END INTERFACE

    INTERFACE
      INTEGER FUNCTION h5fd_dsm_init_flags_c(i_H5FD_dsm_flags)
        USE H5FDDSM_GLOBAL
        INTEGER i_H5FD_dsm_flags(H5FD_DSM_FLAGS_LEN)
      END FUNCTION h5fd_dsm_init_flags_c
    END INTERFACE

    err_0 = h5fd_dsm_init_flags_c(H5FD_dsm_flags)
    err_1 = h5fd_dsm_set_mode_c(mode)
    hdferr = err_0 + err_1
  END SUBROUTINE h5fd_dsm_set_mode_f

!----------------------------------------------------------------------
! Name:     h5fd_dsm_server_update_f
!
! Purpose:  Force the DSM server to be updated
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
  SUBROUTINE h5fd_dsm_server_update_f(hdferr)
    !DEC$if defined(BUILD_H5FD_DSM_DLL)
    !DEC$ ATTRIBUTES DLLEXPORT :: h5fd_dsm_server_update_f
    !DEC$endif
    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: hdferr       ! Error code

    INTERFACE
       INTEGER FUNCTION h5fd_dsm_server_update_c()
         USE H5GLOBAL
         !DEC$IF DEFINED(H5FD_DSM_F90_WIN32)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FD_DSM_SERVER_UPDATE_C'::h5fd_dsm_server_update_c
         !DEC$ENDIF
       END FUNCTION h5fd_dsm_server_update_c
    END INTERFACE

    hdferr = h5fd_dsm_server_update_c()
  END SUBROUTINE h5fd_dsm_server_update_f

END MODULE H5FDDSM
