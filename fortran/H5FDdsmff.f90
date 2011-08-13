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
  USE H5FD_DSM_GLOBAL
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
! Purpose:     Returns MPI communicator information
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
! Name:     h5fd_dsm_set_options_f
!
! Purpose:  Set a specific option to the DSM
!
! Inputs:
!       options     - options are:
!                       - H5FD_DSM_DONT_RELEASE_F
!                       - H5FD_DSM_DONT_NOTIFY_F
! Outputs:
!       hdferr:     - error code
!                   Success:  0
!                   Failure: -1
! Optional parameters:
!               NONE
!
!----------------------------------------------------------------------
  SUBROUTINE h5fd_dsm_set_options_f(options, hdferr)
    !DEC$if defined(BUILD_H5FD_DSM_DLL)
    !DEC$ ATTRIBUTES DLLEXPORT :: h5fd_dsm_set_options_f
    !DEC$endif
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: options      ! DSM options to set
    INTEGER, INTENT(OUT) :: hdferr       ! Error code
    INTEGER :: err_0, err_1

    INTERFACE
       INTEGER FUNCTION h5fd_dsm_set_options_c(options)
         USE H5GLOBAL
         !DEC$IF DEFINED(H5FD_DSM_F90_WIN32)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FD_DSM_SET_OPTIONS_C'::h5fd_dsm_set_options_c
         !DEC$ENDIF
       INTEGER, INTENT(IN)  :: options
       END FUNCTION h5fd_dsm_set_options_c
    END INTERFACE

    INTERFACE
      INTEGER FUNCTION h5fd_dsm_init_flags_c(i_H5FD_dsm_flags)
        USE H5FD_DSM_GLOBAL
        INTEGER i_H5FD_dsm_flags(H5FD_DSM_FLAGS_LEN)
      END FUNCTION h5fd_dsm_init_flags_c
    END INTERFACE

    err_0 = h5fd_dsm_init_flags_c(H5FD_dsm_flags)
    err_1 = h5fd_dsm_set_options_c(options)
    hdferr = err_0 + err_1
  END SUBROUTINE h5fd_dsm_set_options_f

!----------------------------------------------------------------------
! Name:     h5fd_dsm_notify_f
!
! Purpose:  Send a notification to the DSM host
!
! Inputs:
!       notifications   - notifications are:
!                           - H5FD_DSM_NEW_DATA_F
!                           - H5FD_DSM_NEW_INFORMATION_F
! Outputs:
!       hdferr:     - error code
!                   Success:  0
!                   Failure: -1
! Optional parameters:
!               NONE
!
!----------------------------------------------------------------------
  SUBROUTINE h5fd_dsm_notify_f(notifications, hdferr)
    !DEC$if defined(BUILD_H5FD_DSM_DLL)
    !DEC$ ATTRIBUTES DLLEXPORT :: h5fd_dsm_notify_f
    !DEC$endif
    IMPLICIT NONE
    INTEGER, INTENT(IN)  :: notifications ! DSM notifications to be sent
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
    INTEGER :: err_0, err_1

    INTERFACE
       INTEGER FUNCTION h5fd_dsm_notify_c(notifications)
         USE H5GLOBAL
         !DEC$IF DEFINED(H5FD_DSM_F90_WIN32)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FD_DSM_NOTIFY_C'::h5fd_dsm_notify_c
         !DEC$ENDIF
       INTEGER, INTENT(IN)  :: notifications
       END FUNCTION h5fd_dsm_notify_c
    END INTERFACE

    INTERFACE
      INTEGER FUNCTION h5fd_dsm_init_flags_c(i_H5FD_dsm_flags)
        USE H5FD_DSM_GLOBAL
        INTEGER i_H5FD_dsm_flags(H5FD_DSM_FLAGS_LEN)
      END FUNCTION h5fd_dsm_init_flags_c
    END INTERFACE

    err_0 = h5fd_dsm_init_flags_c(H5FD_dsm_flags)
    err_1 = h5fd_dsm_notify_c(notifications)
    hdferr = err_0 + err_1
  END SUBROUTINE h5fd_dsm_notify_f

END MODULE H5FDDSM
