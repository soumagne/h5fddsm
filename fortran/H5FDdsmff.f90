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
         USE H5GLOBAL
         
         IMPLICIT NONE
         
         INTEGER(HID_T)  :: DSM_FileHandle = 0
       
         CONTAINS

!----------------------------------------------------------------------
! Name:		h5pset_fapl_dsm_f
!
! Purpose: 	Set the File access property for DSM VFD usage
!
! Inputs:
!		prp_id		- file access property list identifier
!		comm        - Communicator used by the IO nodes
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
!----------------------------------------------------------------------
         SUBROUTINE h5pset_fapl_dsm_f(prp_id, comm, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
            INTEGER, INTENT(IN)  :: comm         ! Default communicator
            INTEGER, INTENT(OUT) :: hdferr       ! Error code

            INTEGER, EXTERNAL :: h5pset_fapl_dsm_c
            hdferr = h5pset_fapl_dsm_c(prp_id, comm)
          END SUBROUTINE h5pset_fapl_dsm_f

!----------------------------------------------------------------------
! Name:		h5pget_fapl_dsm_f
!
! Purpose: 	Returns MPI communicator information.
!
! Inputs:
!		prp_id		- file access property list identifier
! Outputs:
!		comm		- Communicator used by the IO nodes
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
!----------------------------------------------------------------------
         SUBROUTINE h5pget_fapl_dsm_f(prp_id, comm, hdferr)
           IMPLICIT NONE
           INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
           INTEGER, INTENT(OUT) :: comm ! buffer to return communicator
           INTEGER, INTENT(OUT) :: hdferr  ! Error code

           INTEGER, EXTERNAL :: h5pget_fapl_dsm_c
           hdferr = h5pget_fapl_dsm_c(prp_id, comm)
         END SUBROUTINE h5pget_fapl_dsm_f

    END MODULE H5FDDSM
