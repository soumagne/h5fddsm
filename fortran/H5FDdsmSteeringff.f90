! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! Project                 : H5FDdsmSteering
! Module                  : H5FDdsmSteeringff.f90
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
! This file contains Fortran90 interfaces for H5FDdsmSteering functions.
!
    MODULE H5FDDSM_STEERING
         USE H5FDDSM_GLOBAL
         USE H5GLOBAL
         CONTAINS

!----------------------------------------------------------------------
! Name:        h5fd_dsm_steering_init_f
!
! Purpose:     Initialize the steering interface
!
! Inputs:
!        comm        - Communicator used by the IO nodes
! Outputs:
!        hdferr:        - error code
!                     Success:  0
!                     Failure: -1
! Optional parameters:
!                NONE
!
!----------------------------------------------------------------------
         SUBROUTINE h5fd_dsm_steering_init_f(comm, hdferr)
            IMPLICIT NONE
            INTEGER, INTENT(IN)  :: comm         ! Default communicator
            INTEGER, INTENT(OUT) :: hdferr       ! Error code

            INTEGER, EXTERNAL :: h5fd_dsm_steering_init_c
            hdferr = h5fd_dsm_steering_init_c(comm)
         END SUBROUTINE h5fd_dsm_steering_init_f

!----------------------------------------------------------------------
! Name:     h5fd_dsm_steering_update_f
!
! Purpose:  Update steering orders
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
         SUBROUTINE h5fd_dsm_steering_udpate_f(hdferr)
            IMPLICIT NONE
            INTEGER, INTENT(OUT) :: hdferr       ! Error code

            INTEGER, EXTERNAL :: h5fd_dsm_steering_update_c
            hdferr = h5fd_dsm_steering_update_c()
         END SUBROUTINE h5fd_dsm_steering_udpate_f

!----------------------------------------------------------------------
! Name:     h5fd_dsm_steering_is_enabled_f
!
! Purpose:  Test if a given dataset is enabled or not
!
! Inputs:
!       name        - H5 path of a dataset or grid object name
! Outputs:
!       hdferr:     - error code
!                   Success:  0
!                   Failure: -1
! Optional parameters:
!               NONE
!
!----------------------------------------------------------------------
         SUBROUTINE h5fd_dsm_steering_is_enabled_f(name, hdferr)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: name ! Name of the dataset
            INTEGER, INTENT(OUT) :: hdferr       ! Error code
            INTEGER :: namelen ! Length of the name character string

            INTEGER, EXTERNAL :: h5fd_dsm_steering_is_enabled_c
            namelen = LEN_TRIM(name)
            hdferr = h5fd_dsm_steering_is_enabled_c(name, namelen)
         END SUBROUTINE h5fd_dsm_steering_is_enabled_f


! **********************************
! **********************************

         SUBROUTINE h5fd_dsm_steering_wait_f(hdferr)
            IMPLICIT NONE
            INTEGER, INTENT(OUT) :: hdferr       ! Error code

            INTEGER, EXTERNAL :: h5fd_dsm_steering_wait_c
            hdferr = h5fd_dsm_steering_wait_c()
         END SUBROUTINE h5fd_dsm_steering_wait_f

         SUBROUTINE h5fd_dsm_steering_begin_query_f(hdferr)
            IMPLICIT NONE
            INTEGER, INTENT(OUT) :: hdferr       ! Error code

            INTEGER, EXTERNAL :: h5fd_dsm_steering_begin_query_c
            hdferr = h5fd_dsm_steering_begin_query_c()
         END SUBROUTINE h5fd_dsm_steering_begin_query_f

         SUBROUTINE h5fd_dsm_steering_end_query_f(hdferr)
            IMPLICIT NONE
            INTEGER, INTENT(OUT) :: hdferr       ! Error code

            INTEGER, EXTERNAL :: h5fd_dsm_steering_end_query_c
            hdferr = h5fd_dsm_steering_end_query_c()
         END SUBROUTINE h5fd_dsm_steering_end_query_f

         SUBROUTINE h5fd_dsm_steering_gethandle_f(name, handle, hdferr)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: name ! Name of the property
            INTEGER(HID_T), INTENT(OUT) :: handle ! identifier
            INTEGER, INTENT(OUT) :: hdferr       ! Error code
            INTEGER :: namelen ! Length of the name character string

            INTEGER, EXTERNAL :: h5fd_dsm_steering_gethandle_c
            namelen = LEN_TRIM(name)
            hdferr = h5fd_dsm_steering_gethandle_c(name, namelen, handle)
         END SUBROUTINE h5fd_dsm_steering_gethandle_f

         SUBROUTINE h5fd_dsm_steering_freehandle_f(handle, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(OUT) :: handle ! identifier
            INTEGER, INTENT(OUT) :: hdferr       ! Error code

            INTEGER, EXTERNAL :: h5fd_dsm_steering_freehandle_c
            hdferr = h5fd_dsm_steering_freehandle_c(handle)
         END SUBROUTINE h5fd_dsm_steering_freehandle_f

! **********************************
! **********************************

!----------------------------------------------------------------------
! Name:     h5fd_dsm_steering_is_set_f
!
! Purpose:  Test if a given object exists in the steerable "Interactions" group
!
! Inputs:
!       name        - object name
! Outputs:
!       hdferr:     - error code
!                   Success:  0
!                   Failure: -1
! Optional parameters:
!               NONE
!
!----------------------------------------------------------------------
         SUBROUTINE h5fd_dsm_steering_is_set_f(name, set, hdferr)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: name ! Name of the dataset
            INTEGER, INTENT(OUT) :: hdferr       ! Error code
            INTEGER, INTENT(INOUT) :: set        ! flag to hold result
            INTEGER :: namelen ! Length of the name character string

            INTEGER, EXTERNAL :: h5fd_dsm_steering_is_set_c
            namelen = LEN_TRIM(name)
            hdferr = h5fd_dsm_steering_is_set_c(name, namelen, set)
         END SUBROUTINE h5fd_dsm_steering_is_set_f

!----------------------------------------------------------------------
! Name:     h5fd_dsm_steering_scalar_get_f
!
! Purpose:  Get the steering scalar values
!
! Inputs:
!       name        - property name
!       mem_type_id - memory type identifier
! Outputs:
!       buf         - buffer to read data in
!       hdferr:     - error code
!                   Success:  0
!                   Failure: -1
! Optional parameters:
!               NONE
!
!----------------------------------------------------------------------
         SUBROUTINE h5fd_dsm_steering_scalar_get_f(name, mem_type_id, buf, hdferr)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: name ! Name of the property
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(INOUT) :: buf        ! data
            INTEGER, INTENT(OUT) :: hdferr       ! Error code
            INTEGER :: namelen ! Length of the name character string

            INTEGER, EXTERNAL :: h5fd_dsm_steering_scalar_get_c
            namelen = LEN_TRIM(name)
            hdferr = h5fd_dsm_steering_scalar_get_c(name, namelen, mem_type_id, buf)
         END SUBROUTINE h5fd_dsm_steering_scalar_get_f

!----------------------------------------------------------------------
! Name:     h5fd_dsm_steering_vector_get_f
!
! Purpose:  Get the steering vector values
!
! Inputs:
!       name        - property name
!       mem_type_id - memory type identifier
!       num_elem    - number of elements to get
! Outputs:
!       buf         - buffer to read data in
!       hdferr:     - error code
!                   Success:  0
!                   Failure: -1
! Optional parameters:
!               NONE
!
!----------------------------------------------------------------------
         SUBROUTINE h5fd_dsm_steering_vector_get_int8_f(name, mem_type_id, num_elem, buf, hdferr)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: name ! Name of the property
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN) :: num_elem
            INTEGER, INTENT(INOUT), DIMENSION(num_elem) :: buf
            INTEGER, INTENT(OUT) :: hdferr       ! Error code
            INTEGER :: namelen ! Length of the name character string

            INTEGER, EXTERNAL :: h5fd_dsm_steering_vector_get_c
            namelen = LEN_TRIM(name)
            hdferr = h5fd_dsm_steering_vector_get_c(name, namelen, mem_type_id, num_elem, buf)
         END SUBROUTINE h5fd_dsm_steering_vector_get_int8_f

         SUBROUTINE h5fd_dsm_steering_vector_get_real8_f(name, mem_type_id, num_elem, buf, hdferr)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: name ! Name of the property
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN) :: num_elem
            REAL(8), INTENT(INOUT), DIMENSION(num_elem) :: buf
            INTEGER, INTENT(OUT) :: hdferr       ! Error code
            INTEGER :: namelen ! Length of the name character string

            INTEGER, EXTERNAL :: h5fd_dsm_steering_vector_get_c
            namelen = LEN_TRIM(name)
            hdferr = h5fd_dsm_steering_vector_get_c(name, namelen, mem_type_id, num_elem, buf)
         END SUBROUTINE h5fd_dsm_steering_vector_get_real8_f
         
         SUBROUTINE h5fd_dsm_steering_vector_set_int8_f(name, mem_type_id, num_elem, buf, hdferr)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: name ! Name of the property
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN) :: num_elem
            INTEGER, INTENT(INOUT), DIMENSION(num_elem) :: buf
            INTEGER, INTENT(OUT) :: hdferr       ! Error code
            INTEGER :: namelen ! Length of the name character string

            INTEGER, EXTERNAL :: h5fd_dsm_steering_vector_set_c
            namelen = LEN_TRIM(name)
            hdferr = h5fd_dsm_steering_vector_set_c(name, namelen, mem_type_id, num_elem, buf)
         END SUBROUTINE h5fd_dsm_steering_vector_set_int8_f

         SUBROUTINE h5fd_dsm_steering_vector_set_real8_f(name, mem_type_id, num_elem, buf, hdferr)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: name ! Name of the property
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN) :: num_elem
            REAL(8), INTENT(INOUT), DIMENSION(num_elem) :: buf
            INTEGER, INTENT(OUT) :: hdferr       ! Error code
            INTEGER :: namelen ! Length of the name character string

            INTEGER, EXTERNAL :: h5fd_dsm_steering_vector_set_c
            namelen = LEN_TRIM(name)
            hdferr = h5fd_dsm_steering_vector_set_c(name, namelen, mem_type_id, num_elem, buf)
         END SUBROUTINE h5fd_dsm_steering_vector_set_real8_f
         
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
            IMPLICIT NONE
            INTEGER, INTENT(OUT) :: hdferr       ! Error code

            INTEGER, EXTERNAL :: h5fd_dsm_dump_c
            hdferr = h5fd_dsm_dump_c()
         END SUBROUTINE h5fd_dsm_dump_f

    END MODULE H5FDDSM_STEERING
