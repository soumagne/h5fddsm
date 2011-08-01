/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmf.c

  Authors:
     John Biddiscombe     Jerome Soumagne
     biddisco@cscs.ch     soumagne@cscs.ch

  Copyright (C) CSCS - Swiss National Supercomputing Centre.
  You may use modify and and distribute this code freely providing
  1) This copyright notice appears on all copies of source code
  2) An acknowledgment appears with any substantial usage of the code
  3) If this code is contributed to any other open source project, it
  must not be reformatted such that the indentation, bracketing or
  overall style is modified significantly.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  This work has received funding from the European Community's Seventh
  Framework Programme (FP7/2007-2013) under grant agreement 225967 “NextMuSE”

=========================================================================*/

#include "H5f90dsmproto.h"
#include "H5FDdsm.h"

/*---------------------------------------------------------------------------
 * Name:              h5fd_dsm_init_flags_c
 * Purpose:           Initialize H5FD DSM Fortran flags
 * Input:             h5fd_dsm_flags    - H5FD DSM interface flags
 * Outputs:           None
 * Returns:           0 on success, -1 on failure
 *---------------------------------------------------------------------------*/
int_f nh5fd_dsm_init_flags_c(int_f* h5fd_dsm_flags)
{
  int ret_value = -1;

  h5fd_dsm_flags[0] = (int_f)H5FD_DSM_DONT_RELEASE;
  h5fd_dsm_flags[1] = (int_f)H5FD_DSM_DONT_NOTIFY;
  h5fd_dsm_flags[2] = (int_f)H5FD_DSM_NEW_DATA;
  h5fd_dsm_flags[3] = (int_f)H5FD_DSM_NEW_INFORMATION;

  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pset_fapl_dsm_c
 * Purpose:     Call H5Pset_fapl_dsm to set mode for DSM parallel I/O 
 * Inputs:      prp_id    - property list identifier
 *              comm      - Communicator used by the IO nodes
 * Returns:     0 on success, -1 on failure
 *---------------------------------------------------------------------------*/
int_f nh5pset_fapl_dsm_c(hid_t_f *prp_id, int_f* comm)
{
     int      ret_value = -1;
     hid_t    c_prp_id  = *prp_id;
     MPI_Comm c_comm    = MPI_Comm_f2c(*comm);
     herr_t ret;

     /*
      * Call H5Pset_fapl_dsm function.
      */
     ret = H5Pset_fapl_dsm(c_prp_id, c_comm, NULL);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5pget_fapl_dsm_c
 * Purpose:     Call H5Pget_fapl_dsm to retrieve communicator
 * Inputs:      prp_id - property list identifier
 *              comm   - buffer to return MPI communicator
 * Returns:     0 on success, -1 on failure
 *---------------------------------------------------------------------------*/
int_f nh5pget_fapl_dsm_c(hid_t_f *prp_id, int_f* comm)
{
     int    ret_value = -1;
     hid_t  c_prp_id;
     MPI_Comm c_comm;
     herr_t ret;

     /*
      * Call H5Pget_fapl_dsm function.
      */
     c_prp_id = *prp_id;
     ret = H5Pget_fapl_dsm(c_prp_id, &c_comm, NULL);
     if (ret < 0) return ret_value;
     *comm = (int_f) MPI_Comm_c2f(c_comm);
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5fd_dsm_set_options_c
 * Purpose:     Call H5FD_dsm_set_options to set specific option to the DSM
 * Inputs:      options      - specific options are:
 *                             - H5FD_DSM_DONT_RELEASE
 *                             - H5FD_DSM_DONT_NOTIFY
 * Returns:     0 on success, -1 on failure
 *---------------------------------------------------------------------------*/
int_f nh5fd_dsm_set_options_c(int_f* options)
{
     int        ret_value = -1;
     unsigned long c_options = *options;
     herr_t ret;

     /*
      * Call H5FD_dsm_set_options function.
      */
     ret = H5FD_dsm_set_options(c_options, NULL);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5fd_dsm_notify_c
 * Purpose:     Call H5FD_dsm_notify to manually send a notification to the
 *              DSM host
 * Inputs:      options      - notifications are:
   *                           - H5FD_DSM_NEW_DATA
   *                           - H5FD_DSM_NEW_INFORMATION
 * Returns:     0 on success, -1 on failure
 *---------------------------------------------------------------------------*/
int_f nh5fd_dsm_notify_c(int_f* notifications)
{
     int    ret_value = -1;
     unsigned long c_notifications = *notifications;
     herr_t ret;

     /*
      * Call H5FD_dsm_notify function.
      */
     ret = H5FD_dsm_notify(c_notifications, NULL);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}
