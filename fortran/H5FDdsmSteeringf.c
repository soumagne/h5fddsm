/*=========================================================================

  Project                 : H5FDdsmSteering
  Module                  : H5FDdsmSteeringf.c

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

#include "H5f90dsmSteeringproto.h"
#include "H5FDdsmSteering.h"

/*---------------------------------------------------------------------------
 * Name:              h5fd_dsm_steering_init_c
 * Purpose:           Initialize the steering interface
 * Input:             comm      - Communicator used by the IO nodes
 * Outputs:           None
 * Returns:           0 on success, -1 on failure
 *---------------------------------------------------------------------------*/
int_f nh5fd_dsm_steering_init_c(int_f* comm)
{
  int      ret_value = -1;
  MPI_Comm c_comm    = *comm;
  herr_t ret;

  /*
   * Call H5FD_dsm_steering_init function.
   */
  ret = H5FD_dsm_steering_init(c_comm, NULL);
  if (ret < 0) return ret_value;
  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5fd_dsm_steering_update_c
 * Purpose:     Update steering orders
 * Inputs:      none
 * Returns:     0 on success, -1 on failure
 *---------------------------------------------------------------------------*/
int_f nh5fd_dsm_steering_update_c()
{
  int    ret_value = -1;
  herr_t ret;

  /*
   * Call H5FD_dsm_steering_update function.
   */
  ret = H5FD_dsm_steering_update();
  if (ret < 0) return ret_value;
  ret_value = 0;
  return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5fd_dsm_steering_is_enabled_c
 * Purpose:     Test if a given dataset is enabled or not
 * Inputs:      name    - H5 path of a dataset or grid object name
 *              namelen - name length
 * Returns:     0 on success, -1 on failure
 *---------------------------------------------------------------------------*/
int_f nh5fd_dsm_steering_is_enabled_c(_fcd name, int_f* namelen)
{
     int    ret_value = -1;
     char  *c_name;
     int_f  c_namelen;
     herr_t ret;

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_name = (char *)HD5f2cstring(name, (size_t)c_namelen);
     if (c_name == NULL) return ret_value;

     /*
      * Call H5FD_dsm_steering_is_enabled function.
      */
     ret = H5FD_dsm_steering_is_enabled(c_name);
     if (ret < 0) return ret_value;
     HDfree(c_name);
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5fd_dsm_steering_scalar_get_c
 * Purpose:     Get the steering scalar values
 * Inputs:      name        - property name
 *              namelen     - name length
 *              mem_type_id - memory datatype identifier
 * Outputs:     buf         - character data buffer
 * Returns:     0 on success, -1 on failure
 *---------------------------------------------------------------------------*/
int_f nh5fd_dsm_steering_scalar_get_c(_fcd name, int_f* namelen, hid_t_f* mem_type_id, _fcd buf)
{
     int    ret_value = -1;
     char  *c_name;
     int_f  c_namelen;
     hid_t  c_mem_type_id;

     herr_t ret;

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_mem_type_id = (hid_t)*mem_type_id;
     c_name = (char *)HD5f2cstring(name, (size_t)c_namelen);
     if (c_name == NULL) return ret_value;

     /*
      * Call H5FD_dsm_steering_scalar_get function.
      */
     ret = H5FD_dsm_steering_scalar_get(c_name, c_mem_type_id, _fcdtocp(buf));
     if (ret < 0) return ret_value;
     HDfree(c_name);
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5fd_dsm_steering_vector_get_c
 * Purpose:     Get the steering vector values
 * Inputs:      name        - property name
 *              namelen     - name length
 *              mem_type_id - memory datatype identifier
 *              num_elem    - number of elements to get
 * Outputs:     buf         - character data buffer
 * Returns:     0 on success, -1 on failure
 *---------------------------------------------------------------------------*/
int_f nh5fd_dsm_steering_vector_get_c(_fcd name, int_f* namelen, hid_t_f* mem_type_id, hsize_t_f* num_elem, _fcd buf)
{
     int     ret_value = -1;
     char   *c_name;
     int_f   c_namelen;
     hid_t   c_mem_type_id;
     hsize_t c_num_elem;

     herr_t ret;

     /*
      * Convert FORTRAN name to C name
      */
     c_namelen = *namelen;
     c_mem_type_id = (hid_t)*mem_type_id;
     c_num_elem = (hsize_t)*num_elem;
     c_name = (char *)HD5f2cstring(name, (size_t)c_namelen);
     if (c_name == NULL) return ret_value;

     /*
      * Call H5FD_dsm_steering_vector_get function.
      */
     ret = H5FD_dsm_steering_vector_get(c_name, c_mem_type_id, c_num_elem, _fcdtocp(buf));
     if (ret < 0) return ret_value;
     HDfree(c_name);
     ret_value = 0;
     return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5fd_dsm_dump_c
 * Purpose:     Display the content of the DSM (Debug only)
 * Inputs:      none
 * Returns:     0 on success, -1 on failure
 *---------------------------------------------------------------------------*/
int_f nh5fd_dsm_dump_c()
{
     int    ret_value = -1;
     herr_t ret;

     /*
      * Call H5FD_dsm_dump function.
      */
     ret = H5FD_dsm_dump(NULL);
     if (ret < 0) return ret_value;
     ret_value = 0;
     return ret_value;
}