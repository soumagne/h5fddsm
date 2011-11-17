/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5f90dsmSteeringproto.h

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

#ifndef __H5f90dsmSteeringproto_h
#define __H5f90dsmSteeringproto_h

#include "H5f90.h"
#include "H5public.h"

#define nh5fd_dsm_steering_init_c H5_FC_FUNC_(h5fd_dsm_steering_init_c, H5FD_DSM_STEERING_INIT_C)
H5_FCDLL int_f nh5fd_dsm_steering_init_c(int_f* comm);

#define nh5fd_dsm_steering_update_c H5_FC_FUNC_(h5fd_dsm_steering_update_c, H5FD_DSM_STEERING_UPDATE_C)
H5_FCDLL int_f nh5fd_dsm_steering_update_c();

#define nh5fd_dsm_steering_is_enabled_c H5_FC_FUNC_(h5fd_dsm_steering_is_enabled_c, H5FD_DSM_STEERING_IS_ENABLED_C)
H5_FCDLL int_f nh5fd_dsm_steering_is_enabled_c(_fcd name, int_f* namelen);

#define nh5fd_dsm_steering_wait_c H5_FC_FUNC_(h5fd_dsm_steering_wait_c, H5FD_DSM_STEERING_WAIT_C)
H5_FCDLL int_f nh5fd_dsm_steering_wait_c();

#define nh5fd_dsm_steering_begin_query_c H5_FC_FUNC_(h5fd_dsm_steering_begin_query_c, H5FD_DSM_STEERING_BEGIN_QUERY_C)
H5_FCDLL int_f nh5fd_dsm_steering_begin_query_c();

#define nh5fd_dsm_steering_end_query_c H5_FC_FUNC_(h5fd_dsm_steering_end_query_c, H5FD_DSM_STEERING_END_QUERY_C)
H5_FCDLL int_f nh5fd_dsm_steering_end_query_c();

#define nh5fd_dsm_steering_get_handle_c H5_FC_FUNC_(h5fd_dsm_steering_get_handle_c, H5FD_DSM_STEERING_GET_HANDLE_C)
H5_FCDLL int_f nh5fd_dsm_steering_get_handle_c(_fcd name, int_f* namelen, int_f* handle);

#define nh5fd_dsm_steering_free_handle_c H5_FC_FUNC_(h5fd_dsm_steering_free_handle_c, H5FD_DSM_STEERING_FREE_HANDLE_C)
H5_FCDLL int_f nh5fd_dsm_steering_free_handle_c(int_f* handle);

#define nh5fd_dsm_steering_is_set_c H5_FC_FUNC_(h5fd_dsm_steering_is_set_c, H5FD_DSM_STEERING_IS_SET_C)
H5_FCDLL int_f nh5fd_dsm_steering_is_set_c(_fcd name, int_f* namelen, int_f* set);

#define nh5fd_dsm_steering_scalar_get_c H5_FC_FUNC_(h5fd_dsm_steering_scalar_get_c, H5FD_DSM_STEERING_SCALAR_GET_C)
H5_FCDLL int_f nh5fd_dsm_steering_scalar_get_c(_fcd name, int_f* namelen, hid_t_f* mem_type_id, _fcd buf);

#define nh5fd_dsm_steering_scalar_set_c H5_FC_FUNC_(h5fd_dsm_steering_scalar_set_c, H5FD_DSM_STEERING_SCALAR_SET_C)
H5_FCDLL int_f nh5fd_dsm_steering_scalar_set_c(_fcd name, int_f* namelen, hid_t_f* mem_type_id, _fcd buf);

#define nh5fd_dsm_steering_vector_get_c H5_FC_FUNC_(h5fd_dsm_steering_vector_get_c, H5FD_DSM_STEERING_VECTOR_GET_C)
H5_FCDLL int_f nh5fd_dsm_steering_vector_get_c(_fcd name, int_f* namelen, hid_t_f* mem_type_id, hsize_t_f* num_elem, _fcd buf);

#define nh5fd_dsm_steering_vector_set_c H5_FC_FUNC_(h5fd_dsm_steering_vector_set_c, H5FD_DSM_STEERING_VECTOR_SET_C)
H5_FCDLL int_f nh5fd_dsm_steering_vector_set_c(_fcd name, int_f* namelen, hid_t_f* mem_type_id, hsize_t_f* num_elem, _fcd buf);

#endif /* __H5f90dsmSteeringproto_h */
