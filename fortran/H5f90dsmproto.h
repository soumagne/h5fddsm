/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5f90dsmproto.h

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

#ifndef __H5f90dsmproto_h
#define __H5f90dsmproto_h

#include "H5f90.h"
#include "H5public.h"

#define nh5fd_dsm_init_flags_c H5_FC_FUNC_(h5fd_dsm_init_flags_c, H5FD_DSM_INIT_FLAGS_C)
H5_FCDLL int_f nh5fd_dsm_init_flags_c(int_f *h5fd_dsm_flags);

#define nh5pset_fapl_dsm_c H5_FC_FUNC_(h5pset_fapl_dsm_c, H5PSET_FAPL_DSM_C)
H5_FCDLL int_f nh5pset_fapl_dsm_c(hid_t_f *prp_id, int_f* comm);

#define nh5pget_fapl_dsm_c H5_FC_FUNC_(h5pget_fapl_dsm_c, H5PGET_FAPL_DSM_C)
H5_FCDLL int_f nh5pget_fapl_dsm_c(hid_t_f *prp_id, int_f* comm);

#define nh5fd_dsm_set_options_c H5_FC_FUNC_(h5fd_dsm_set_options_c, H5FD_DSM_SET_OPTIONS_C)
H5_FCDLL int_f nh5fd_dsm_set_options_c(int_f* options);

#define nh5fd_dsm_notify_c H5_FC_FUNC_(h5fd_dsm_notify_c, H5FD_DSM_NOTIFY_C)
H5_FCDLL int_f nh5fd_dsm_notify_c(int_f* notifications);

#endif /* __H5f90dsmproto_h */
