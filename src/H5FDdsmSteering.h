/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmSteering.h

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

#ifndef __H5FDdsmSteering_h
#define __H5FDdsmSteering_h

#include <mpi.h>

// HDF5
#include "H5Ipublic.h"

// H5FDdsm
#include "H5FDdsmConfig.h"

#ifdef __cplusplus
extern "C" {
#endif
  H5FDdsm_EXPORT herr_t H5FD_dsm_steering_init(MPI_Comm comm, void *buffer);

  H5FDdsm_EXPORT herr_t H5FD_dsm_begin_loop(const char *name);
  H5FDdsm_EXPORT herr_t H5FD_dsm_end_loop(const char *name);

  H5FDdsm_EXPORT herr_t H5FD_dsm_is_steerable(const char *hdf_path);

  H5FDdsm_EXPORT herr_t H5FD_dsm_boolean_get(const char *name, int type, void *data);
  H5FDdsm_EXPORT herr_t H5FD_dsm_scalar_get(const char *name, int type, void *data);
  H5FDdsm_EXPORT herr_t H5FD_dsm_vector_get(const char *name, int type, void *data);
#ifdef __cplusplus
}
#endif

#endif // __H5FDdsmSteering_h
