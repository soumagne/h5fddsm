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
#include "H5Ipublic.h"
#include "H5FDdsmConfig.h"

#ifdef __cplusplus
extern "C" {
#endif
  /* Description:
   * Initialize the steering interface. This must be called before using
   * the other steering functions.
   */
  H5FDdsm_EXPORT herr_t H5FD_dsm_steering_init(MPI_Comm comm, void *buffer);

  /* Description:
   * Update the steering orders and get from the GUI newly modified parameters.
   */
  H5FDdsm_EXPORT herr_t H5FD_dsm_steering_update();

  /* Description:
   * Test if a given dataset is enabled or not in the GUI.
   * Either the HDF path of a particular dataset or the grid object name
   * can bee given (In this last case, it has to match the name given in the template).
   */
  H5FDdsm_EXPORT herr_t H5FD_dsm_is_enabled(const char *name);

  /* Description:
   * Get the scalar value corresponding to the property name given in the template.
   */
  H5FDdsm_EXPORT herr_t H5FD_dsm_scalar_get(const char *name, int type, void *data);

  /* Description:
   * Get the vector valued corresponding to the property name given in the template.
   */
  H5FDdsm_EXPORT herr_t H5FD_dsm_vector_get(const char *name, int type, int number_of_elements, void *data);

  /* Description:
   * Display the content of the DSM (Debug only).
   */
  H5FDdsm_EXPORT herr_t H5FD_dsm_dump();
#ifdef __cplusplus
}
#endif

#endif /* __H5FDdsmSteering_h */
