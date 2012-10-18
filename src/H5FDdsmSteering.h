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
#include "H5VLdsoConfig.h"

#ifdef __cplusplus
extern "C" {
#endif
  /* Description:
   * Initialize the steering interface. This must be called before using
   * the other steering functions.
   */
  H5VLdso_EXPORT herr_t H5FD_dsm_steering_init(MPI_Comm intra_comm);

  /* Description:
   * Refresh and update the state of raw steering commands such as pause/resume
   * and datasets that have been enabled/disabled through the GUI. Any
   * simulation code that needs to check for these commands should make use of
   * this call. In case of a pause command being sent in, the simulation waits
   * for a resume command at this control point. Note that creating a new HDF
   * file does not reset these values.
   */
  H5VLdso_EXPORT herr_t H5FD_dsm_steering_update();

  /* Description:
   * Test if a given dataset is enabled or not in the GUI.
   * Either the HDF path of a particular dataset or the grid object name
   * can be given (In this last case, it has to match the name given in the template).
   */
  H5VLdso_EXPORT hbool_t H5FD_dsm_steering_is_enabled(const char *name);

  /* Description:
   * Pause and wait until completion of steering orders, released by a play.
   */
  H5VLdso_EXPORT herr_t H5FD_dsm_steering_wait();

  /* Description:
   * Begin/End query - Avoid to open and request file lock acquisition multiple times.
   */
  H5VLdso_EXPORT herr_t H5FD_dsm_steering_begin_query();
  H5VLdso_EXPORT herr_t H5FD_dsm_steering_end_query();

  /* Description:
   * Get/Free DSM handle to interaction dataset - can be passed
   * to HDF5 common functions for further read/write.
   */
  H5VLdso_EXPORT herr_t H5FD_dsm_steering_get_handle(const char *name, hid_t *handle);
  H5VLdso_EXPORT herr_t H5FD_dsm_steering_free_handle(hid_t handle);

  /* Description:
   * Return true if the object exists in the "Interactions" group.
   */
  H5VLdso_EXPORT herr_t H5FD_dsm_steering_is_set(const char *name, hbool_t *set);

  /* Description:
   * Get/Set the scalar value corresponding to the property name given in the template.
   */
  H5VLdso_EXPORT herr_t H5FD_dsm_steering_scalar_get(const char *name, hid_t mem_type, void *data);
  H5VLdso_EXPORT herr_t H5FD_dsm_steering_scalar_set(const char *name, hid_t mem_type, void *data);

  /* Description:
   * Get/Set the vector values corresponding to the property name given in the template.
   */
  H5VLdso_EXPORT herr_t H5FD_dsm_steering_vector_get(const char *name, hid_t mem_type, hsize_t number_of_elements, void *data);
  H5VLdso_EXPORT herr_t H5FD_dsm_steering_vector_set(const char *name, hid_t mem_type, hsize_t number_of_elements, void *data);

#ifdef __cplusplus
}
#endif

#endif /* __H5FDdsmSteering_h */
