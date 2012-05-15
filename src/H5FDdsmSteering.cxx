/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmSteering.cxx

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
// Put this before others as we must not mess up WIN32 macros/defs

#include "H5FDdsmSteering.h"
#include "H5FDdsmSteerer.h"
#include "H5FDdsmManager.h"
#include "H5FDdsm.h"
#include "H5FDdsmDriver.h"
//
#include "H5Eprivate.h" // Error handling

#define DSM_STEERING_ERROR(x) \
{                             \
   H5FDdsmError(x);           \
   return(FAIL);              \
}

#define DSM_STEERING_INIT(manager)                                     \
{                                                                      \
  if (!dsm_get_manager())                                              \
    DSM_STEERING_ERROR                                                 \
      ("DSM Steering library not connected (H5FD_dsm_steering_init)")  \
                                                                       \
  manager = static_cast<H5FDdsmManager *> (dsm_get_manager());         \
}

//----------------------------------------------------------------------------
// Function:    H5FD_dsm_steering_init
//
// Purpose:     Initialize the steering interface. This must be called before using
//              the other steering functions.
//
// Return:      Success:        non-negative
//              Failure:        negative
//
//----------------------------------------------------------------------------
herr_t H5FD_dsm_steering_init(MPI_Comm intra_comm)
{
  H5FDdsmManager *dsmManager;

  if (!dsm_get_manager()) {
    if (SUCCEED != dsm_alloc(intra_comm, NULL, 0))
      DSM_STEERING_ERROR("Error during initialization of the DSM Steering library")
  }

  dsmManager = static_cast<H5FDdsmManager *> (dsm_get_manager());

  if (dsmManager->GetIsAutoAllocated() && !dsmManager->GetIsConnected()) {
    dsmManager->ReadConfigFile();
    if (dsmManager->Connect(H5FD_DSM_TRUE) == H5FD_DSM_FAIL) {
      dsm_free();
      DSM_STEERING_ERROR("DSM Connection failed, destroying dsmManager Singleton")
    }
  }

  return(SUCCEED);
}

//----------------------------------------------------------------------------
// Function:    H5FD_dsm_steering_update
//
// Purpose:     Update the steering orders and get from the GUI newly modified parameters.
//
// Return:      Success:        non-negative
//              Failure:        negative
//
//----------------------------------------------------------------------------
herr_t H5FD_dsm_steering_update()
{
  H5FDdsmManager *dsmManager;

  DSM_STEERING_INIT(dsmManager)

  if (SUCCEED != dsm_lock()) DSM_STEERING_ERROR("Cannot lock DSM")

  if (dsmManager->GetSteerer()->GetSteeringCommands() != H5FD_DSM_SUCCESS)
    DSM_STEERING_ERROR("Cannot get steering commands")
  if (dsmManager->GetSteerer()->GetDisabledObjects() != H5FD_DSM_SUCCESS)
    DSM_STEERING_ERROR("Cannot get disabled objects")

  // Automatically send a notification so that objects are updated
  if (SUCCEED != dsm_notify(H5FD_DSM_NEW_DATA))
    DSM_STEERING_ERROR("Cannot notify DSM")

  return(SUCCEED);
}

//----------------------------------------------------------------------------
// Function:    H5FD_dsm_steering_is_enabled
//
// Purpose:     Test if a given dataset is enabled or not in the GUI.
//              Either the HDF path of a particular dataset or the grid object name
//              can be given (In this last case, it has to match the name given in the template).
//
// Return:      Success:        non-negative
//              Failure:        negative
//
//----------------------------------------------------------------------------
hbool_t H5FD_dsm_steering_is_enabled(const char *name)
{
  herr_t ret_value = TRUE;
  H5FDdsmManager *dsmManager;

  DSM_STEERING_INIT(dsmManager)

  if (dsmManager->GetSteerer()->IsObjectEnabled(name) != H5FD_DSM_TRUE)
    ret_value = FALSE;

  return(ret_value);
}

//----------------------------------------------------------------------------
// Function:    H5FD_dsm_steering_wait
//
// Purpose:     Pause and wait until completion of steering orders, released by a play.
//
// Return:      Success:        non-negative
//              Failure:        negative
//
//----------------------------------------------------------------------------
herr_t H5FD_dsm_steering_wait()
{
  H5FDdsmManager *dsmManager;

  DSM_STEERING_INIT(dsmManager)

  if (dsmManager->GetSteerer()->CheckCommand("pause") != H5FD_DSM_SUCCESS)
    DSM_STEERING_ERROR("Cannot check for pause command")

  return(SUCCEED);
}

//----------------------------------------------------------------------------
// Function:    H5FD_dsm_steering_begin_query
//
// Purpose:     Begin/End query - Avoid to open and request file lock acquisition multiple times.
//
// Return:      Success:        non-negative
//              Failure:        negative
//
//----------------------------------------------------------------------------
herr_t H5FD_dsm_steering_begin_query()
{
  H5FDdsmManager *dsmManager;

  DSM_STEERING_INIT(dsmManager)

  if (dsmManager->GetSteerer()->BeginInteractionsCache(H5F_ACC_RDWR) != H5FD_DSM_SUCCESS) {
    /* TODO do we print an error message here */
  }

  return(SUCCEED);
}

//----------------------------------------------------------------------------
// Function:    H5FD_dsm_steering_end_query
//
// Purpose:     Begin/End query - Avoid to open and request file lock acquisition multiple times.
//
// Return:      Success:        non-negative
//              Failure:        negative
//
//----------------------------------------------------------------------------
herr_t H5FD_dsm_steering_end_query()
{
  H5FDdsmManager *dsmManager;

  DSM_STEERING_INIT(dsmManager)

  if (dsmManager->GetSteerer()->EndInteractionsCache() != H5FD_DSM_SUCCESS) {
    /* TODO do we print an error message here */
  }

  return(SUCCEED);
}

//----------------------------------------------------------------------------
// Function:    H5FD_dsm_steering_get_handle
//
// Purpose:     Get/Free DSM handle to interaction dataset - can be passed
//              to HDF5 common functions for further read/write.
//
// Return:      Success:        non-negative
//              Failure:        negative
//
//----------------------------------------------------------------------------
herr_t H5FD_dsm_steering_get_handle(const char *name, hid_t *handle)
{
  H5FDdsmManager *dsmManager;

  DSM_STEERING_INIT(dsmManager)

  if (dsmManager->GetSteerer()->GetHandle(name, handle) != H5FD_DSM_SUCCESS) {
    /* TODO do we print an error message here */
  }

  return(SUCCEED);
}

//----------------------------------------------------------------------------
// Function:    H5FD_dsm_steering_free_handle
//
// Purpose:     Get/Free DSM handle to interaction dataset - can be passed
//              to HDF5 common functions for further read/write.
//
// Return:      Success:        non-negative
//              Failure:        negative
//
//----------------------------------------------------------------------------
herr_t H5FD_dsm_steering_free_handle(hid_t handle)
{
  H5FDdsmManager *dsmManager;

  DSM_STEERING_INIT(dsmManager)

  if (dsmManager->GetSteerer()->FreeHandle(handle) != H5FD_DSM_SUCCESS)
    DSM_STEERING_ERROR("Cannot free handle")

  return(SUCCEED);
}

//----------------------------------------------------------------------------
// Function:    H5FD_dsm_steering_is_set
//
// Purpose:     Test if the steerable objects has been set (in the Interactions group)
//
// Return:      Success:        non-negative
//              Failure:        negative
//
//----------------------------------------------------------------------------
herr_t H5FD_dsm_steering_is_set(const char *name, hbool_t *set)
{
  H5FDdsmManager *dsmManager;

  DSM_STEERING_INIT(dsmManager)

  if (dsmManager->GetSteerer()->IsObjectPresent(name, *((H5FDdsmBoolean*) set)) != H5FD_DSM_SUCCESS)
    DSM_STEERING_ERROR("Cannot check whether object is present or not")

  return(SUCCEED);
}

//----------------------------------------------------------------------------
// Function:    H5FD_dsm_steering_scalar_get
//
// Purpose:     Get the scalar value corresponding to the property name given in the template.
//
// Return:      Success:        non-negative
//              Failure:        negative
//
//----------------------------------------------------------------------------
herr_t H5FD_dsm_steering_scalar_get(const char *name, hid_t mem_type, void *data)
{
  H5FDdsmManager *dsmManager;

  DSM_STEERING_INIT(dsmManager)

  if (!H5Tequal(mem_type, H5T_NATIVE_INT) && !H5Tequal(mem_type, H5T_NATIVE_DOUBLE))
    DSM_STEERING_ERROR("Type not supported, please use H5T_NATIVE_INT or H5T_NATIVE_DOUBLE")

  if (dsmManager->GetSteerer()->GetVector(name, mem_type, 1, data) != H5FD_DSM_SUCCESS)
    DSM_STEERING_ERROR("Cannot get scalar")

  return(SUCCEED);
}

//----------------------------------------------------------------------------
// Function:    H5FD_dsm_steering_scalar_set
//
// Purpose:     Set the scalar value corresponding to the property name given in the template.
//
// Return:      Success:        non-negative
//              Failure:        negative
//
//----------------------------------------------------------------------------
herr_t H5FD_dsm_steering_scalar_set(const char *name, hid_t mem_type, void *data)
{
  H5FDdsmManager *dsmManager;

  DSM_STEERING_INIT(dsmManager)

  if (!H5Tequal(mem_type, H5T_NATIVE_INT) && !H5Tequal(mem_type, H5T_NATIVE_DOUBLE))
    DSM_STEERING_ERROR("Type not supported, please use H5T_NATIVE_INT or H5T_NATIVE_DOUBLE")

  if (dsmManager->GetSteerer()->SetVector(name, mem_type, 1, data) != H5FD_DSM_SUCCESS)
    DSM_STEERING_ERROR("Cannot set scalar")

  return(SUCCEED);
}

//----------------------------------------------------------------------------
// Function:    H5FD_dsm_steering_vector_get
//
// Purpose:     Get the vector valued corresponding to the property name given in the template.
//
// Return:      Success:        non-negative
//              Failure:        negative
//
//----------------------------------------------------------------------------
herr_t H5FD_dsm_steering_vector_get(const char *name, hid_t mem_type, hsize_t number_of_elements, void *data)
{
  H5FDdsmManager *dsmManager;

  DSM_STEERING_INIT(dsmManager)

  if (!H5Tequal(mem_type, H5T_NATIVE_INT) && !H5Tequal(mem_type, H5T_NATIVE_DOUBLE))
    DSM_STEERING_ERROR("Type not supported, please use H5T_NATIVE_INT or H5T_NATIVE_DOUBLE")

  if (dsmManager->GetSteerer()->GetVector(name, mem_type, number_of_elements, data) != H5FD_DSM_SUCCESS)
    DSM_STEERING_ERROR("Cannot get vector")

  return(SUCCEED);
}

//----------------------------------------------------------------------------
// Function:    H5FD_dsm_steering_vector_set
//
// Purpose:     Set the vector valued corresponding to the property name given in the template.
//
// Return:      Success:        non-negative
//              Failure:        negative
//
//----------------------------------------------------------------------------
herr_t H5FD_dsm_steering_vector_set(const char *name, hid_t mem_type, hsize_t number_of_elements, void *data)
{
  H5FDdsmManager *dsmManager;

  DSM_STEERING_INIT(dsmManager)

  if (!H5Tequal(mem_type, H5T_NATIVE_INT) && !H5Tequal(mem_type, H5T_NATIVE_DOUBLE))
    DSM_STEERING_ERROR("Type not supported, please use H5T_NATIVE_INT or H5T_NATIVE_DOUBLE")

  if (dsmManager->GetSteerer()->SetVector(name, mem_type, number_of_elements, data) != H5FD_DSM_SUCCESS)
    DSM_STEERING_ERROR("Cannot set vector")

  return(SUCCEED);
}
