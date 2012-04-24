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

#define DSM_STEERING_GOTO_ERROR(x, ret_val)                                \
{                                                                          \
   fprintf(stderr, "Error at %s %s:%d %s\n", __FILE__, FUNC, __LINE__, x); \
   err_occurred = TRUE;                                                    \
   if (err_occurred) { HGOTO_DONE(ret_val) }                               \
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
  herr_t ret_value = SUCCEED;
  H5FDdsmManager *dsmManager;

  FUNC_ENTER_NOAPI(H5FD_dsm_steering_init, FAIL)

  if (!dsm_get_manager()) {
    if (SUCCEED != dsm_alloc(intra_comm, NULL, 0))
      DSM_STEERING_GOTO_ERROR("Error during initialization of the DSM Steering library", FAIL);
  }

  dsmManager = static_cast<H5FDdsmManager *> (dsm_get_manager());

  if (dsmManager->GetIsAutoAllocated() && !dsmManager->GetIsConnected()) {
    dsmManager->ReadConfigFile();
    if (dsmManager->Connect(H5FD_DSM_TRUE) == H5FD_DSM_FAIL) {
      dsm_free();
      DSM_STEERING_GOTO_ERROR("DSM Connection failed, destroying dsmManager Singleton", FAIL);
    }
  }

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
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
  herr_t ret_value = SUCCEED;
  H5FDdsmManager *dsmManager;

  FUNC_ENTER_NOAPI(H5FD_dsm_steering_update, FAIL)

  if (!dsm_get_manager())
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL);

  dsmManager = static_cast<H5FDdsmManager *> (dsm_get_manager());

  if (SUCCEED != dsm_lock())
    DSM_STEERING_GOTO_ERROR("Cannot lock DSM", FAIL);

  if (dsmManager->GetSteerer()->GetSteeringCommands() != H5FD_DSM_SUCCESS)
    DSM_STEERING_GOTO_ERROR("Cannot get steering commands", FAIL);
  if (dsmManager->GetSteerer()->GetDisabledObjects() != H5FD_DSM_SUCCESS)
    DSM_STEERING_GOTO_ERROR("Cannot get disabled objects", FAIL);

  // Automatically send a notification so that objects are updated
  if (SUCCEED != dsm_notify(H5FD_DSM_NEW_DATA))
    DSM_STEERING_GOTO_ERROR("Cannot notify DSM", FAIL);

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
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

  FUNC_ENTER_NOAPI(H5FD_dsm_steering_is_enabled, FALSE)

  if (!dsm_get_manager())
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FALSE);

  dsmManager = static_cast<H5FDdsmManager *> (dsm_get_manager());

  if (dsmManager->GetSteerer()->IsObjectEnabled(name) != H5FD_DSM_TRUE)
    ret_value = FALSE;

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
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
  herr_t ret_value = SUCCEED;
  H5FDdsmManager *dsmManager;

  FUNC_ENTER_NOAPI(H5FD_dsm_steering_wait, FAIL)

  if (!dsm_get_manager())
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL);

  dsmManager = static_cast<H5FDdsmManager *> (dsm_get_manager());

  if (dsmManager->GetSteerer()->CheckCommand("pause") != H5FD_DSM_SUCCESS)
    DSM_STEERING_GOTO_ERROR("Cannot check for pause command", FAIL);

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
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
  herr_t ret_value = SUCCEED;
  H5FDdsmManager *dsmManager;

  FUNC_ENTER_NOAPI(H5FD_dsm_steering_begin_query, FAIL)

  if (!dsm_get_manager())
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL);

  dsmManager = static_cast<H5FDdsmManager *> (dsm_get_manager());

  if (dsmManager->GetSteerer()->BeginInteractionsCache(H5F_ACC_RDWR) != H5FD_DSM_SUCCESS) {
    /* TODO do we print an error message here */
    ret_value = FAIL;
  }

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
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
  herr_t ret_value = SUCCEED;
  H5FDdsmManager *dsmManager;

  FUNC_ENTER_NOAPI(H5FD_dsm_steering_end_query, FAIL)

  if (!dsm_get_manager())
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL);

  dsmManager = static_cast<H5FDdsmManager *> (dsm_get_manager());

  if (dsmManager->GetSteerer()->EndInteractionsCache() != H5FD_DSM_SUCCESS) {
    /* TODO do we print an error message here */
    ret_value = FAIL;
  }

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
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
  herr_t ret_value = SUCCEED;
  H5FDdsmManager *dsmManager;

  FUNC_ENTER_NOAPI(H5FD_dsm_steering_get_handle, FAIL)

  if (!dsm_get_manager())
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL);

  dsmManager = static_cast<H5FDdsmManager *> (dsm_get_manager());

  if (dsmManager->GetSteerer()->GetHandle(name, handle) != H5FD_DSM_SUCCESS) {
    /* TODO do we print an error message here */
    ret_value = FAIL;
  }

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
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
  herr_t ret_value = SUCCEED;
  H5FDdsmManager *dsmManager;

  FUNC_ENTER_NOAPI(H5FD_dsm_steering_free_handle, FAIL)

  if (!dsm_get_manager())
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL);

  dsmManager = static_cast<H5FDdsmManager *> (dsm_get_manager());

  if (dsmManager->GetSteerer()->FreeHandle(handle) != H5FD_DSM_SUCCESS)
    DSM_STEERING_GOTO_ERROR("Cannot free handle", FAIL);

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
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
  herr_t ret_value = SUCCEED;
  H5FDdsmManager *dsmManager;

  FUNC_ENTER_NOAPI(H5FD_dsm_steering_is_set, FAIL)

  if (!dsm_get_manager())
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL);

  dsmManager = static_cast<H5FDdsmManager *> (dsm_get_manager());

  if (dsmManager->GetSteerer()->IsObjectPresent(name, *((H5FDdsmBoolean*) set)) != H5FD_DSM_SUCCESS)
    DSM_STEERING_GOTO_ERROR("Cannot check whether object is present or not", FAIL);

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
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
  herr_t ret_value = SUCCEED;
  H5FDdsmManager *dsmManager;

  FUNC_ENTER_NOAPI(H5FD_dsm_steering_scalar_get, FAIL)

  if (!dsm_get_manager())
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL);

  dsmManager = static_cast<H5FDdsmManager *> (dsm_get_manager());

  if (!H5Tequal(mem_type, H5T_NATIVE_INT) && !H5Tequal(mem_type, H5T_NATIVE_DOUBLE))
    DSM_STEERING_GOTO_ERROR("Type not supported, please use H5T_NATIVE_INT or H5T_NATIVE_DOUBLE", FAIL);

  if (dsmManager->GetSteerer()->GetVector(name, mem_type, 1, data) != H5FD_DSM_SUCCESS)
    DSM_STEERING_GOTO_ERROR("Cannot get scalar", FAIL);

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
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
  herr_t ret_value = SUCCEED;
  H5FDdsmManager *dsmManager;

  FUNC_ENTER_NOAPI(H5FD_dsm_steering_scalar_set, FAIL)

  if (!dsm_get_manager())
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL)

  dsmManager = static_cast<H5FDdsmManager *> (dsm_get_manager());

  if (!H5Tequal(mem_type, H5T_NATIVE_INT) && !H5Tequal(mem_type, H5T_NATIVE_DOUBLE))
    DSM_STEERING_GOTO_ERROR("Type not supported, please use H5T_NATIVE_INT or H5T_NATIVE_DOUBLE", FAIL);

  if (dsmManager->GetSteerer()->SetVector(name, mem_type, 1, data) != H5FD_DSM_SUCCESS)
    DSM_STEERING_GOTO_ERROR("Cannot set scalar", FAIL);

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
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
  herr_t ret_value = SUCCEED;
  H5FDdsmManager *dsmManager;

  FUNC_ENTER_NOAPI(H5FD_dsm_steering_vector_get, FAIL)

  if (!dsm_get_manager())
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL)

  dsmManager = static_cast<H5FDdsmManager *> (dsm_get_manager());

  if (!H5Tequal(mem_type, H5T_NATIVE_INT) && !H5Tequal(mem_type, H5T_NATIVE_DOUBLE))
    DSM_STEERING_GOTO_ERROR("Type not supported, please use H5T_NATIVE_INT or H5T_NATIVE_DOUBLE", FAIL);

  if (dsmManager->GetSteerer()->GetVector(name, mem_type, number_of_elements, data) != H5FD_DSM_SUCCESS)
    DSM_STEERING_GOTO_ERROR("Cannot get vector", FAIL);

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
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
  herr_t ret_value = SUCCEED;
  H5FDdsmManager *dsmManager;

  FUNC_ENTER_NOAPI(H5FD_dsm_steering_vector_set, FAIL)

  if (!dsm_get_manager())
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL)

  dsmManager = static_cast<H5FDdsmManager *> (dsm_get_manager());

  if (!H5Tequal(mem_type, H5T_NATIVE_INT) && !H5Tequal(mem_type, H5T_NATIVE_DOUBLE))
    DSM_STEERING_GOTO_ERROR("Type not supported, please use H5T_NATIVE_INT or H5T_NATIVE_DOUBLE", FAIL);

  if (dsmManager->GetSteerer()->SetVector(name, mem_type, number_of_elements, data) != H5FD_DSM_SUCCESS)
    DSM_STEERING_GOTO_ERROR("Cannot set vector", FAIL);

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}
