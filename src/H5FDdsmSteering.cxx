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
// H5private.h defines attribute, but we don't want it as it causes link errors
// on some gcc versions
#ifdef __GNUC__
# undef __attribute__
#endif
//

#define DSM_STEERING_GOTO_ERROR(x, ret_val)                                \
{                                                                          \
   fprintf(stderr, "Error at %s %s:%d %s\n", __FILE__, FUNC, __LINE__, x); \
   err_occurred = TRUE;                                                    \
   if (err_occurred) { HGOTO_DONE(ret_val) }                               \
}

//----------------------------------------------------------------------------
// C steering bindings

//H5FDdsmBuffer *dsm_buffer = NULL; // pointer to internal DSM buffer reference

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

  if (!DsmGetManager()) {
    if (SUCCEED != DsmAlloc(intra_comm, NULL, 0, NULL, NULL, NULL))
      DSM_STEERING_GOTO_ERROR("Error during initialization of the DSM Steering library", FAIL);
  }

  dsmManager = static_cast<H5FDdsmManager *> (DsmGetManager());

  if (dsmManager->GetIsAutoAllocated() && !dsmManager->GetIsConnected()) {
    dsmManager->ReadConfigFile();
    if (dsmManager->Connect() == H5FD_DSM_FAIL) {
      DsmDealloc();
      dsm_buffer = NULL;
      DSM_STEERING_GOTO_ERROR("DSM Connection failed, destroying dsmManager Singleton", FAIL)
    }
  }

done:
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
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_update, FAIL)
  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL)
  }

  dsmBufferService = dynamic_cast<H5FDdsmBufferService *> (dsm_buffer);

  if (!dsmBufferService->GetIsLocked()) dsmBufferService->RequestLockAcquire();

  dsmBufferService->GetSteerer()->GetSteeringCommands();
  dsmBufferService->GetSteerer()->GetDisabledObjects();
  // Automatically triggers an update of steering objects during the begin loop function
  H5FD_dsm_notify(H5FD_DSM_NEW_DATA);

done:
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
herr_t H5FD_dsm_steering_is_enabled(const char *name)
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_is_enabled, FAIL)
  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL)
  }

  dsmBufferService = dynamic_cast<H5FDdsmBufferService *> (dsm_buffer);
  if (dsmBufferService->GetSteerer()->IsObjectEnabled(name) < 0) {
    ret_value = FAIL;
  }

done:
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
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_wait, FAIL)
  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL)
  }

  dsmBufferService = dynamic_cast<H5FDdsmBufferService *> (dsm_buffer);

  dsmBufferService->GetSteerer()->CheckCommand("pause");

done:
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
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_begin_query, FAIL)
  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL)
  }

  dsmBufferService = dynamic_cast<H5FDdsmBufferService *> (dsm_buffer);
  if (dsmBufferService->GetSteerer()->BeginInteractionsCache(H5F_ACC_RDONLY) < 0) {
    ret_value = FAIL;
  }

done:
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
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_end_query, FAIL)
  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL)
  }

  dsmBufferService = dynamic_cast<H5FDdsmBufferService *> (dsm_buffer);
  if (dsmBufferService->GetSteerer()->EndInteractionsCache() < 0) {
    ret_value = FAIL;
  }

done:
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
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_get_handle, FAIL)
  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL)
  }

  dsmBufferService = dynamic_cast<H5FDdsmBufferService *> (dsm_buffer);
  if (dsmBufferService->GetSteerer()->GetHandle(name, handle) < 0) {
    ret_value = FAIL;
  }
done:
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
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_free_handle, FAIL)
  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL)
  }

  dsmBufferService = dynamic_cast<H5FDdsmBufferService *> (dsm_buffer);
  if (dsmBufferService->GetSteerer()->FreeHandle(handle) < 0) {
    ret_value = FAIL;
  }

done:
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
herr_t H5FD_dsm_steering_is_set(const char *name, int *set)
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_is_set, FAIL)
  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL)
  }

  dsmBufferService = dynamic_cast<H5FDdsmBufferService *> (dsm_buffer);
  if (dsmBufferService->GetSteerer()->IsObjectPresent(name, *set) < 0) {
    ret_value = FAIL;
  }

done:
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
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_scalar_get, FAIL)
  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL)
  }

  dsmBufferService = dynamic_cast<H5FDdsmBufferService *> (dsm_buffer);
  if (H5Tequal(mem_type,H5T_NATIVE_INT) || H5Tequal(mem_type,H5T_NATIVE_DOUBLE)) {
    if (!dsmBufferService->GetSteerer()->GetScalar(name, mem_type, data)) {
      ret_value = FAIL;
    }
  } else {
    DSM_STEERING_GOTO_ERROR("Type not supported, please use H5T_NATIVE_INT or H5T_NATIVE_DOUBLE", FAIL)
  }

done:
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
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_scalar_set, FAIL)
  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL)
  }

  dsmBufferService = dynamic_cast<H5FDdsmBufferService *> (dsm_buffer);
  if (H5Tequal(mem_type,H5T_NATIVE_INT) || H5Tequal(mem_type,H5T_NATIVE_DOUBLE)) {
    if (!dsmBufferService->GetSteerer()->SetScalar(name, mem_type, data)) {
      ret_value = FAIL;
    }
  } else {
    DSM_STEERING_GOTO_ERROR("Type not supported, please use H5T_NATIVE_INT or H5T_NATIVE_DOUBLE", FAIL)
  }

done:
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
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_vector_get, FAIL)
  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL)
  }

  dsmBufferService = dynamic_cast<H5FDdsmBufferService *> (dsm_buffer);
  if (H5Tequal(mem_type,H5T_NATIVE_INT) || H5Tequal(mem_type,H5T_NATIVE_DOUBLE)) {
    if (!dsmBufferService->GetSteerer()->GetVector(name, mem_type, number_of_elements, data)) {
      ret_value = FAIL;
    }
  } else {
    DSM_STEERING_GOTO_ERROR("Type not supported, please use H5T_NATIVE_INT or H5T_NATIVE_DOUBLE", FAIL)
  }

done:
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
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_vector_set, FAIL)
  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL)
  }

  dsmBufferService = dynamic_cast<H5FDdsmBufferService *> (dsm_buffer);
  if (H5Tequal(mem_type,H5T_NATIVE_INT) || H5Tequal(mem_type,H5T_NATIVE_DOUBLE)) {
    if (!dsmBufferService->GetSteerer()->SetVector(name, mem_type, number_of_elements, data)) {
      ret_value = FAIL;
    }
  } else {
    DSM_STEERING_GOTO_ERROR("Type not supported, please use H5T_NATIVE_INT or H5T_NATIVE_DOUBLE", FAIL)
  }

done:
  FUNC_LEAVE_NOAPI(ret_value);
}

//----------------------------------------------------------------------------
// Function:    H5FD_dsm_dump
//
// Purpose:     Display the content of the DSM (Debug only).
//
// Return:      Success:        non-negative
//              Failure:        negative
//
//----------------------------------------------------------------------------
herr_t H5FD_dsm_dump()
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBufferService *dsmBufferService;
  FUNC_ENTER_NOAPI(H5FD_dsm_dump, FAIL)
  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("DSM Steering library not connected (H5FD_dsm_steering_init)", FAIL)
  }

  dsmBufferService = dynamic_cast<H5FDdsmBufferService *> (dsm_buffer);
  if (!dsmBufferService->GetSteerer()->DsmDump()) {
    ret_value = FAIL;
  }

done:
  FUNC_LEAVE_NOAPI(ret_value);
}
