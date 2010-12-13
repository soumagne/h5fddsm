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

#include "H5Eprivate.h" // Error handling
// H5private.h defines attribute, but we don't want it as it causes link errors on some gcc versions
#ifdef __GNUC__
# undef __attribute__
#endif
//
#include "H5FDdsmSteering.h"
#include "H5FDdsmSteerer.h"
#include "H5FDdsmManager.h"
#include "H5FDdsm.h"


#define DSM_STEERING_GOTO_ERROR(x, ret_val) \
{                                           \
   fprintf(stderr, "Error at %s %s:%d %s\n", __FILE__, FUNC, __LINE__, x); \
   err_occurred = TRUE;                     \
   HGOTO_DONE(ret_val)                      \
}

extern H5FDdsmInt32 DsmAutoAlloc(MPI_Comm comm);
extern void* DsmGetAutoAllocatedBuffer();
extern void* DsmGetAutoAllocatedManager();
//----------------------------------------------------------------------------
// C steering bindings

void *dsm_buffer = NULL; // pointer to internal dsm buffer reference


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
herr_t H5FD_dsm_steering_init(MPI_Comm comm, void *buffer)
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBuffer *dsmBuffer;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_init, FAIL)

  if (!buffer) {
    DsmAutoAlloc(comm);
    dsm_buffer = DsmGetAutoAllocatedBuffer();
  } else {
    dsm_buffer = buffer;
  }

  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("Error during initialization of the DSM Steering library", FAIL)
  }

  dsmBuffer = (H5FDdsmBuffer *)dsm_buffer;
  if (dsmBuffer->GetIsAutoAllocated() && !dsmBuffer->GetIsConnected()) {
    H5FDdsmManager *dsmManager = (H5FDdsmManager *)DsmGetAutoAllocatedManager();
    dsmManager->ReadDSMConfigFile();
    dsmManager->ConnectDSM();
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
  H5FDdsmBuffer *dsmBuffer;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_update, FAIL)

  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("Attempting to use the DSM Steering library before calling H5FD_dsm_steering_init", FAIL)
  }

  dsmBuffer = (H5FDdsmBuffer *)dsm_buffer;
  if ((dsmBuffer->GetComm()->GetCommType() == H5FD_DSM_COMM_MPI_RMA) &&
      dsmBuffer->GetIsSyncRequired() && !dsmBuffer->GetIsServer()) {
    // After possible RMA put / get from the server, need to sync windows before
    // further operations
    dsmBuffer->GetComm()->RemoteCommSync();
    dsmBuffer->SetIsSyncRequired(false);
  }
  dsmBuffer->GetSteerer()->GetSteeringCommands();
  dsmBuffer->GetSteerer()->GetDisabledObjects();
  // Automatically triggers an update of steering objects during the begin loop function
  H5FD_dsm_server_update(dsmBuffer);

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
  H5FDdsmBuffer *dsmBuffer;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_is_enabled, FAIL)

  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("Attempting to use the DSM Steering library before calling H5FD_dsm_steering_init", FAIL)
  }

  dsmBuffer = (H5FDdsmBuffer *)dsm_buffer;

  if (dsmBuffer->GetSteerer()->IsObjectEnabled(name) < 0) {
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
herr_t H5FD_dsm_steering_scalar_get(const char *name, int mem_type, void *data)
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBuffer *dsmBuffer;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_scalar_get, FAIL)

  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("Attempting to use the DSM Steering library before calling H5FD_dsm_steering_init", FAIL)
  }

  dsmBuffer = (H5FDdsmBuffer *)dsm_buffer;
  if ((mem_type == H5T_NATIVE_INT) || (mem_type == H5T_NATIVE_DOUBLE)) {
    if (!dsmBuffer->GetSteerer()->GetScalar(name, mem_type, data)) {
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
herr_t H5FD_dsm_steering_vector_get(const char *name, int mem_type, int number_of_elements, void *data)
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBuffer *dsmBuffer;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_vector_get, FAIL)

  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("Attempting to use the DSM Steering library before calling H5FD_dsm_steering_init", FAIL)
  }

  dsmBuffer = (H5FDdsmBuffer *)dsm_buffer;
  if ((mem_type == H5T_NATIVE_INT) || (mem_type == H5T_NATIVE_DOUBLE)) {
    if (!dsmBuffer->GetSteerer()->GetVector(name, mem_type, number_of_elements, data)) {
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
  H5FDdsmBuffer *dsmBuffer;
  FUNC_ENTER_NOAPI(H5FD_dsm_dump, FAIL)

  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("Attempting to use the DSM Steering library before calling H5FD_dsm_steering_init", FAIL)
  }

  dsmBuffer = (H5FDdsmBuffer *)dsm_buffer;
  if (!dsmBuffer->GetSteerer()->DsmDump()) {
    ret_value = FAIL;
  }

done:
  FUNC_LEAVE_NOAPI(ret_value);
}
