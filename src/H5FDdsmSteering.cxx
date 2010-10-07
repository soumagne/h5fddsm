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
#include "H5FDdsm.h"
#include "H5FDdsmSteering.h"
#include "H5FDdsmSteerer.h"
#include "H5FDdsmBuffer.h"

#define DSM_STEERING_GOTO_ERROR(x, ret_val) \
{                                           \
   fprintf(stderr, "Error at %s %s:%d %s\n", __FILE__, FUNC, __LINE__, x); \
   err_occurred = TRUE;                     \
   HGOTO_DONE(ret_val)                      \
}

extern H5FDdsmInt32 DsmAutoAlloc(MPI_Comm comm);
extern void* DsmGetAutoAllocatedBuffer();
//----------------------------------------------------------------------------
// C steering bindings

void *dsm_buffer = NULL; // pointer to internal dsm buffer reference
//----------------------------------------------------------------------------
herr_t H5FD_dsm_steering_init(MPI_Comm comm)
{
  herr_t ret_value = SUCCEED;
  FUNC_ENTER_NOAPI(H5FD_dsm_steering_init, FAIL)

  DsmAutoAlloc(comm);
  dsm_buffer = DsmGetAutoAllocatedBuffer();

  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("Error during initialization of the DSM Steering library", FAIL)
  }

done:
  FUNC_LEAVE_NOAPI(ret_value);
}
//----------------------------------------------------------------------------
herr_t H5FD_dsm_begin_loop(const char *name)
{
  herr_t ret_value = SUCCEED;
  H5FDdsmBuffer *dsmBuffer;
  FUNC_ENTER_NOAPI(H5FD_dsm_begin_loop, FAIL)

  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("Attempting to use the DSM Steering library before calling H5FD_dsm_steering_init", FAIL)
  }

  dsmBuffer = (H5FDdsmBuffer *)dsm_buffer;
  // start HTM loop section
//  if (dsmBuffer) fprintf(stderr, "dsmBuffer is not NULL\n");
//  if (dsmBuffer && dsmBuffer->GetSteerer()) {
//    fprintf(stderr, "dsmBuffer steerer is not NULL\n");
//  }

done:
  FUNC_LEAVE_NOAPI(ret_value);
}
//----------------------------------------------------------------------------
herr_t H5FD_dsm_end_loop(const char *name)
{
  herr_t ret_value = SUCCEED;
  FUNC_ENTER_NOAPI(H5FD_dsm_end_loop, FAIL)

  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("Attempting to use the DSM Steering library before calling H5FD_dsm_steering_init", FAIL)
  }

  // finish to build - close the HTM loop section
  // for later

done:
  FUNC_LEAVE_NOAPI(ret_value);
}
//----------------------------------------------------------------------------
herr_t H5FD_dsm_is_steerable(const char *name)
{
  H5FDdsmBuffer *dsmBuffer;
  herr_t ret_value = SUCCEED;
  FUNC_ENTER_NOAPI(H5FD_dsm_is_steerable, FAIL)

  if (!dsm_buffer) {
    DSM_STEERING_GOTO_ERROR("Attempting to use the DSM Steering library before calling H5FD_dsm_steering_init", FAIL)
  }

  dsmBuffer = (H5FDdsmBuffer *)dsm_buffer;
  if (!dsmBuffer->GetSteerer()->IsSteerable(name)) ret_value = FAIL;

done:
  FUNC_LEAVE_NOAPI(ret_value);
}
//----------------------------------------------------------------------------
