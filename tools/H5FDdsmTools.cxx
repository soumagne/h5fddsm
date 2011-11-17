/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmTools.cxx

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

#include "H5FDdsmTools.h"
#include "H5FDdsmDriver.h"
#include "H5FDdsmManager.h"
//
#include "H5Eprivate.h" // Error handling
#include "h5dump.h"

#define DSM_TOOLS_GOTO_ERROR(x, ret_val)                                \
{                                                                          \
   fprintf(stderr, "Error at %s %s:%d %s\n", __FILE__, FUNC, __LINE__, x); \
   err_occurred = TRUE;                                                    \
   if (err_occurred) { HGOTO_DONE(ret_val) }                               \
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
  const char *argv[5]={"./h5dump", "-f", "dsm", "-H", "dsm.h5"};
  std::ostringstream stream;
  H5FDdsmManager *dsmManager;

  FUNC_ENTER_NOAPI(H5FD_dsm_dump, FAIL)

  if (!dsm_get_manager())
    DSM_TOOLS_GOTO_ERROR("DSM buffer not initialized", FAIL);

  dsmManager = static_cast<H5FDdsmManager *> (dsm_get_manager());

  H5dump(5, (const char**) argv, stream);

  if (dsmManager->GetUpdatePiece() == 0) std::cout << stream.str() << std::endl;

done:
  if (err_occurred) {
    /* Nothing */
  }

  FUNC_LEAVE_NOAPI(ret_value);
}
