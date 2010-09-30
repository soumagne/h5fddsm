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
//
#include "H5FDdsm.h"
#include "H5FDdsmSteering.h"
#include "H5FDdsmSteerer.h"
#include "H5FDdsmBuffer.h"

//----------------------------------------------------------------------------
// C steering bindings

void *dsm_buffer = NULL; // pointer to internal dsm buffer reference
//----------------------------------------------------------------------------
herr_t H5FD_dsm_steering_init(MPI_Comm comm)
{
  MPI_Comm retComm;
  hid_t hdf5_fapl;

  hdf5_fapl = H5Pcreate(H5P_FILE_ACCESS);
  H5Pset_fapl_dsm(hdf5_fapl, comm, NULL);
  H5Pget_fapl_dsm(hdf5_fapl, &retComm, &dsm_buffer);
  H5Pclose(hdf5_fapl);

  return(SUCCEED);
}
//----------------------------------------------------------------------------
herr_t H5FD_dsm_begin_loop(const char *name)
{
  H5FDdsmBuffer *dsmBuffer = (H5FDdsmBuffer *)dsm_buffer;
  // start HTM loop section
  if (dsmBuffer) std::cerr << "dsmBuffer is not NULL" << std::endl;
  if (dsmBuffer && dsmBuffer->GetSteerer()) {
    std::cerr << "dsmBuffer steerer is not NULL" << std::endl;
  }

  return(SUCCEED);
}
//----------------------------------------------------------------------------
herr_t H5FD_dsm_end_loop(const char *name)
{
  // finish to build - close the HTM loop section
  // for later
  return(SUCCEED);
}
//----------------------------------------------------------------------------
