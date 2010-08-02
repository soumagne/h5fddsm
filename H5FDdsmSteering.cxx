/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmStorage.h

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

#include "H5FDdsmSteering.h"

volatile int pause = 1;
//----------------------------------------------------------------------------
//H5FDdsmSteering::H5FDdsmSteering() {}
//----------------------------------------------------------------------------
//H5FDdsmSteering::~H5FDdsmSteering() {}
//----------------------------------------------------------------------------
//void H5FDdsmSteering::
void H5FD_dsm_begin_loop(const char *name)
{
  while (pause == 1) {
    // spin
    // look for new data coming in and new update of the value
  }
}
//----------------------------------------------------------------------------
//void H5FDdsmSteering::
void H5FD_dsm_end_loop(const char *name)
{
  // finish to build - close the HTM loop section
  // for later
}
//----------------------------------------------------------------------------

