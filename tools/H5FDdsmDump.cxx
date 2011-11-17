/*=========================================================================

  Project                 : vtkCSCS
  Module                  : H5FDdsmDump.cxx

  Copyright (C) CSCS - Swiss National Supercomputing Centre.
  You may use modify and and distribute this code freely providing
  1) This copyright notice appears on all copies of source code
  2) An acknowledgment appears with any substantial usage of the code
  3) If this code is contributed to any other open source project, it
  must not be reformatted such that the indentation, bracketing or
  overall style is modified significantly.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=========================================================================*/
#include "H5FDdsmDump.h"
#include "H5FDdsmManager.h"
#include "H5FDdsmDriver.h"

#include "h5dump.h"
//----------------------------------------------------------------------------
H5FDdsmDump::H5FDdsmDump()
{
  this->DsmManager = NULL;
}
//----------------------------------------------------------------------------
H5FDdsmDump::~H5FDdsmDump()
{
}
//----------------------------------------------------------------------------
void
H5FDdsmDump::SetDsmManager(H5FDdsmManager *dsmManager)
{
  this->DsmManager = dsmManager;
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDump::Dump()
{
  const char *argv[4]={"./h5dump", "-f", "dsm", "dsm.h5"};
  std::ostringstream stream;
  dsm_set_manager(this->DsmManager);
  H5dump(4, (const char**) argv, stream);
  if (this->DsmManager->GetUpdatePiece() == 0) std::cout << stream.str() << std::endl;
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDump::DumpLight()
{
  const char *argv[5]={"./h5dump", "-f", "dsm", "-H", "dsm.h5"};
  std::ostringstream stream;
  dsm_set_manager(this->DsmManager);
  H5dump(5, (const char**) argv, stream);
  if (this->DsmManager->GetUpdatePiece() == 0) std::cout << stream.str() << std::endl;
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDump::DumpXML(std::ostringstream &stream)
{
  if (this->DsmManager) {
    const char *argv[8] = {"./h5dump", "-f", "dsm", "-x", "-X", ":", "-H", "dsm.h5"};
    dsm_set_manager(this->DsmManager);
    H5dump(8, (const char**) argv, stream);
  } else {
    const char *argv[6] = {"./h5dump", "-x", "-X", ":", "-H", "dsm.h5"};
    H5dump(6, (const char**) argv, stream);
  }
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
