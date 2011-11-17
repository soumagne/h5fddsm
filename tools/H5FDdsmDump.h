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
#ifndef __H5FDdsmDump_h
#define __H5FDdsmDump_h

#include "H5FDdsmObject.h"
#include "hdf5.h"

class H5FDdsmManager;

#include <cstring>
#include <sstream>

class H5TOOLS_DLL H5FDdsmDump : public H5FDdsmObject
{
  public:
    H5FDdsmDump();
   ~H5FDdsmDump();

    H5FDdsmInt32 Dump();
    H5FDdsmInt32 DumpLight();
    H5FDdsmInt32 DumpXML(std::ostringstream &);

    void SetDsmManager(H5FDdsmManager* _arg);

  protected:
    H5FDdsmManager *DsmManager;
};

#endif // __H5FDdsmDump_h
