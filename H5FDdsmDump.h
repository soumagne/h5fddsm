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

class H5FDdsmBuffer;

#include <cstring>
#include <sstream>

class H5FDdsm_EXPORT H5FDdsmDump : public H5FDdsmObject 
{
  public:
    H5FDdsmDump();
   ~H5FDdsmDump();

    H5FDdsmSetStringMacro(FileName);
    H5FDdsmGetStringMacro(FileName);

    void Dump();
    void DumpLight();
    void DumpXML(std::ostringstream &);

    void SetDsmBuffer(H5FDdsmBuffer* _arg);

  protected:
    H5FDdsmBuffer *DsmBuffer;
    H5FDdsmString  FileName;
};

#endif // __H5FDdsmDump_h
