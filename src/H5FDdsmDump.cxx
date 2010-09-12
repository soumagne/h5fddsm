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
#include "h5dump.h"

#include "H5FDdsmBuffer.h"
#include "H5FDdsmDump.h"

//----------------------------------------------------------------------------
H5FDdsmDump::H5FDdsmDump()
{
    this->DsmBuffer = NULL;
    this->FileName = NULL;
}
//----------------------------------------------------------------------------
H5FDdsmDump::~H5FDdsmDump()
{
    this->SetFileName(NULL);
}
//----------------------------------------------------------------------------
void
H5FDdsmDump::SetDsmBuffer(H5FDdsmBuffer *dsmBuffer)
{
    this->DsmBuffer = dsmBuffer;
}
//----------------------------------------------------------------------------
void
H5FDdsmDump::Dump()
{
    H5dump_dsm(this->FileName, this->DsmBuffer);
}
//----------------------------------------------------------------------------
void
H5FDdsmDump::DumpLight()
{
    H5dump_dsm_light(this->FileName, this->DsmBuffer);
}
//----------------------------------------------------------------------------
void
H5FDdsmDump::DumpXML(std::ostringstream &stream)
{
  if (this->DsmBuffer) {
    H5dump_dsm_xml(this->FileName, stream, this->DsmBuffer);
  } else {
    H5dump_xml(this->FileName, stream);
  }
}
//----------------------------------------------------------------------------
