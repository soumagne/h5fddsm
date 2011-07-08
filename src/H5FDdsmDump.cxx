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
  const char *argv[4]={"./h5dump", "-f", "dsm", this->FileName};
  int print_rank;
  std::ostringstream stream;
  H5dump(4, (const char**) argv, stream, this->DsmBuffer);
  MPI_Comm_rank(MPI_COMM_WORLD, &print_rank);
  if (print_rank == 0) std::cout << stream.str() << std::endl;
}
//----------------------------------------------------------------------------
void
H5FDdsmDump::DumpLight()
{
  const char *argv[5]={"./h5dump", "-f", "dsm", "-H", this->FileName};
  int print_rank;
  std::ostringstream stream;
  H5dump(5, (const char**) argv, stream, this->DsmBuffer);
  MPI_Comm_rank(MPI_COMM_WORLD, &print_rank);
  if (print_rank == 0) std::cout << stream.str() << std::endl;
}
//----------------------------------------------------------------------------
void
H5FDdsmDump::DumpXML(std::ostringstream &stream)
{
  if (this->DsmBuffer) {
    const char *argv[8] = {"./h5dump", "-f", "dsm", "-x", "-X", ":", "-H", this->FileName};
    H5dump(8, (const char**) argv, stream, this->DsmBuffer);
  } else {
    const char *argv[6] = {"./h5dump", "-x", "-X", ":", "-H", this->FileName};
    H5dump(6, (const char**) argv, stream, NULL);
  }
}
//----------------------------------------------------------------------------
