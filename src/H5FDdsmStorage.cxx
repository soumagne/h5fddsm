/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmStorage.cxx

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

#include "H5FDdsmStorage.h"

#include <cstdlib>
#include <cstdio>

#ifndef _WIN32
#include <unistd.h>
#endif

//----------------------------------------------------------------------------
H5FDdsmStorage::H5FDdsmStorage()
{
  this->DataPointer = NULL;
  this->Length = 0;
}

//----------------------------------------------------------------------------
H5FDdsmStorage::~H5FDdsmStorage()
{
  this->Deallocate();
}

//----------------------------------------------------------------------------
H5FDdsmPointer H5FDdsmStorage::GetDataPointer(H5FDdsmAddr Addr)
{
  H5FDdsmByte  *pointer;
  pointer = (H5FDdsmByte*) this->DataPointer;
  pointer += sizeof(H5FDdsmAddr) * Addr;
  return((H5FDdsmPointer)pointer);
}

//----------------------------------------------------------------------------
H5FDdsmInt32  H5FDdsmStorage::SetLength(H5FDdsmUInt64 Length, H5FDdsmBoolean AllowAllocate)
{
  this->Length = Length;
  if (AllowAllocate) {
    if (this->Allocate() != H5FD_DSM_SUCCESS) {
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmStorage::Allocate()
{
  if (this->DataPointer) {
    // try to reallocate
    // this should not be called in most cases
    this->DataPointer = realloc(this->DataPointer, this->Length*sizeof(H5FDdsmByte));
  } else {
#ifdef _WIN32
    H5FDdsmDebug("Allocating memory with calloc");
    this->DataPointer = calloc(this->Length, sizeof(H5FDdsmByte));
#else
    H5FDdsmDebug("Allocating memory with alignment of " << getpagesize());
    posix_memalign(&this->DataPointer, getpagesize(), this->Length);
    memset(this->DataPointer, 0, this->Length);
#endif
  }
  if (this->DataPointer == NULL) {
    H5FDdsmError("Allocation Failed, unable to allocate " << this->Length);
    perror("Alloc :" );
    return(H5FD_DSM_FAIL);
  }
  H5FDdsmDebug("Allocation Succeeded");
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmStorage::Deallocate()
{
  if (this->DataPointer) {
    H5FDdsmDebug("Deallocating memory with free");
    free(this->DataPointer);
  }
  this->DataPointer = NULL;
  return(H5FD_DSM_SUCCESS);
}
