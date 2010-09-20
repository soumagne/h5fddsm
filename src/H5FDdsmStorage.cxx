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

#include "H5FDdsmStorage.h"

#include "H5FDdsmComm.h"

#include <cstdio>
#include <cstdlib>
#include <mpi.h>

//----------------------------------------------------------------------------
H5FDdsmStorage::H5FDdsmStorage()
{
  this->DataPointer = NULL;
  this->Comm = NULL;
}
//----------------------------------------------------------------------------
H5FDdsmStorage::~H5FDdsmStorage()
{
  if (this->DataPointer) {
    if (this->Comm && (this->Comm->GetCommType() == H5FD_DSM_COMM_MPI_RMA)) {
      MPI_Free_mem(this->DataPointer);
    } else {
      free(this->DataPointer);
    }
    this->DataPointer = NULL;
  }
}
//----------------------------------------------------------------------------
H5FDdsmPointer H5FDdsmStorage::GetDataPointer(H5FDdsmInt64 Index)
{
  H5FDdsmByte  *pointer;
  pointer = (H5FDdsmByte*) this->DataPointer;
  pointer += sizeof(H5FDdsmInt64) * Index;
  return((H5FDdsmPointer)pointer);
}
//----------------------------------------------------------------------------
H5FDdsmInt32  H5FDdsmStorage::SetNumberOfElements(H5FDdsmInt64 Length, H5FDdsmBoolean AllowAllocate)
{
  this->NumberOfElements = Length;
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
  if (this->Comm && (this->Comm->GetCommType() == H5FD_DSM_COMM_MPI_RMA)) {
    int err;
    if (this->DataPointer) {
      MPI_Free_mem(this->DataPointer);
      this->DataPointer = NULL;
    }
    err = MPI_Alloc_mem(this->NumberOfElements*sizeof(H5FDdsmInt8), MPI_INFO_NULL, &this->DataPointer);
    if ((this->DataPointer == NULL) || err) {
      int errclass;
      // An error of MPI_ERR_NO_MEM is allowed
      MPI_Error_class(err, &errclass);
      if (errclass == MPI_ERR_NO_MEM) {
        H5FDdsmError("MPI_Alloc_mem failed, not enough memory");
      }
      H5FDdsmError("Allocation Failed, unable to allocate "
          << this->NumberOfElements);
      return(H5FD_DSM_FAIL);
    }
  } else {
    if (this->DataPointer) {
      // try to reallocate
      this->DataPointer = realloc(this->DataPointer,
          this->NumberOfElements*sizeof(H5FDdsmInt8));
    } else {
      this->DataPointer = calloc(this->NumberOfElements, sizeof(H5FDdsmInt8));
    }
    if (this->DataPointer == NULL) {
      H5FDdsmError("Allocation Failed, unable to allocate "
          << this->NumberOfElements);
      perror("Alloc :" );
      return(H5FD_DSM_FAIL);
    }
  }
  H5FDdsmDebug("Allocation Succeeded");
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
