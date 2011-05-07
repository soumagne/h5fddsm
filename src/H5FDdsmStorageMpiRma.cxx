/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmStorageMpiRma.cxx

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

#include "H5FDdsmStorageMpiRma.h"

#include <mpi.h>

//----------------------------------------------------------------------------
H5FDdsmStorageMpiRma::H5FDdsmStorageMpiRma() {}

//----------------------------------------------------------------------------
H5FDdsmStorageMpiRma::~H5FDdsmStorageMpiRma()
{
  this->Deallocate();
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmStorageMpiRma::Allocate()
{
  int err;
  if (this->DataPointer) {
    MPI_Free_mem(this->DataPointer);
    this->DataPointer = NULL;
  }
  H5FDdsmDebug("Allocating memory with MPI_Alloc_mem");
  err = MPI_Alloc_mem(this->Length*sizeof(H5FDdsmByte), MPI_INFO_NULL, &this->DataPointer);
  if ((this->DataPointer == NULL) || err) {
    int errclass;
    // An error of MPI_ERR_NO_MEM is allowed
    MPI_Error_class(err, &errclass);
    if (errclass == MPI_ERR_NO_MEM) {
      H5FDdsmError("MPI_Alloc_mem failed, not enough memory");
    }
    H5FDdsmError("Allocation Failed, unable to allocate " << this->Length);
    return(H5FD_DSM_FAIL);
  }
  H5FDdsmDebug("Allocation Succeeded");
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmStorageMpiRma::Deallocate()
{
  if (this->DataPointer) {
    H5FDdsmDebug("Deallocating memory with MPI_Free_mem");
    MPI_Free_mem(this->DataPointer);
  }
  this->DataPointer = NULL;
  return(H5FD_DSM_SUCCESS);
}
