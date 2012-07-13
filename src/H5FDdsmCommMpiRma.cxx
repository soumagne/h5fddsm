/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmCommMpiRma.cxx

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

#include "H5FDdsmCommMpiRma.h"
#include "H5FDdsmMsg.h"

#include <cstring>

//----------------------------------------------------------------------------
H5FDdsmCommMpiRma::H5FDdsmCommMpiRma()
{
  this->InterCommType = H5FD_DSM_COMM_MPI_RMA;
  this->UseOneSidedComm = H5FD_DSM_TRUE;
  this->InterWin = MPI_WIN_NULL;
}

//----------------------------------------------------------------------------
H5FDdsmCommMpiRma::~H5FDdsmCommMpiRma()
{
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::Init()
{
  if (H5FDdsmCommMpi::Init() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  H5FDdsmDebugLevel(1, "CommMpiRma initialized");
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::Put(H5FDdsmMsg *dataMsg)
{
  if (H5FDdsmCommMpi::Put(dataMsg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if ((this->InterWin != MPI_WIN_NULL) && (dataMsg->Communicator == H5FD_DSM_INTER_COMM)) {
    H5FDdsmInt32   status;

    MPI_Win_lock(MPI_LOCK_EXCLUSIVE, dataMsg->Dest, 0, this->InterWin);

    status = MPI_Put(dataMsg->Data, dataMsg->Length, MPI_UNSIGNED_CHAR,
        dataMsg->Dest, dataMsg->Address, dataMsg->Length, MPI_UNSIGNED_CHAR, this->InterWin);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Put failed to put " <<
          dataMsg->Length << " Bytes to " << dataMsg->Dest << " at address " << dataMsg->Address);
      return(H5FD_DSM_FAIL);
    }

    MPI_Win_unlock(dataMsg->Dest, this->InterWin);
    H5FDdsmDebugLevel(3, "(" << this->Id << ") Put " << dataMsg->Length << " Bytes to " << dataMsg->Dest
        << " at address " << dataMsg->Address);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::Get(H5FDdsmMsg *dataMsg)
{
  if (H5FDdsmCommMpi::Get(dataMsg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if ((this->InterWin != MPI_WIN_NULL) && (dataMsg->Communicator == H5FD_DSM_INTER_COMM)) {
    H5FDdsmInt32 status;

    MPI_Win_lock(MPI_LOCK_SHARED, dataMsg->Source, 0, this->InterWin);

    status = MPI_Get(dataMsg->Data, dataMsg->Length, MPI_UNSIGNED_CHAR,
        dataMsg->Source, dataMsg->Address, dataMsg->Length, MPI_UNSIGNED_CHAR, this->InterWin);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Get failed to get " <<
          dataMsg->Length << " Bytes from " << dataMsg->Source << " at address " << dataMsg->Address);
      return(H5FD_DSM_FAIL);
    }

    MPI_Win_unlock(dataMsg->Source, this->InterWin);
    H5FDdsmDebugLevel(3, "(" << this->Id << ") Got " << dataMsg->Length << " Bytes from " << dataMsg->Dest
        << " at address " << dataMsg->Address);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::Accept(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize)
{
  MPI_Comm winComm;

  if (H5FDdsmCommMpi::Accept(storagePointer, storageSize) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  MPI_Intercomm_merge(this->InterComm, 0, &winComm);

  if (MPI_Win_create(storagePointer, storageSize*sizeof(H5FDdsmByte),
      sizeof(H5FDdsmByte), MPI_INFO_NULL, winComm, &this->InterWin) != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Win_create failed");
    return(H5FD_DSM_FAIL);
  }
  MPI_Comm_free(&winComm);
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::Connect()
{
  MPI_Comm winComm;

  if (H5FDdsmCommMpi::Connect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  MPI_Intercomm_merge(this->InterComm, 1, &winComm);

  if (MPI_Win_create(NULL, 0, sizeof(H5FDdsmInt8), MPI_INFO_NULL, winComm,
      &this->InterWin) != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Win_create failed");
    return(H5FD_DSM_FAIL);
  }
  MPI_Comm_free(&winComm);
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::Disconnect()
{
  if (H5FDdsmCommMpi::Disconnect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->InterWin != MPI_WIN_NULL) {
    MPI_Win_free(&this->InterWin);
    this->InterWin = MPI_WIN_NULL;
  }
  return(H5FD_DSM_SUCCESS);
}
