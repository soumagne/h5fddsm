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
  this->NotificationWin = MPI_WIN_NULL;
  this->LockWin = MPI_WIN_NULL;
}

//----------------------------------------------------------------------------
H5FDdsmCommMpiRma::~H5FDdsmCommMpiRma()
{
  if (this->InterWin != MPI_WIN_NULL) this->MpiWinFree(&this->InterWin);
  this->InterWin = MPI_WIN_NULL;
  if (this->NotificationWin != MPI_WIN_NULL) this->MpiWinFree(&this->NotificationWin);
  this->NotificationWin = MPI_WIN_NULL;
  if (this->LockWin != MPI_WIN_NULL) this->MpiWinFree(&this->LockWin);
  this->LockWin = MPI_WIN_NULL;
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
H5FDdsmCommMpiRma::WinCreateData(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm)
{
  if (H5FDdsmComm::WinCreateData(storagePointer, storageSize, comm) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (comm == H5FD_DSM_INTER_COMM) {
    if (this->InterWin != MPI_WIN_NULL) this->MpiWinFree(&this->InterWin);
    if (this->MpiWinCreateRemote(storagePointer, storageSize, &this->InterWin) != H5FD_DSM_SUCCESS) {
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::PutData(H5FDdsmMsg *msg)
{
  if (H5FDdsmComm::PutData(msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (msg->Communicator == H5FD_DSM_INTER_COMM) {
    if (this->InterWin != MPI_WIN_NULL) {
      if (this->MpiPut(msg, this->InterWin) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
    } else {
      H5FDdsmError("InterWin is MPI_WIN_NULL");
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::GetData(H5FDdsmMsg *msg)
{
  if (H5FDdsmComm::GetData(msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (msg->Communicator == H5FD_DSM_INTER_COMM) {
    if (this->InterWin != MPI_WIN_NULL) {
      if (this->MpiGet(msg, this->InterWin) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
    } else {
      H5FDdsmError("InterWin is MPI_WIN_NULL");
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::WinCreateNotification(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm)
{
  if (H5FDdsmComm::WinCreateData(storagePointer, storageSize, comm) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (comm == H5FD_DSM_INTER_COMM) {
    if (this->NotificationWin != MPI_WIN_NULL) this->MpiWinFree(&this->NotificationWin);
    if (this->MpiWinCreateRemote(storagePointer, storageSize, &this->NotificationWin) != H5FD_DSM_SUCCESS) {
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::PutNotification(H5FDdsmMsg *msg)
{
  if (msg->Communicator == H5FD_DSM_INTER_COMM) {
    if (this->NotificationWin != MPI_WIN_NULL) {
      if (this->MpiPut(msg, this->NotificationWin) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
    } else {
      H5FDdsmError("NotificationWin is MPI_WIN_NULL");
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::GetNotification(H5FDdsmMsg *msg)
{
  if (msg->Communicator == H5FD_DSM_INTER_COMM) {
    if (this->NotificationWin != MPI_WIN_NULL) {
      if (this->MpiGet(msg, this->NotificationWin) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
    } else {
      H5FDdsmError("NotificationWin is MPI_WIN_NULL");
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::WinCreateLock(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm)
{
  if (H5FDdsmComm::WinCreateLock(storagePointer, storageSize, comm) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (comm == H5FD_DSM_INTER_COMM) {
    if (this->LockWin != MPI_WIN_NULL) this->MpiWinFree(&this->LockWin);
    if (this->MpiWinCreateRemote(storagePointer, storageSize, &this->LockWin) != H5FD_DSM_SUCCESS) {
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::PutLock(H5FDdsmMsg *msg)
{
  if (msg->Communicator == H5FD_DSM_INTER_COMM) {
    if (this->LockWin != MPI_WIN_NULL) {
      if (this->MpiPut(msg, this->LockWin) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
    } else {
      H5FDdsmError("LockWin is MPI_WIN_NULL");
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::GetLock(H5FDdsmMsg *msg)
{
  if (msg->Communicator == H5FD_DSM_INTER_COMM) {
    if (this->LockWin != MPI_WIN_NULL) {
      if (this->MpiGet(msg, this->LockWin) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
    } else {
      H5FDdsmError("LockWin is MPI_WIN_NULL");
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::MpiWinCreateRemote(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, MPI_Win *win)
{
  H5FDdsmBoolean isClient;
  MPI_Comm     winComm;
  H5FDdsmInt32 status;

  // If NULL we don't host the DSM so use this result for Intercomm ordering
  isClient = (storagePointer == NULL) ? H5FD_DSM_TRUE : H5FD_DSM_FALSE;
  MPI_Intercomm_merge(this->InterComm, isClient, &winComm);

  status = MPI_Win_create(storagePointer, storageSize * sizeof(H5FDdsmByte),
      sizeof(H5FDdsmByte), MPI_INFO_NULL, winComm, win);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Win_create failed");
    return(H5FD_DSM_FAIL);
  }

  MPI_Comm_free(&winComm);
  return(H5FD_DSM_SUCCESS);
}
