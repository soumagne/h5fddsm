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
  if (this->InterWin != MPI_WIN_NULL) this->WinFree(&this->InterWin);
  this->InterWin = MPI_WIN_NULL;
  if (this->NotificationWin != MPI_WIN_NULL) this->WinFree(&this->NotificationWin);
  this->NotificationWin = MPI_WIN_NULL;
  if (this->LockWin != MPI_WIN_NULL) this->WinFree(&this->LockWin);
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
    if (this->InterWin != MPI_WIN_NULL) this->WinFree(&this->InterWin);
    if (this->WinCreate(storagePointer, storageSize, &this->InterWin) != H5FD_DSM_SUCCESS) {
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
      if (this->Put(msg, this->InterWin) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
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
      if (this->Get(msg, this->InterWin) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
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
    if (this->NotificationWin != MPI_WIN_NULL) this->WinFree(&this->NotificationWin);
    if (this->WinCreate(storagePointer, storageSize, &this->NotificationWin) != H5FD_DSM_SUCCESS) {
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
      if (this->Put(msg, this->NotificationWin) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
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
      if (this->Get(msg, this->NotificationWin) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
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
    if (this->LockWin != MPI_WIN_NULL) this->WinFree(&this->LockWin);
    if (this->WinCreate(storagePointer, storageSize, &this->LockWin) != H5FD_DSM_SUCCESS) {
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
      if (this->Put(msg, this->LockWin) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
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
      if (this->Get(msg, this->LockWin) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
    } else {
      H5FDdsmError("LockWin is MPI_WIN_NULL");
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::WinCreate(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, MPI_Win *win)
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

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::WinFree(MPI_Win *win)
{
  H5FDdsmInt32 status;

  status = MPI_Win_free(win);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Win_free failed");
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::Put(H5FDdsmMsg *msg, MPI_Win win)
{
  H5FDdsmInt32   status;

  MPI_Win_lock(MPI_LOCK_EXCLUSIVE, msg->Dest, 0, win);

  status = MPI_Put(msg->Data, msg->Length, MPI_UNSIGNED_CHAR,
      msg->Dest, msg->Address, msg->Length, MPI_UNSIGNED_CHAR, win);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Put failed to put " <<
        msg->Length << " Bytes to " << msg->Dest << " at address " << msg->Address);
    return(H5FD_DSM_FAIL);
  }

  MPI_Win_unlock(msg->Dest, win);
  H5FDdsmDebugLevel(3, "(" << this->Id << ") Put " << msg->Length << " Bytes to " << msg->Dest
      << " at address " << msg->Address);

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpiRma::Get(H5FDdsmMsg *msg, MPI_Win win)
{
  H5FDdsmInt32 status;

  MPI_Win_lock(MPI_LOCK_SHARED, msg->Source, 0, win);

  status = MPI_Get(msg->Data, msg->Length, MPI_UNSIGNED_CHAR,
      msg->Source, msg->Address, msg->Length, MPI_UNSIGNED_CHAR, win);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Get failed to get " <<
        msg->Length << " Bytes from " << msg->Source << " at address " << msg->Address);
    return(H5FD_DSM_FAIL);
  }

  MPI_Win_unlock(msg->Source, win);
  H5FDdsmDebugLevel(3, "(" << this->Id << ") Got " << msg->Length << " Bytes from " << msg->Dest
      << " at address " << msg->Address);

  return(H5FD_DSM_SUCCESS);
}
