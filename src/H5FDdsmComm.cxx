/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmComm.cxx

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

/*=========================================================================
  This code is derived from an earlier work and is distributed
  with permission from, and thanks to ...
=========================================================================*/

/*******************************************************************/
/*                               XDMF                              */
/*                   eXtensible Data Model and Format              */
/*                                                                 */
/*                                                                 */
/*  Author:                                                        */
/*     Jerry A. Clarke                                             */
/*     clarke@arl.army.mil                                         */
/*     US Army Research Laboratory                                 */
/*     Aberdeen Proving Ground, MD                                 */
/*                                                                 */
/*     Copyright @ 2007 US Army Research Laboratory                */
/*     All Rights Reserved                                         */
/*     See Copyright.txt or http://www.arl.hpc.mil/ice for details */
/*                                                                 */
/*     This software is distributed WITHOUT ANY WARRANTY; without  */
/*     even the implied warranty of MERCHANTABILITY or FITNESS     */
/*     FOR A PARTICULAR PURPOSE.  See the above copyright notice   */
/*     for more information.                                       */
/*                                                                 */
/*******************************************************************/
#include "H5FDdsmComm.h"
#include "H5FDdsmMsg.h"

#include <queue>


//------------------------------------------------------------------------------
// Utility function to return a string for a given tag
//------------------------------------------------------------------------------
#define H5FD_DSM_COMM_MACRO(def, value) \
  if (value==def) return #def;

H5FDdsmConstString H5FDdsmCommToString(H5FDdsmInt32 comm) {
  H5FD_DSM_COMM_MACRO(H5FD_DSM_ANY_COMM, comm)
  H5FD_DSM_COMM_MACRO(H5FD_DSM_INTRA_COMM, comm)
  H5FD_DSM_COMM_MACRO(H5FD_DSM_INTER_COMM, comm)
  return "COMM UNDEFINED/UNRECOGNIZED";
}

//----------------------------------------------------------------------------
H5FDdsmComm::H5FDdsmComm()
{
  this->UseOneSidedComm    = H5FD_DSM_FALSE;
  //
  this->IntraComm          = MPI_COMM_NULL;
  this->IntraWin           = MPI_WIN_NULL;
  this->Id                 = -1;
  this->IntraSize          = -1;
  //
  this->UseStaticInterComm = H5FD_DSM_FALSE;
  this->InterCommType      = -1;
  this->InterSize          = -1;
  //
  this->SyncChannels       = 0;
}

//----------------------------------------------------------------------------
H5FDdsmComm::~H5FDdsmComm()
{
  if (this->IntraComm != MPI_COMM_NULL) this->CommFree();
  this->IntraComm = MPI_COMM_NULL;
  if (this->IntraWin != MPI_WIN_NULL) this->WinFreeData();
  this->IntraWin = MPI_WIN_NULL;
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::DupComm(MPI_Comm source)
{
  H5FDdsmInt32 status;

  if (this->IntraComm != MPI_COMM_NULL) this->CommFree();
  status = MPI_Comm_dup(source, &this->IntraComm);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Comm_dup failed");
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::CommFree()
{
  H5FDdsmInt32 status;

  status = MPI_Comm_free(&this->IntraComm);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Comm_free failed");
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Init()
{
  H5FDdsmInt32 size, rank;

  if (MPI_Comm_size(this->IntraComm, &size) != MPI_SUCCESS) return(H5FD_DSM_FAIL);
  if (MPI_Comm_rank(this->IntraComm, &rank) != MPI_SUCCESS) return(H5FD_DSM_FAIL);

  this->Id = rank;
  this->IntraSize = size;
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Send(H5FDdsmMsg *msg)
{
  if (msg->Tag < 0) msg->Tag = H5FD_DSM_DEFAULT_TAG;
  if (msg->Length <= 0 ) {
    H5FDdsmError("Cannot Send Message of Length = " << msg->Length);
    return(H5FD_DSM_FAIL);
  }
  if (msg->Data <= 0 ) {
    H5FDdsmError("Cannot Send Message from Data Buffer = " << msg->Length);
    return(H5FD_DSM_FAIL);
  }

  H5FDdsmDebugLevel(3, "(" << this->Id << ") Sending " << msg->Length << " Bytes on "
      << H5FDdsmCommToString(msg->Communicator) << " to " << msg->Dest <<
      " Tag = " << H5FDdsmTagToString(msg->Tag));

  if (msg->Communicator == H5FD_DSM_INTRA_COMM) {
    H5FDdsmInt32 status;

    status = MPI_Send(msg->Data, msg->Length, MPI_UNSIGNED_CHAR, msg->Dest,
        msg->Tag, this->IntraComm);

    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Send failed to send " << msg->Length
          << " Bytes to " << msg->Dest);
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebugLevel(3, "(" << this->Id << ") Sent " << msg->Length << " Bytes on "
        << H5FDdsmCommToString(msg->Communicator) << " to " << msg->Dest);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Receive(H5FDdsmMsg *msg)
{
  if (msg->Source < 0) msg->Source = MPI_ANY_SOURCE;
  if (msg->Tag < 0) msg->Tag = MPI_ANY_TAG;
  if (msg->Length <= 0) {
    H5FDdsmError("Cannot Receive Message of Length = " << msg->Length);
    return(H5FD_DSM_FAIL);
  }
  if (msg->Data <= 0) {
    H5FDdsmError("Cannot Receive Message into Data Buffer = " << msg->Length);
    return(H5FD_DSM_FAIL);
  }

  H5FDdsmDebugLevel(3, "(" << this->Id << ") Receiving " << msg->Length << " Bytes on "
      << H5FDdsmCommToString(msg->Communicator) << " from " << msg->Source <<
      " Tag = " << H5FDdsmTagToString(msg->Tag));

  if (msg->Communicator == H5FD_DSM_INTRA_COMM) {
    int            message_length;
    H5FDdsmInt32   status;
    MPI_Status     mpi_status;

    status = MPI_Recv(msg->Data, msg->Length, MPI_UNSIGNED_CHAR, msg->Source, msg->Tag,
        this->IntraComm, &mpi_status);

    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Recv failed to receive " << msg->Length
          << " Bytes from " << msg->Source);
      H5FDdsmError("MPI Error Code = " << mpi_status.MPI_ERROR);
      return(H5FD_DSM_FAIL);
    }

    status = MPI_Get_count(&mpi_status, MPI_UNSIGNED_CHAR, &message_length);
    H5FDdsmDebugLevel(3, "(" << this->Id << ") Received " << message_length << " Bytes on "
        << H5FDdsmCommToString(msg->Communicator) << " from " << mpi_status.MPI_SOURCE);
    msg->SetSource(mpi_status.MPI_SOURCE);
    msg->SetLength(message_length);

    if (status != MPI_SUCCESS) {
      H5FDdsmError("MPI_Get_count failed");
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Probe(H5FDdsmMsg *msg)
{
  if (msg->Source < 0) msg->Source = MPI_ANY_SOURCE;
  if (msg->Tag < 0) msg->Tag = MPI_ANY_TAG;

  H5FDdsmDebugLevel(3, "(" << this->Id << ") Probing on "
      << H5FDdsmCommToString(msg->Communicator) << " from " << msg->Source <<
      " Tag = " << H5FDdsmTagToString(msg->Tag));

  if (msg->Communicator == H5FD_DSM_INTRA_COMM) {
    int            flag;
    H5FDdsmInt32   status;
    MPI_Status     mpi_status;

    status = MPI_Iprobe(msg->Source, msg->Tag, this->IntraComm, &flag, &mpi_status);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Iprobe failed to probe from "
          << msg->Source << " Tag = " << H5FDdsmTagToString(msg->Tag));
      H5FDdsmError("MPI Error Code = " << mpi_status.MPI_ERROR);
      return(H5FD_DSM_FAIL);
    }

    if (flag) {
      msg->SetSource(mpi_status.MPI_SOURCE);
      msg->SetCommunicator(H5FD_DSM_INTRA_COMM);
      H5FDdsmDebugLevel(3, "MPI_Iprobe found pending messages on " << H5FDdsmCommToString(msg->Communicator)
          << " from " << msg->Source << " Tag = " << H5FDdsmTagToString(msg->Tag));
    } else {
      H5FDdsmDebugLevel(5, "MPI_Iprobe did not find pending messages on " << H5FDdsmCommToString(msg->Communicator)
          << " from " << msg->Source << " Tag = " << H5FDdsmTagToString(msg->Tag));
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::WinCreateData(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm)
{
  if (comm == H5FD_DSM_INTRA_COMM) {
    H5FDdsmInt32 status;

    if (this->IntraWin != MPI_WIN_NULL) this->WinFreeData();
    status = MPI_Win_create(storagePointer, storageSize * sizeof(H5FDdsmByte),
        sizeof(H5FDdsmByte), MPI_INFO_NULL, this->IntraComm, &this->IntraWin);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Win_create failed");
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::WinFreeData()
{
  H5FDdsmInt32 status;

  status = MPI_Win_free(&this->IntraWin);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Win_free failed");
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::PutData(H5FDdsmMsg *msg)
{
  if (msg->Length <= 0) {
    H5FDdsmError("Cannot Put Message of Length = " << msg->Length);
    return(H5FD_DSM_FAIL);
  }
  if (msg->Data <= 0) {
    H5FDdsmError("Cannot Put Message from Data Buffer = " << msg->Length);
    return(H5FD_DSM_FAIL);
  }

  H5FDdsmDebugLevel(3, "(" << this->Id << ") Putting " << msg->Length << " Bytes to " << msg->Dest
      << " at address " << msg->Address);

  if (msg->Communicator == H5FD_DSM_INTRA_COMM) {
    if (this->IntraWin != MPI_WIN_NULL) {
      H5FDdsmInt32   status;

      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, msg->Dest, 0, this->IntraWin);

      status = MPI_Put(msg->Data, msg->Length, MPI_UNSIGNED_CHAR,
          msg->Dest, msg->Address, msg->Length, MPI_UNSIGNED_CHAR, this->IntraWin);
      if (status != MPI_SUCCESS) {
        H5FDdsmError("Id = " << this->Id << " MPI_Put failed to put " <<
            msg->Length << " Bytes to " << msg->Dest << " at address " << msg->Address);
        return(H5FD_DSM_FAIL);
      }

      MPI_Win_unlock(msg->Dest, this->IntraWin);
      H5FDdsmDebugLevel(3, "(" << this->Id << ") Put " << msg->Length << " Bytes to " << msg->Dest
          << " at address " << msg->Address);
    } else {
      H5FDdsmError("IntraWin is MPI_WIN_NULL");
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::GetData(H5FDdsmMsg *msg)
{
  if (msg->Length <= 0) {
    H5FDdsmError("Cannot Get Message of Length = " << msg->Length);
    return(H5FD_DSM_FAIL);
  }
  if (msg->Data <= 0) {
    H5FDdsmError("Cannot Get Message into Data Buffer = " << msg->Length);
    return(H5FD_DSM_FAIL);
  }

  H5FDdsmDebugLevel(3, "(" << this->Id << ") Getting " << msg->Length << " Bytes from " << msg->Source
      << " at address " << msg->Address);

  if (msg->Communicator == H5FD_DSM_INTRA_COMM) {
    if (this->IntraWin != MPI_WIN_NULL) {
      H5FDdsmInt32 status;

      MPI_Win_lock(MPI_LOCK_SHARED, msg->Source, 0, this->IntraWin);

      status = MPI_Get(msg->Data, msg->Length, MPI_UNSIGNED_CHAR,
          msg->Source, msg->Address, msg->Length, MPI_UNSIGNED_CHAR, this->IntraWin);
      if (status != MPI_SUCCESS) {
        H5FDdsmError("Id = " << this->Id << " MPI_Get failed to get " <<
            msg->Length << " Bytes from " << msg->Source << " at address " << msg->Address);
        return(H5FD_DSM_FAIL);
      }

      MPI_Win_unlock(msg->Source, this->IntraWin);
      H5FDdsmDebugLevel(3, "(" << this->Id << ") Got " << msg->Length << " Bytes from " << msg->Dest
          << " at address " << msg->Address);
    } else {
      H5FDdsmError("IntraWin is MPI_WIN_NULL");
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::WindowSyncData()
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::WindowSync()
{
  this->WindowSyncData();
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::WinCreateNotification(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm)
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::PutNotification(H5FDdsmMsg *msg)
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::GetNotification(H5FDdsmMsg *msg)
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::WinCreateLock(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm)
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::PutLock(H5FDdsmMsg *msg)
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::GetLock(H5FDdsmMsg *msg)
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Barrier(H5FDdsmInt32 comm)
{
  H5FDdsmDebugLevel(3, "(" << this->Id << ") Going into Barrier on " << H5FDdsmCommToString(comm));

  if (comm == H5FD_DSM_INTRA_COMM) {
    H5FDdsmInt32 status;

    status = MPI_Barrier(this->IntraComm);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Barrier on " << H5FDdsmCommToString(comm) << " failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebugLevel(3, "(" << this->Id << ") Left Barrier on " << H5FDdsmCommToString(comm));
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Barrier()
{
  this->Barrier(H5FD_DSM_INTRA_COMM);
  return(H5FD_DSM_SUCCESS);
}


//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Broadcast(H5FDdsmMsg *msg)
{
  H5FDdsmDebugLevel(3, "(" << this->Id << ") Broadcasting " << msg->Length << " Bytes on "
      << H5FDdsmCommToString(msg->Communicator) << " from " << msg->Source);

  if (msg->Communicator == H5FD_DSM_INTRA_COMM) {
    H5FDdsmInt32 status;

    status = MPI_Bcast(msg->Data, msg->Length, MPI_UNSIGNED_CHAR, msg->Source, this->IntraComm);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Bcast failed to bcast " << msg->Length
          << " Bytes from " << msg->Source);
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebugLevel(3, "(" << this->Id << ") Broadcasted " << msg->Length << " Bytes on "
        << H5FDdsmCommToString(msg->Communicator) << " from " << msg->Source);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::OpenPort()
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::ClosePort()
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Accept()
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Connect()
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Disconnect()
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::ChannelSynced(H5FDdsmInt32 who, H5FDdsmInt32 *syncId, H5FDdsmBoolean fromServer)
{
  static std::queue<H5FDdsmInt32> syncQueue;
  H5FDdsmInt32 ret = H5FD_DSM_FALSE;
  H5FDdsmInt32 numberOfChannels = (fromServer) ? this->IntraSize : this->InterSize;

  this->SyncChannels++;
  if (this->SyncChannels == numberOfChannels) {
    H5FDdsmDebugLevel(4, "Channels cleared: " << this->SyncChannels << "/" << numberOfChannels);
    this->SyncChannels = 0;
//    if (!this->UseOneSidedComm) {
      if (!syncQueue.empty()) {
        H5FDdsmDebugLevel(4, "(" << this->Id << ") " << "pop sync queue from " << who);
        if (syncQueue.front() != who) {
          H5FDdsmError("(" << this->Id << ") " << "Mismatched IDs in sync queue ");
        }
        syncQueue.pop();
        if (!syncQueue.empty()) {
          H5FDdsmError("(" << this->Id << ") " << "Sync queue should be empty!! " << syncQueue.front());
        }
      }
//    }
    *syncId = -1;
    ret = H5FD_DSM_TRUE;
  } else {
//    if (!this->UseOneSidedComm) {
      if (syncQueue.empty()) {
        H5FDdsmDebugLevel(4, "(" << this->Id << ") " << "filling sync queue from " << who);
        for (int i=0; i<numberOfChannels; i++) {
          if (i != who) syncQueue.push(i);
        }
      } else {
        H5FDdsmDebugLevel(4, "(" << this->Id << ") " << "pop sync queue from " << syncQueue.front());
        if (syncQueue.front() != who) {
          H5FDdsmError("(" << this->Id << ") " << "Mismatched IDs in sync queue ");
        }
        syncQueue.pop();
      }
      *syncId = syncQueue.front();
//    }
  }
  return(ret);
}
