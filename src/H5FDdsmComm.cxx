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
#include <set>

//----------------------------------------------------------------------------
// Declare extra debug info 
#undef H5FDdsmDebugLevel
#ifdef H5FDdsm_DEBUG_GLOBAL
#define H5FDdsmDebugLevel(level, x) \
{ if (this->DebugLevel >= level) { \
  std::cout << "H5FD_DSM Debug Level " << level << ": " << ("       ") << this->GetId() << " : " << x << std::endl; \
  } \
}
#else
#define H5FDdsmDebugLevel(level, x) \
{ if (this->Debug && this->DebugLevel >= level) { \
  std::cout << "H5FD_DSM Debug Level " << level << ": " << ("       ") << this->GetId() << " : " << x << std::endl; \
  } \
}
#endif
//----------------------------------------------------------------------------

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
  for (int i=0; i<H5FDdsm_NUM_CONNECTION_IDS; i++) {
    this->SyncCounter[i]       = 0;
  }
}

//----------------------------------------------------------------------------
H5FDdsmComm::~H5FDdsmComm()
{
  if (this->IntraComm != MPI_COMM_NULL) this->MpiCommFree(&this->IntraComm);
  this->IntraComm = MPI_COMM_NULL;
  if (this->IntraWin != MPI_WIN_NULL) this->MpiWinFree(&this->IntraWin);
  this->IntraWin = MPI_WIN_NULL;
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::DupComm(MPI_Comm source)
{
  H5FDdsmInt32 status;

  if (this->IntraComm != MPI_COMM_NULL) this->MpiCommFree(&this->IntraComm);
  status = MPI_Comm_dup(source, &this->IntraComm);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Comm_dup failed");
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

  H5FDdsmDebugLevel(3,"Sending " << msg->Length << " Bytes on "
      << H5FDdsmCommToString(msg->Communicator) << " to " << msg->Dest <<
      " Tag = " << H5FDdsmTagToString(msg->Tag));

  if (msg->Communicator == H5FD_DSM_INTRA_COMM) {
    if (this->MpiSend(msg, this->IntraComm) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
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

  H5FDdsmDebugLevel(3,"Receiving " << msg->Length << " Bytes on "
      << H5FDdsmCommToString(msg->Communicator) << " from " << msg->Source <<
      " Tag = " << H5FDdsmTagToString(msg->Tag));

  if (msg->Communicator == H5FD_DSM_INTRA_COMM) {
    if (this->MpiReceive(msg, this->IntraComm) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Probe(H5FDdsmMsg *msg)
{
  if (msg->Source < 0) msg->Source = MPI_ANY_SOURCE;
  if (msg->Tag < 0) msg->Tag = MPI_ANY_TAG;

  H5FDdsmDebugLevel(5,"Probing on "
      << H5FDdsmCommToString(msg->Communicator) << " from " << msg->Source <<
      " Tag = " << H5FDdsmTagToString(msg->Tag));

  if (msg->Communicator == H5FD_DSM_INTRA_COMM) {
    if (this->MpiProbe(msg, this->IntraComm) == H5FD_DSM_SUCCESS) return(H5FD_DSM_SUCCESS);
  }
  return(H5FD_DSM_FAIL);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::WinCreateData(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm)
{
  if (comm == H5FD_DSM_INTRA_COMM) {
    if (this->IntraWin != MPI_WIN_NULL) this->MpiWinFree(&this->IntraWin);
    if (this->MpiWinCreateLocal(storagePointer, storageSize, &this->IntraWin) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
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

  H5FDdsmDebugLevel(3,"Putting " << msg->Length << " Bytes to " << msg->Dest
      << " at address " << msg->Address);

  if (msg->Communicator == H5FD_DSM_INTRA_COMM) {
    if (this->IntraWin != MPI_WIN_NULL) {
      if (this->MpiPut(msg, this->IntraWin) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
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

  H5FDdsmDebugLevel(3,"Getting " << msg->Length << " Bytes from " << msg->Source
      << " at address " << msg->Address);

  if (msg->Communicator == H5FD_DSM_INTRA_COMM) {
    if (this->IntraWin != MPI_WIN_NULL) {
      if (this->MpiGet(msg, this->IntraWin) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
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
  H5FDdsmDebugLevel(3,"Going into Barrier on " << H5FDdsmCommToString(comm));

  if (comm == H5FD_DSM_INTRA_COMM) {
    if (this->MpiBarrier(this->IntraComm) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
    H5FDdsmDebugLevel(3,"Left Barrier on " << H5FDdsmCommToString(comm));
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
// Only for compatibility
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Barrier()
{
  return(this->Barrier(H5FD_DSM_INTRA_COMM));
}


//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Broadcast(H5FDdsmMsg *msg)
{
  H5FDdsmDebugLevel(3,"Broadcasting " << msg->Length << " Bytes on "
      << H5FDdsmCommToString(msg->Communicator) << " from " << msg->Source);

  if (msg->Communicator == H5FD_DSM_INTRA_COMM) {
    if (this->MpiBroadcast(msg, this->IntraComm) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
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
H5FDdsmComm::ChannelSynced(H5FDdsmInt32 who, H5FDdsmInt32 *syncId, H5FDdsmInt32 connectionId)
{
  static std::set<H5FDdsmInt32> syncQueue[H5FDdsm_NUM_CONNECTION_IDS];
  H5FDdsmInt32 ret = H5FD_DSM_FALSE;
  H5FDdsmInt32 numberOfRanks = (connectionId==0) ? this->IntraSize : this->InterSize;

  // on first entry, fill a set with the IDs we need, skip current one
  if (syncQueue[connectionId].empty()) {
    for (int i=0; i<numberOfRanks; i++) {
      if (i!=who) syncQueue[connectionId].insert(i);
    }
  }
  // otherwise remove current ID from set
  else {
    std::set<H5FDdsmInt32>::iterator loc = syncQueue[connectionId].find(who);
    if (loc==syncQueue[connectionId].end()) {
      H5FDdsmError("(" << this->Id << ") " << "Mismatched IDs in sync queue ");
    }
    else {
      syncQueue[connectionId].erase(loc);
    }
  }
  H5FDdsmDebugLevel(1, "ChannelSynced syncQueue[" << connectionId << "] : " << syncQueue[connectionId].size() << " / " << numberOfRanks);
  // if set is empty, we have cleared the queue
  if (syncQueue[connectionId].empty()) {
    H5FDdsmDebugLevel(1, "ChannelSynced syncQueue[" << connectionId << "] : Cleared " << syncQueue[connectionId].size() << " / " << numberOfRanks);
    ret = H5FD_DSM_TRUE;
    *syncId = -1;
  }
  else {
    // set next source tag to last one in queue so we will only receive messages from this rank.
    // if we don't do this, we need a separate set for each message that needs syncing
    *syncId = *syncQueue[connectionId].rbegin();
  }

  return (ret);
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::MpiCommFree(MPI_Comm *comm)
{
  H5FDdsmInt32 status;

  status = MPI_Comm_free(comm);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Comm_free failed");
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::MpiSend(H5FDdsmMsg *msg, MPI_Comm comm)
{
  H5FDdsmInt32 status;

  status = MPI_Send(msg->Data, msg->Length, MPI_UNSIGNED_CHAR, msg->Dest,
      msg->Tag, comm);

  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Send failed to send " << msg->Length
        << " Bytes to " << msg->Dest);
    return(H5FD_DSM_FAIL);
  }
  H5FDdsmDebugLevel(3,"Sent " << msg->Length << " Bytes on "
      << H5FDdsmCommToString(msg->Communicator) << " to " << msg->Dest);
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::MpiReceive(H5FDdsmMsg *msg, MPI_Comm comm)
{
  int            message_length;
  H5FDdsmInt32   status;
  MPI_Status     mpi_status;

  status = MPI_Recv(msg->Data, msg->Length, MPI_UNSIGNED_CHAR, msg->Source, msg->Tag,
      comm, &mpi_status);

  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Recv failed to receive " << msg->Length
        << " Bytes from " << msg->Source);
    H5FDdsmError("MPI Error Code = " << mpi_status.MPI_ERROR);
    return(H5FD_DSM_FAIL);
  }

  status = MPI_Get_count(&mpi_status, MPI_UNSIGNED_CHAR, &message_length);
  H5FDdsmDebugLevel(3,"Received " << message_length << " Bytes on "
      << H5FDdsmCommToString(msg->Communicator) << " from " << mpi_status.MPI_SOURCE);
  msg->SetSource(mpi_status.MPI_SOURCE);
  msg->SetLength(message_length);

  if (status != MPI_SUCCESS) {
    H5FDdsmError("MPI_Get_count failed");
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::MpiProbe(H5FDdsmMsg *msg, MPI_Comm comm)
{
  int            flag;
  H5FDdsmInt32   status;
  MPI_Status     mpi_status;

  status = MPI_Iprobe(msg->Source, msg->Tag, comm, &flag, &mpi_status);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Iprobe failed to probe from "
        << msg->Source << " Tag = " << H5FDdsmTagToString(msg->Tag));
    H5FDdsmError("MPI Error Code = " << mpi_status.MPI_ERROR);
    return(H5FD_DSM_FAIL);
  }

  if (flag) {
    msg->SetSource(mpi_status.MPI_SOURCE);
    H5FDdsmDebugLevel(3, "MPI_Iprobe found pending messages on " << H5FDdsmCommToString(msg->Communicator)
        << " from " << msg->Source << " Tag = " << H5FDdsmTagToString(msg->Tag));
  } else {
    H5FDdsmDebugLevel(5, "MPI_Iprobe did not find pending messages on " << H5FDdsmCommToString(msg->Communicator)
        << " from " << msg->Source << " Tag = " << H5FDdsmTagToString(msg->Tag));
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::MpiWinCreateLocal(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, MPI_Win *win)
{
  H5FDdsmInt32 status;

  status = MPI_Win_create(storagePointer, storageSize * sizeof(H5FDdsmByte),
      sizeof(H5FDdsmByte), MPI_INFO_NULL, this->IntraComm, win);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Win_create failed");
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::MpiWinFree(MPI_Win *win)
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
H5FDdsmComm::MpiPut(H5FDdsmMsg *msg, MPI_Win win)
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
  H5FDdsmDebugLevel(3,"Put " << msg->Length << " Bytes to " << msg->Dest
      << " at address " << msg->Address);

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::MpiGet(H5FDdsmMsg *msg, MPI_Win win)
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
  H5FDdsmDebugLevel(3,"Got " << msg->Length << " Bytes from " << msg->Dest
      << " at address " << msg->Address);

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::MpiBarrier(MPI_Comm comm)
{
  H5FDdsmInt32 status;

  status = MPI_Barrier(comm);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Barrier failed");
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::MpiBroadcast(H5FDdsmMsg *msg, MPI_Comm comm)
{
  H5FDdsmInt32 status;

  status = MPI_Bcast(msg->Data, msg->Length, MPI_UNSIGNED_CHAR, msg->Source, comm);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast failed to bcast " << msg->Length
        << " Bytes from " << msg->Source);
    return(H5FD_DSM_FAIL);
  }
  H5FDdsmDebugLevel(3,"Broadcasted " << msg->Length << " Bytes from "
      << msg->Source);
  return(H5FD_DSM_SUCCESS);
}
