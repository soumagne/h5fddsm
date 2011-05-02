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

//----------------------------------------------------------------------------
H5FDdsmComm::H5FDdsmComm()
{
  this->Comm = MPI_COMM_NULL;

  this->UseOneSidedComm = 0;
  this->UseStaticInterComm = 0;

  this->SyncChannels = 0;
}

//----------------------------------------------------------------------------
H5FDdsmComm::~H5FDdsmComm()
{
  if (this->Comm != MPI_COMM_NULL) {
    MPI_Comm_free(&this->Comm);
  }
  this->Comm = MPI_COMM_NULL;
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::DupComm(MPI_Comm Source)
{
  MPI_Comm    NewComm;

  MPI_Comm_dup(Source, &NewComm);
  return(this->SetComm(NewComm));
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Init()
{
  H5FDdsmInt32 size, rank;

  if(MPI_Comm_size(this->Comm, &size) != MPI_SUCCESS) return(H5FD_DSM_FAIL);
  if(MPI_Comm_rank(this->Comm, &rank) != MPI_SUCCESS) return(H5FD_DSM_FAIL);

  this->SetId(rank);
  this->SetTotalSize(size);
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Probe(H5FDdsmMsg *Msg)
{
  if(Msg->Tag <= 0) Msg->Tag = H5FD_DSM_DEFAULT_TAG;
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Receive(H5FDdsmMsg *Msg, H5FDdsmInt32 Channel)
{
  if(Msg->Tag <= 0) Msg->Tag = H5FD_DSM_DEFAULT_TAG;
  if(Msg->Length <= 0 ){
    H5FDdsmError("Cannot Receive Message of Length = " << Msg->Length);
    return(H5FD_DSM_FAIL);
  }
  if(Msg->Data <= 0 ){
    H5FDdsmError("Cannot Receive Message into Data Buffer = " << Msg->Length);
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Send(H5FDdsmMsg *Msg)
{
  if(Msg->Tag <= 0) Msg->Tag = H5FD_DSM_DEFAULT_TAG;
  if(Msg->Length <= 0 ){
    H5FDdsmError("Cannot Send Message of Length = " << Msg->Length);
    return(H5FD_DSM_FAIL);
  }
  if(Msg->Data <= 0 ){
    H5FDdsmError("Cannot Send Message from Data Buffer = " << Msg->Length);
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::GetData(H5FDdsmMsg *DataMsg)
{
  if(DataMsg->Tag <= 0) DataMsg->Tag = H5FD_DSM_DEFAULT_TAG;
  if(DataMsg->Length <= 0 ){
    H5FDdsmError("Cannot Get Message of Length = " << DataMsg->Length);
    return(H5FD_DSM_FAIL);
  }
  if(DataMsg->Data <= 0 ){
    H5FDdsmError("Cannot Get Message into Data Buffer = " << DataMsg->Length);
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::PutData(H5FDdsmMsg *DataMsg)
{
  if(DataMsg->Tag <= 0) DataMsg->Tag = H5FD_DSM_DEFAULT_TAG;
  if(DataMsg->Length <= 0 ){
    H5FDdsmError("Cannot Put Message of Length = " << DataMsg->Length);
    return(H5FD_DSM_FAIL);
  }
  if(DataMsg->Data <= 0 ){
    H5FDdsmError("Cannot Put Message from Data Buffer = " << DataMsg->Length);
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::Barrier()
{
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
H5FDdsmComm::RemoteCommAccept(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize)
{
  if (!storagePointer) {
    H5FDdsmError("Null Data Pointer passed to CommAccept");
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::RemoteCommConnect()
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::RemoteCommDisconnect()
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::RemoteCommSync()
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::RemoteCommChannelSynced(H5FDdsmInt32 who, H5FDdsmInt32 *syncId)
{
  static std::queue<H5FDdsmInt32> syncQueue;
  H5FDdsmInt32 ret = H5FD_DSM_FALSE;

  this->SyncChannels++;
  if (this->SyncChannels == this->InterSize) {
    H5FDdsmDebug("Channels cleared: " << this->SyncChannels << "/" << this->InterSize);
    this->SyncChannels = 0;
    if (!this->UseOneSidedComm) {
      if (!syncQueue.empty()) {
        H5FDdsmDebug("(" << this->Id << ") " << "pop sync queue from " << who);
        if (syncQueue.front() != who) {
          H5FDdsmError("(" << this->Id << ") " << "Mismatched IDs in sync queue ");
        }
        syncQueue.pop();
        if (!syncQueue.empty()) {
          H5FDdsmError("(" << this->Id << ") " << "Sync queue should be empty!! " << syncQueue.front());
        }
      }
    }
    *syncId = -1;
    ret = H5FD_DSM_TRUE;
  } else {
    if (!this->UseOneSidedComm) {
      if (syncQueue.empty()) {
        H5FDdsmDebug("(" << this->Id << ") " << "filling sync queue from " << who);
        for (int i=0; i<this->InterSize; i++) {
          if (i != who) syncQueue.push(i);
        }
      } else {
        H5FDdsmDebug("(" << this->Id << ") " << "pop sync queue from " << syncQueue.front());
        if (syncQueue.front() != who) {
          H5FDdsmError("(" << this->Id << ") " << "Mismatched IDs in sync queue ");
        }
        syncQueue.pop();
      }
      *syncId = syncQueue.front();
    }
  }
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::RemoteCommRecvReady()
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::RemoteCommSendReady()
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::RemoteCommRecvInfo(H5FDdsmInfo *dsmInfo)
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::RemoteCommSendInfo(H5FDdsmInfo *dsmInfo)
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::RemoteCommSendXML(H5FDdsmString file, H5FDdsmInt32 dest)
{
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmComm::RemoteCommRecvXML(H5FDdsmString *file)
{
  return(H5FD_DSM_SUCCESS);
}
