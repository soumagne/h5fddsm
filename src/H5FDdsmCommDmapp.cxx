/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmCommDmapp.cxx

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

#include "H5FDdsmCommDmapp.h"
#include "H5FDdsmMsg.h"

#include <cstring>
#include <vector>

struct H5FDdsmCommDmappInternals
{
  struct DmappSegEntry
  {
    DmappSegEntry(H5FDdsmAddr addr, dmapp_seg_desc_t seg, H5FDdsmInt32 pe) : Addr(addr),
        PE(pe)
    {
      memcpy(&SegDesc, &seg, sizeof(dmapp_seg_desc_t));
    }
    H5FDdsmAddr Addr;
    dmapp_seg_desc_t SegDesc;
    H5FDdsmInt32 PE;
  };

  typedef std::vector<DmappSegEntry> DmappSegEntries;

  DmappSegEntries DmappSegTable;
};
//----------------------------------------------------------------------------
H5FDdsmCommDmapp::H5FDdsmCommDmapp()
{
  this->CommType = H5FD_DSM_COMM_DMAPP;
  this->InterComm = MPI_COMM_NULL;
  this->CommChannel = H5FD_DSM_INTRA_COMM;
  this->Win = MPI_WIN_NULL;
  this->CommDmappInternals = new H5FDdsmCommDmappInternals;
  this->IsDmappInitialized = 0;
  this->DmappRank = 0;
  this->IsStorageSegRegistered = 0;
}

//----------------------------------------------------------------------------
H5FDdsmCommDmapp::~H5FDdsmCommDmapp()
{
  delete this->CommDmappInternals;
  if (this->IsDmappInitialized) {
    dmapp_finalize();
    this->IsDmappInitialized = 0;
  }
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::Init()
{
  dmapp_return_t status;
  dmapp_rma_attrs_t actual_args = {0}, rma_args = {0};
  dmapp_jobinfo_t job;

  if(H5FDdsmComm::Init() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  // Set the RMA parameters
  rma_args.put_relaxed_ordering = DMAPP_ROUTING_ADAPTIVE;
  rma_args.max_outstanding_nb = DMAPP_DEF_OUTSTANDING_NB;
  rma_args.offload_threshold = DMAPP_OFFLOAD_THRESHOLD;
  rma_args.max_concurrency = 2;

  // Initialize DMAPP
  status = dmapp_init(&rma_args, &actual_args);
  if (status != DMAPP_RC_SUCCESS) {
    H5FDdsmError("dmapp_init Failed: " << status);
    return(H5FD_DSM_FAIL);
  }

  // Get job related information
  status = dmapp_get_jobinfo(&job);
  if (status != DMAPP_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << "dmapp_get_jobinfo FAILED: " << status);
    return(H5FD_DSM_FAIL);
  }

  this->DmappRank = job.pe;

  this->IsDmappInitialized = 1;
  H5FDdsmDebug("CommDmapp initialized");
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::Probe(H5FDdsmMsg *Msg)
{
  int         nid, flag=0;
  MPI_Status  status;

  if(H5FDdsmComm::Probe(Msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  MPI_Iprobe(MPI_ANY_SOURCE, Msg->Tag, this->IntraComm, &flag, &status);
  H5FDdsmDebug("MPI_Iprobe " << H5FDdsmTagToString(Msg->Tag));
  if(flag){
    nid = status.MPI_SOURCE;
    Msg->SetSource(nid);
    return(H5FD_DSM_SUCCESS);
  }
  return(H5FD_DSM_FAIL);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::Receive(H5FDdsmMsg *Msg, H5FDdsmInt32 Channel)
{
  int            messageLength;
  H5FDdsmInt32   status;
  H5FDdsmInt32   source = MPI_ANY_SOURCE;
  H5FDdsmInt32   receiveChannel = Channel;
  MPI_Status     sendRecvStatus;

  if(H5FDdsmComm::Receive(Msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  if(Msg->Source >= 0) source = Msg->Source;

  if (!receiveChannel) receiveChannel = this->CommChannel;

  if (receiveChannel == H5FD_DSM_INTER_COMM) {
    H5FDdsmDebug("(" << this->Id << ") Receiving from remote DSM " << Msg->Length << " bytes from " << source << " Tag = " << H5FDdsmTagToString(Msg->Tag));
    status = MPI_Recv(Msg->Data, Msg->Length, MPI_UNSIGNED_CHAR, source, Msg->Tag, this->InterComm, &sendRecvStatus);
  }
  else {
    H5FDdsmDebug("(" << this->Id << ") Receiving " << Msg->Length << " bytes from " << source << " Tag = " << H5FDdsmTagToString(Msg->Tag));
    status = MPI_Recv(Msg->Data, Msg->Length, MPI_UNSIGNED_CHAR, source, Msg->Tag, this->IntraComm, &sendRecvStatus);
  }
  if(status != MPI_SUCCESS){
    H5FDdsmError("Id = " << this->Id << " MPI_Recv failed to receive " << Msg->Length << " Bytes from " << Msg->Source);
    H5FDdsmError("MPI Error Code = " << sendRecvStatus.MPI_ERROR);
    return(H5FD_DSM_FAIL);
  }
  status = MPI_Get_count(&sendRecvStatus, MPI_UNSIGNED_CHAR, &messageLength);
  H5FDdsmDebug("(" << this->Id << ") Received " << messageLength << " bytes from " << sendRecvStatus.MPI_SOURCE);
  Msg->SetSource(sendRecvStatus.MPI_SOURCE);
  Msg->SetLength(messageLength);
  if(status != MPI_SUCCESS){
    H5FDdsmError("MPI_Get_count failed ");
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::Send(H5FDdsmMsg *Msg)
{
  H5FDdsmInt32   status;

  if(H5FDdsmComm::Send(Msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->CommChannel == H5FD_DSM_INTER_COMM) {
    H5FDdsmDebug("(" << this->Id << ") Sending to remote DSM " << Msg->Length << " bytes to " << Msg->Dest << " Tag = " << H5FDdsmTagToString(Msg->Tag));
    status = MPI_Send(Msg->Data, Msg->Length, MPI_UNSIGNED_CHAR, Msg->Dest, Msg->Tag, this->InterComm);
  }
  else {
    H5FDdsmDebug("(" << this->Id << ") Sending " << Msg->Length << " bytes to " << Msg->Dest << " Tag = " << H5FDdsmTagToString(Msg->Tag));
    status = MPI_Send(Msg->Data, Msg->Length, MPI_UNSIGNED_CHAR, Msg->Dest, Msg->Tag, this->IntraComm);
  }
  if(status != MPI_SUCCESS){
    H5FDdsmError("Id = " << this->Id << " MPI_Send failed to send " << Msg->Length << " Bytes to " << Msg->Dest);
    return(H5FD_DSM_FAIL);
  }
  H5FDdsmDebug("(" << this->Id << ") Sent " << Msg->Length << " bytes to " << Msg->Dest);

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::GetData(H5FDdsmMsg *DataMsg)
{
  H5FDdsmInt32   status;

  if (H5FDdsmComm::GetData(DataMsg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  MPI_Win_lock(MPI_LOCK_SHARED, DataMsg->Source, 0, this->Win);

  H5FDdsmDebug("Getting " << DataMsg->Length << " Bytes from Address " << DataMsg->Address << " from Id = " << DataMsg->Source);
  status = MPI_Get(DataMsg->Data, DataMsg->Length, MPI_UNSIGNED_CHAR, DataMsg->Source, DataMsg->Address, DataMsg->Length, MPI_UNSIGNED_CHAR, this->Win);
  if(status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Get failed to get " << DataMsg->Length << " Bytes from " << DataMsg->Source);
    return(H5FD_DSM_FAIL);
  }

  MPI_Win_unlock(DataMsg->Source, this->Win);

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::PutData(H5FDdsmMsg *DataMsg)
{
  dmapp_return_t status;
  H5FDdsmByte *targetPtr = (H5FDdsmByte *) (this->CommDmappInternals->DmappSegTable[DataMsg->Dest].Addr + DataMsg->Address);
  dmapp_seg_desc_t *targetSeg = &this->CommDmappInternals->DmappSegTable[DataMsg->Dest].SegDesc;
  H5FDdsmInt32 targetPE = this->CommDmappInternals->DmappSegTable[DataMsg->Dest].PE;

  if(H5FDdsmComm::PutData(DataMsg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  H5FDdsmDebug("Putting " << DataMsg->Length << " Bytes to Address " << DataMsg->Address << " to Id = " << DataMsg->Dest);
  status = dmapp_put(targetPtr, targetSeg, targetPE, DataMsg->Data, DataMsg->Length, DMAPP_BYTE);
  if (status != DMAPP_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " dmapp_put failed to put " << DataMsg->Length
        << " Bytes to " << DataMsg->Dest << " (PE " << targetPE << ")");
    return(H5FD_DSM_FAIL);
  }

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::Barrier()
{
  if(H5FDdsmComm::Barrier() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  MPI_Barrier(this->IntraComm);

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::OpenPort()
{
  // Not supported
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::ClosePort()
{
  // Not supported
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::RemoteCommAccept(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize)
{
  MPI_Comm winComm;
  dmapp_return_t status;

  if(H5FDdsmComm::RemoteCommAccept(storagePointer, storageSize) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->UseStaticInterComm) {
    H5FDdsmInt32 global_size;
    MPI_Comm_size(MPI_COMM_WORLD, &global_size);
    MPI_Intercomm_create(this->IntraComm, 0, MPI_COMM_WORLD, global_size - (global_size - this->TotalSize), H5FD_DSM_DEFAULT_TAG, &this->InterComm);
  } else {
    H5FDdsmError("DMAPP communicator does not support dynamic connection");
  }

  MPI_Comm_remote_size(this->InterComm, &this->InterSize);
  H5FDdsmDebug("Server InterComm size: " << this->InterSize);

  MPI_Intercomm_merge(this->InterComm, 0, &winComm);
  if (MPI_Win_create(storagePointer, storageSize*sizeof(H5FDdsmByte), sizeof(H5FDdsmByte), MPI_INFO_NULL, winComm, &this->Win) != MPI_SUCCESS){
    H5FDdsmError("Id = " << this->Id << " MPI_Win_create failed");
    return(H5FD_DSM_FAIL);
  }
  MPI_Comm_free(&winComm);

  status = dmapp_mem_register(storagePointer, storageSize, &this->StorageSegDesc);
  if (status != DMAPP_RC_SUCCESS) {
    H5FDdsmError("dmapp_mem_register Failed: " << status);
    return(H5FD_DSM_FAIL);
  }
  this->IsStorageSegRegistered = 1;

  // Send now memory segment information
  H5FDdsmAddr storageAddr = (H5FDdsmAddr) storagePointer;
  for (H5FDdsmInt32 i = 0; i < this->InterSize; i++) {
    if (MPI_Send(&storageAddr, sizeof(storageAddr), MPI_UNSIGNED_CHAR, i, H5FD_DSM_EXCHANGE_TAG, this->InterComm)!= MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Send of Storage Address failed");
      return(H5FD_DSM_FAIL);
    };
    if (MPI_Send(&this->StorageSegDesc, sizeof(this->StorageSegDesc), MPI_UNSIGNED_CHAR, i, H5FD_DSM_EXCHANGE_TAG, this->InterComm)!= MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Send of Storage Segment Descriptor failed");
      return(H5FD_DSM_FAIL);
    };
    if (MPI_Send(&this->DmappRank, sizeof(this->DmappRank), MPI_UNSIGNED_CHAR, i, H5FD_DSM_EXCHANGE_TAG, this->InterComm)!= MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Send of DMAPP PE rank failed");
      return(H5FD_DSM_FAIL);
    };
  }

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::RemoteCommConnect()
{
  H5FDdsmInt32 isConnected = H5FD_DSM_FAIL;
  MPI_Comm     winComm;
  H5FDdsmInt32 status;

  if(H5FDdsmComm::RemoteCommConnect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->UseStaticInterComm) {
    status = MPI_Intercomm_create(this->IntraComm, 0, MPI_COMM_WORLD, 0, H5FD_DSM_DEFAULT_TAG, &this->InterComm);
    if (status == MPI_SUCCESS) {
      H5FDdsmDebug("Id = " << this->Id << " MPI_Intercomm_create returned SUCCESS");
      isConnected = H5FD_DSM_SUCCESS;
    }
  } else {
    H5FDdsmError("DMAPP communicator does not support dynamic connection");
  }

  if (isConnected == H5FD_DSM_SUCCESS) {
    MPI_Status recvStatus;

    this->CommChannel = H5FD_DSM_INTER_COMM;

    MPI_Comm_remote_size(this->InterComm, &this->InterSize);
    H5FDdsmDebug("Client InterComm size: " << this->InterSize);

    MPI_Intercomm_merge(this->InterComm, 1, &winComm);
    if (MPI_Win_create(NULL, 0, sizeof(H5FDdsmInt8), MPI_INFO_NULL, winComm, &this->Win) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Win_create failed");
      return(H5FD_DSM_FAIL);
    }
    MPI_Comm_free(&winComm);

    // Receive now remote memory segment information
    for (H5FDdsmInt32 i = 0; i < this->InterSize; i++) {
      H5FDdsmAddr storageAddr;
      dmapp_seg_desc_t segDesc;
      H5FDdsmInt32 dmappRank;
      if (MPI_Recv(&storageAddr, sizeof(storageAddr), MPI_UNSIGNED_CHAR, i, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &recvStatus) != MPI_SUCCESS) {
        H5FDdsmError("Id = " << this->Id << " MPI_Recv of Storage Address failed");
        return(H5FD_DSM_FAIL);
      }
      if (MPI_Recv(&segDesc, sizeof(segDesc), MPI_UNSIGNED_CHAR, i, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &recvStatus) != MPI_SUCCESS) {
        H5FDdsmError("Id = " << this->Id << " MPI_Recv of Storage Segment Descriptor failed");
        return(H5FD_DSM_FAIL);
      }
      if (MPI_Recv(&dmappRank, sizeof(dmappRank), MPI_UNSIGNED_CHAR, i, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &recvStatus) != MPI_SUCCESS) {
        H5FDdsmError("Id = " << this->Id << " MPI_Recv of of DMAPP PE rank failed");
        return(H5FD_DSM_FAIL);
      }
      this->CommDmappInternals->DmappSegTable.push_back(
          H5FDdsmCommDmappInternals::DmappSegEntry(storageAddr, segDesc, dmappRank));
    }
    return(H5FD_DSM_SUCCESS);
  } else {
    return(H5FD_DSM_FAIL);
  }
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::RemoteCommDisconnect()
{
  if(H5FDdsmComm::RemoteCommDisconnect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->IsStorageSegRegistered) {
    dmapp_mem_unregister(&this->StorageSegDesc);
    this->IsStorageSegRegistered = 0;
  }

  if (this->Win != MPI_WIN_NULL) {
    MPI_Win_free(&this->Win);
    this->Win = MPI_WIN_NULL;
  }
  if (this->InterComm != MPI_COMM_NULL) {
    if (this->UseStaticInterComm) {
      MPI_Comm_free(&this->InterComm);
    } else {
      // Not supported
    }
    this->InterComm = MPI_COMM_NULL;
  }
  this->CommChannel = H5FD_DSM_INTRA_COMM;
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::RemoteCommSync()
{
  if (H5FDdsmComm::RemoteCommSync() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Win != MPI_WIN_NULL) {
    MPI_Barrier(this->InterComm);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::RemoteCommRecvReady()
{
  if (H5FDdsmComm::RemoteCommRecvReady() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  H5FDdsmByte ready[6];

  if (this->Id == 0) {
    MPI_Status status;
    if (MPI_Recv(ready, sizeof(ready), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &status) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of ready failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("Recv ready: " << ready);
  }
  this->Barrier();

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::RemoteCommSendReady()
{
  if (H5FDdsmComm::RemoteCommSendReady() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  H5FDdsmByte ready[6] = "ready";

  if (this->Id == 0) {
    H5FDdsmDebug("Send ready: " << ready);
    if (MPI_Send(ready, sizeof(ready), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Send of ready failed");
      return(H5FD_DSM_FAIL);
    }
  }
  this->Barrier();

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::RemoteCommRecvInfo(H5FDdsmInfo *dsmInfo)
{
  if(H5FDdsmComm::RemoteCommRecvInfo(dsmInfo) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    MPI_Status status;
    H5FDdsmDebug("Receiving DSM Info...");
    if (MPI_Recv(dsmInfo, sizeof(H5FDdsmInfo), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &status) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of Info failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("Recv DSM Info Completed");
  }
  if (MPI_Bcast(dsmInfo, sizeof(H5FDdsmInfo), MPI_UNSIGNED_CHAR, 0, this->IntraComm) != MPI_SUCCESS){
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of Info failed");
    return(H5FD_DSM_FAIL);
  }

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::RemoteCommSendInfo(H5FDdsmInfo *dsmInfo)
{
  if(H5FDdsmComm::RemoteCommSendInfo(dsmInfo) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    H5FDdsmDebug("Sending DSM Info...");
    if (MPI_Send(dsmInfo, sizeof(H5FDdsmInfo), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Send of Info failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("Send DSM Info Completed");
  }
  this->Barrier();
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::RemoteCommSendXML(H5FDdsmString file, H5FDdsmInt32 dest)
{
  MPI_Comm communicator;
  if(H5FDdsmComm::RemoteCommSendXML(file, dest) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (this->Id == 0) {
    if (this->CommChannel == H5FD_DSM_INTER_COMM) {
      communicator = this->InterComm;
    }
    else {
      communicator = this->IntraComm;
    }
    //
    H5FDdsmInt32 length = (H5FDdsmInt32)strlen(file) + 1;
    H5FDdsmDebug("Send XML to DSM Buffer object - FilePath: " << file);
    if (MPI_Send(file, length, MPI_CHAR, dest, H5FD_DSM_XML_TAG, communicator) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Send of XML file failed");
      return(H5FD_DSM_FAIL);
    }
  }
  H5FDdsmDebug("Send DSM XML Completed");
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::RemoteCommRecvXML(H5FDdsmString *file)
{
  MPI_Comm communicator;
  H5FDdsmInt32 length;

  if(H5FDdsmComm::RemoteCommRecvXML(file) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (this->CommChannel == H5FD_DSM_INTER_COMM) {
    communicator = this->InterComm;
  }
  else {
    communicator = this->IntraComm;
  }
  //
  MPI_Status status;
  MPI_Probe(0, H5FD_DSM_XML_TAG, communicator, &status);
  MPI_Get_count(&status, MPI_CHAR, &length);
  *file = new char[length];
  if (MPI_Recv(*file, length, MPI_CHAR, 0, H5FD_DSM_XML_TAG, communicator, &status) != MPI_SUCCESS){
    H5FDdsmError("Id = " << this->Id << " MPI_Recv of XML file failed");
    return(H5FD_DSM_FAIL);
  }
  H5FDdsmDebug("Recv DSM XML: " << *file);
  return(H5FD_DSM_SUCCESS);
}
