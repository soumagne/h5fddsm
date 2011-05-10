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
  this->UseOneSidedComm = 1;
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

  if(H5FDdsmCommMpi::Init() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

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
H5FDdsmCommDmapp::Put(H5FDdsmMsg *DataMsg)
{
  dmapp_return_t status;
  H5FDdsmByte *targetPtr = (H5FDdsmByte *) (this->CommDmappInternals->DmappSegTable[DataMsg->Dest].Addr + DataMsg->Address);
  dmapp_seg_desc_t *targetSeg = &this->CommDmappInternals->DmappSegTable[DataMsg->Dest].SegDesc;
  H5FDdsmInt32 targetPE = this->CommDmappInternals->DmappSegTable[DataMsg->Dest].PE;

  if(H5FDdsmCommMpi::Put(DataMsg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

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
H5FDdsmCommDmapp::Get(H5FDdsmMsg *DataMsg)
{
  H5FDdsmInt32   status;

  if (H5FDdsmCommMpi::Get(DataMsg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

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
H5FDdsmCommDmapp::WindowSync()
{
  if (H5FDdsmCommMpi::WindowSync() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Win != MPI_WIN_NULL) {
    MPI_Win_fence(0, this->Win);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::Accept(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize)
{
  MPI_Comm winComm;
  dmapp_return_t status;

  if(H5FDdsmCommMpi::Accept(storagePointer, storageSize) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

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
H5FDdsmCommDmapp::Connect()
{
  MPI_Comm   winComm;
  MPI_Status recvStatus;

  if(H5FDdsmCommMpi::Connect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

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
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::Disconnect()
{
  if(H5FDdsmCommMpi::Disconnect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->IsStorageSegRegistered) {
    dmapp_mem_unregister(&this->StorageSegDesc);
    this->IsStorageSegRegistered = 0;
  }

  if (this->Win != MPI_WIN_NULL) {
    MPI_Win_free(&this->Win);
    this->Win = MPI_WIN_NULL;
  }
  return(H5FD_DSM_SUCCESS);
}
