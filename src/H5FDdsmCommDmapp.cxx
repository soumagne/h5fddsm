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
#include <cstdlib>
#include <dmapp.h>

#define DMAPP_DQW_SIZE 16 // DMAPP_DQW

typedef struct DmappMdhEntry_
{
  H5FDdsmAddr      addr;
  dmapp_seg_desc_t mdh;
} DmappMdhEntry;
//
struct H5FDdsmCommDmappInternals
{
  H5FDdsmCommDmappInternals()
  {
    this->remote_inst_ids = NULL;
    this->local_inst_ids  = NULL;
    //
    this->remote_mdh_entries   = NULL;
    this->IsLocalMemRegistered = H5FD_DSM_FALSE;
  }
  ~H5FDdsmCommDmappInternals()
  {
    if (this->remote_inst_ids) free(this->remote_inst_ids);
    this->remote_inst_ids = NULL;
    if(this->local_inst_ids) free(this->local_inst_ids);
    this->local_inst_ids = NULL;
    //
    if (this->remote_mdh_entries) free(this->remote_mdh_entries);
    this->remote_mdh_entries = NULL;
  }
  //
  H5FDdsmInt32     *remote_inst_ids; // Remote instance IDs
  H5FDdsmInt32     *local_inst_ids;  // Local instance IDs
  //
  DmappMdhEntry  *remote_mdh_entries; // Server memory descriptor handles
  DmappMdhEntry   local_mdh_entry;    // Local storage memory descriptor handle
  H5FDdsmBoolean  IsLocalMemRegistered;
};

//----------------------------------------------------------------------------
H5FDdsmCommDmapp::H5FDdsmCommDmapp()
{
  this->InterCommType = H5FD_DSM_COMM_DMAPP;
  this->UseOneSidedComm = H5FD_DSM_TRUE;
  this->CommDmappInternals = new H5FDdsmCommDmappInternals;
  this->IsDmappInitialized = H5FD_DSM_FALSE;
  this->UseBlockingComm = H5FD_DSM_FALSE;
}

//----------------------------------------------------------------------------
H5FDdsmCommDmapp::~H5FDdsmCommDmapp()
{
  delete this->CommDmappInternals;
  if (this->IsDmappInitialized) {
    dmapp_finalize();
    this->IsDmappInitialized = H5FD_DSM_FALSE;
  }
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::Init()
{
  dmapp_return_t status;
  dmapp_rma_attrs_ext_t actual_args = {0}, rma_args = {0};

  if (H5FDdsmCommMpi::Init() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  // Set the RMA parameters
  rma_args.max_outstanding_nb   = DMAPP_DEF_OUTSTANDING_NB;
  rma_args.offload_threshold    = DMAPP_OFFLOAD_THRESHOLD;
  rma_args.put_relaxed_ordering = DMAPP_ROUTING_ADAPTIVE;
  rma_args.get_relaxed_ordering = DMAPP_ROUTING_ADAPTIVE;
  rma_args.max_concurrency      = 1;
  rma_args.PI_ordering          = DMAPP_PI_ORDERING_RELAXED;

  // Initialize DMAPP
  status = dmapp_init_ext(&rma_args, &actual_args);
  if (status != DMAPP_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " dmapp_init failed: " << status);
    return(H5FD_DSM_FAIL);
  }

  this->IsDmappInitialized = H5FD_DSM_TRUE;
  H5FDdsmDebug("CommDmapp initialized");
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::Put(H5FDdsmMsg *DataMsg)
{
  dmapp_return_t status;
  H5FDdsmByte *targetPtr = (H5FDdsmByte *)
          (this->CommDmappInternals->remote_mdh_entries[DataMsg->Dest].addr + DataMsg->Address);
  dmapp_seg_desc_t targetSeg = this->CommDmappInternals->remote_mdh_entries[DataMsg->Dest].mdh;
  H5FDdsmInt32 targetPE = this->CommDmappInternals->remote_inst_ids[DataMsg->Dest];
  //
  if (H5FDdsmCommMpi::Put(DataMsg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  H5FDdsmDebug("Putting " << DataMsg->Length << " Bytes to Address "
      << DataMsg->Address << " to Id = " << DataMsg->Dest);
  if (this->UseBlockingComm) {
    status = dmapp_put(targetPtr, &targetSeg, targetPE, DataMsg->Data, DataMsg->Length, DMAPP_BYTE);
  } else {
    status = dmapp_put_nbi(targetPtr, &targetSeg, targetPE, DataMsg->Data, DataMsg->Length, DMAPP_BYTE);
  }
  if (status != DMAPP_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " dmapp_put failed to put "
        << DataMsg->Length << " Bytes to " << DataMsg->Dest << " (PE " << targetPE << ")");
    return(H5FD_DSM_FAIL);
  }
  //
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::Get(H5FDdsmMsg *DataMsg)
{
  dmapp_return_t status;
  H5FDdsmByte *sourcePtr = (H5FDdsmByte *)
          (this->CommDmappInternals->remote_mdh_entries[DataMsg->Source].addr + DataMsg->Address);
  dmapp_seg_desc_t sourceSeg = this->CommDmappInternals->remote_mdh_entries[DataMsg->Source].mdh;
  H5FDdsmInt32 sourcePE = this->CommDmappInternals->remote_inst_ids[DataMsg->Source];
  //
  if (H5FDdsmCommMpi::Get(DataMsg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  H5FDdsmDebug("Getting " << DataMsg->Length << " Bytes from Address "
      << DataMsg->Address << " from Id = " << DataMsg->Source);
  if (this->UseBlockingComm) {
    status = dmapp_get(DataMsg->Data, sourcePtr, &sourceSeg, sourcePE,
        DataMsg->Length, DMAPP_BYTE);
  } else {
    status = dmapp_get_nbi(DataMsg->Data, sourcePtr, &sourceSeg, sourcePE,
        DataMsg->Length, DMAPP_BYTE);
  }
  if (status != DMAPP_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " dmapp_get failed to get "
        << DataMsg->Length << " Bytes from " << DataMsg->Source << " (PE " << sourcePE << ")");
    return(H5FD_DSM_FAIL);
  }
  //
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::WindowSync()
{
  dmapp_return_t status;

  if (H5FDdsmCommMpi::WindowSync() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (!this->UseBlockingComm) {
    status = dmapp_gsync_wait();
    if (status != DMAPP_RC_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " dmapp_gsync_wait failed: " << status);
      return(H5FD_DSM_FAIL);
    }
  }
  //
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::Accept(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize)
{
  dmapp_return_t dmapp_status;
  H5FDdsmInt32 status;

  // Create an MPI InterComm
  if (H5FDdsmCommMpi::Accept(storagePointer, storageSize) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  status = this->GatherIntraInstIds();
  if (status != H5FD_DSM_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " GatherIntraInstIds failed");
    return(H5FD_DSM_FAIL);
  }
  //
  if (this->Id == 0) {
    for (H5FDdsmInt32 i = 0; i < this->InterSize; i++) {
      status = MPI_Send(this->CommDmappInternals->local_inst_ids, this->IntraSize, MPI_INT32_T,
          i, H5FD_DSM_EXCHANGE_TAG, this->InterComm);
      if (status != MPI_SUCCESS) {
        H5FDdsmError("Id = " << this->Id << " MPI_Send of local instance IDs failed");
        return(H5FD_DSM_FAIL);
      }
    }
  }
  this->Barrier();
  //
  // Register memory segments
  this->CommDmappInternals->local_mdh_entry.addr = (H5FDdsmAddr) storagePointer;
  dmapp_status = dmapp_mem_register(storagePointer, storageSize, &this->CommDmappInternals->local_mdh_entry.mdh);
  if (dmapp_status != DMAPP_RC_SUCCESS) {
    H5FDdsmError("dmapp_mem_register failed: " << dmapp_status);
    return(H5FD_DSM_FAIL);
  }
  this->CommDmappInternals->IsLocalMemRegistered = H5FD_DSM_TRUE;
  //
  status = this->GatherIntraMdhEntries();
  if (status != H5FD_DSM_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " GatherIntraMdhEntries failed");
    return(H5FD_DSM_FAIL);
  }
  //
  // Send MDH entry to each remote PE
  for (H5FDdsmInt32 i = 0; i < this->InterSize; i++) {
    status = MPI_Send(&this->CommDmappInternals->local_mdh_entry, sizeof(DmappMdhEntry), MPI_UNSIGNED_CHAR,
        i, H5FD_DSM_EXCHANGE_TAG, this->InterComm);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Send of MDH entry failed");
      return(H5FD_DSM_FAIL);
    }
  }
  //
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::Connect()
{
  MPI_Status mpi_status;
  H5FDdsmInt32 status;

  if (H5FDdsmCommMpi::Connect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  // Get remote instance IDs
  H5FDdsmDebug("Get remote instance IDs");
  if (this->CommDmappInternals->remote_inst_ids) free(this->CommDmappInternals->remote_inst_ids);
  this->CommDmappInternals->remote_inst_ids = (H5FDdsmInt32*) malloc(this->InterSize * sizeof(H5FDdsmInt32));
  //
  status = MPI_Recv(this->CommDmappInternals->remote_inst_ids, this->InterSize, MPI_INT32_T,
      0, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &mpi_status);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Recv of local instance IDs failed");
    return(H5FD_DSM_FAIL);
  }
  //
  // Get remote MDH entries
  H5FDdsmDebug("Get remote MDH entries");
  if (this->CommDmappInternals->remote_mdh_entries) free(this->CommDmappInternals->remote_mdh_entries);
  this->CommDmappInternals->remote_mdh_entries = (DmappMdhEntry*) malloc(this->InterSize * sizeof(DmappMdhEntry));
  for (H5FDdsmInt32 i = 0; i < this->InterSize; i++) {
    status = MPI_Recv(&this->CommDmappInternals->remote_mdh_entries[i], sizeof(DmappMdhEntry), MPI_UNSIGNED_CHAR,
        i, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &mpi_status);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of MDH entry failed");
      return(H5FD_DSM_FAIL);
    }
  }
  //
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::Disconnect()
{
  dmapp_return_t status;
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  //
  if (H5FDdsmCommMpi::Disconnect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (this->CommDmappInternals->IsLocalMemRegistered) {
    status = dmapp_mem_unregister(&this->CommDmappInternals->local_mdh_entry.mdh);
    if (status != DMAPP_RC_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " dmapp_mem_unregister failed: " << status);
      ret = H5FD_DSM_FAIL;
    } else {
      this->CommDmappInternals->IsLocalMemRegistered = H5FD_DSM_FALSE;
    }
  }
  //
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::GatherIntraInstIds()
{
  dmapp_jobinfo_t job;
  H5FDdsmInt32 local_id;
  H5FDdsmInt32 status;
  //
  // Get job related information
  status = dmapp_get_jobinfo(&job);
  if (status != DMAPP_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " dmapp_get_jobinfo failed: " << status);
    return(H5FD_DSM_FAIL);
  }
  //
  local_id = job.pe;
  //
  // Allocate a buffer to hold the instance IDs from all of the other ranks.
  if (this->CommDmappInternals->local_inst_ids) free(this->CommDmappInternals->local_inst_ids);
  this->CommDmappInternals->local_inst_ids = (H5FDdsmInt32 *) malloc(sizeof(H5FDdsmInt32) * this->IntraSize);

  status = MPI_Allgather(&local_id, 1, MPI_INT32_T,
      this->CommDmappInternals->local_inst_ids, 1, MPI_INT32_T,
      this->IntraComm);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Gather of instance IDs failed");
    return(H5FD_DSM_FAIL);
  }
  //
  // Remote instance IDs are local IDs for the server.
  if (this->CommDmappInternals->remote_inst_ids) free(this->CommDmappInternals->remote_inst_ids);
  this->CommDmappInternals->remote_inst_ids = (H5FDdsmInt32*) malloc(this->IntraSize * sizeof(H5FDdsmInt32));
  memcpy(this->CommDmappInternals->remote_inst_ids, this->CommDmappInternals->local_inst_ids, this->IntraSize * sizeof(H5FDdsmInt32));
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::GatherIntraMdhEntries()
{
  H5FDdsmInt32 status;
  //
  // Allocate a buffer to hold the MDH entries from all of the other ranks.
  // Remote MDH entries are local entries for the server.
  if (this->CommDmappInternals->remote_mdh_entries) free(this->CommDmappInternals->remote_mdh_entries);
  this->CommDmappInternals->remote_mdh_entries = (DmappMdhEntry*) malloc(this->IntraSize * sizeof(DmappMdhEntry));

  status = MPI_Allgather(&this->CommDmappInternals->local_mdh_entry, sizeof(DmappMdhEntry), MPI_UNSIGNED_CHAR,
      this->CommDmappInternals->remote_mdh_entries, sizeof(DmappMdhEntry), MPI_UNSIGNED_CHAR,
      this->IntraComm);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Gather of MDH entries failed");
    return(H5FD_DSM_FAIL);
  }
  //
  return(H5FD_DSM_SUCCESS);
}
