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
  if (this->CommDmappInternals->IsLocalMemRegistered) this->DmappWinFree();
  this->CommDmappInternals->IsLocalMemRegistered = H5FD_DSM_FALSE;
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
  H5FDdsmDebugLevel(1, "CommDmapp initialized");
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::WinCreateData(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm)
{
  if (H5FDdsmComm::WinCreateData(storagePointer, storageSize, comm) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (comm == H5FD_DSM_INTER_COMM) {
    if (this->CommDmappInternals->IsLocalMemRegistered) this->DmappWinFree();
    this->CommDmappInternals->IsLocalMemRegistered = H5FD_DSM_FALSE;

    if (this->DmappWinCreateRemote(storagePointer, storageSize) != H5FD_DSM_SUCCESS) {
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::PutData(H5FDdsmMsg *msg)
{
  if (H5FDdsmComm::PutData(msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (msg->Communicator == H5FD_DSM_INTER_COMM) {
    if (this->CommDmappInternals->remote_mdh_entries) {
      dmapp_return_t status;
      H5FDdsmByte *targetPtr = (H5FDdsmByte *)
                  (this->CommDmappInternals->remote_mdh_entries[msg->Dest].addr + msg->Address);
      dmapp_seg_desc_t targetSeg = this->CommDmappInternals->remote_mdh_entries[msg->Dest].mdh;
      H5FDdsmInt32 targetPE = this->CommDmappInternals->remote_inst_ids[msg->Dest];

      if (this->UseBlockingComm) {
        status = dmapp_put(targetPtr, &targetSeg, targetPE, msg->Data, msg->Length, DMAPP_BYTE);
      } else {
        status = dmapp_put_nbi(targetPtr, &targetSeg, targetPE, msg->Data, msg->Length, DMAPP_BYTE);
      }
      if (status != DMAPP_RC_SUCCESS) {
        H5FDdsmError("Id = " << this->Id << " dmapp_put failed to put " <<
            msg->Length << " Bytes to " << msg->Dest << " (PE " << targetPE << ")"
            << " at address " << msg->Address);
        return(H5FD_DSM_FAIL);
      }
    } else {
      H5FDdsmError("remote_mdh_entries is NULL");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebugLevel(3, "Put " << msg->Length << " Bytes to " << msg->Dest
        << " at address " << msg->Address);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::GetData(H5FDdsmMsg *msg)
{
  if (H5FDdsmComm::GetData(msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (msg->Communicator == H5FD_DSM_INTER_COMM) {
    if (this->CommDmappInternals->remote_mdh_entries) {
      dmapp_return_t status;
      H5FDdsmByte *sourcePtr = (H5FDdsmByte *)
              (this->CommDmappInternals->remote_mdh_entries[msg->Source].addr + msg->Address);
      dmapp_seg_desc_t sourceSeg = this->CommDmappInternals->remote_mdh_entries[msg->Source].mdh;
      H5FDdsmInt32 sourcePE = this->CommDmappInternals->remote_inst_ids[msg->Source];

      if (this->UseBlockingComm) {
        status = dmapp_get(msg->Data, sourcePtr, &sourceSeg, sourcePE, msg->Length, DMAPP_BYTE);
      } else {
        status = dmapp_get_nbi(msg->Data, sourcePtr, &sourceSeg, sourcePE, msg->Length, DMAPP_BYTE);
      }
      if (status != DMAPP_RC_SUCCESS) {
        H5FDdsmError("Id = " << this->Id << " dmapp_get failed to get " <<
            msg->Length << " Bytes from " << msg->Source << " (PE " << sourcePE << ")"
            << " at address " << msg->Address);
        return(H5FD_DSM_FAIL);
      }
    } else {
      H5FDdsmError("remote_mdh_entries is NULL");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebugLevel(3, "Got " << msg->Length << " Bytes from " << msg->Dest
        << " at address " << msg->Address);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::WindowSyncData()
{
  dmapp_return_t status;

  if (H5FDdsmComm::WindowSyncData() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (!this->UseBlockingComm) {
    status = dmapp_gsync_wait();
    if (status != DMAPP_RC_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " dmapp_gsync_wait failed: " << status);
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebugLevel(3, "Id = " << this->Id << " dmapp_gsync_wait succeeded");
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::WinCreateNotification(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm)
{
  if (H5FDdsmComm::WinCreateData(storagePointer, storageSize, comm) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (comm == H5FD_DSM_INTER_COMM) {
    // not implemented
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::PutNotification(H5FDdsmMsg *msg)
{
  if (msg->Communicator == H5FD_DSM_INTER_COMM) {
    // not implemented
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::GetNotification(H5FDdsmMsg *msg)
{
  if (msg->Communicator == H5FD_DSM_INTER_COMM) {
    // not implemented
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::WinCreateLock(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm)
{
  if (H5FDdsmComm::WinCreateLock(storagePointer, storageSize, comm) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (comm == H5FD_DSM_INTER_COMM) {
    // not implemented
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::PutLock(H5FDdsmMsg *msg)
{
  if (msg->Communicator == H5FD_DSM_INTER_COMM) {
    // not implemented
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::GetLock(H5FDdsmMsg *msg)
{
  if (msg->Communicator == H5FD_DSM_INTER_COMM) {
    // not implemented
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::DmappWinCreateRemote(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize)
{
  H5FDdsmBoolean isClient;
  dmapp_return_t dmapp_status;
  H5FDdsmInt32 status;

  // If NULL we don't host the DSM so use this result for Intercomm ordering
  isClient = (storagePointer == NULL) ? H5FD_DSM_TRUE : H5FD_DSM_FALSE;

  if (isClient) {
    // Get remote instance IDs
    H5FDdsmDebugLevel(2, "Get remote instance IDs");
    if (this->CommDmappInternals->remote_inst_ids) free(this->CommDmappInternals->remote_inst_ids);
    this->CommDmappInternals->remote_inst_ids = (H5FDdsmInt32*) malloc(this->InterSize * sizeof(H5FDdsmInt32));
    //
    status = MPI_Recv(this->CommDmappInternals->remote_inst_ids, this->InterSize, MPI_INT32_T,
        0, H5FD_DSM_EXCHANGE_TAG, this->InterComm, MPI_STATUS_IGNORE);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of local instance IDs failed");
      return(H5FD_DSM_FAIL);
    }
    //
    // Get remote MDH entries
    H5FDdsmDebugLevel(2, "Get remote MDH entries");
    if (this->CommDmappInternals->remote_mdh_entries) free(this->CommDmappInternals->remote_mdh_entries);
    this->CommDmappInternals->remote_mdh_entries = (DmappMdhEntry*) malloc(this->InterSize * sizeof(DmappMdhEntry));
    for (H5FDdsmInt32 i = 0; i < this->InterSize; i++) {
      status = MPI_Recv(&this->CommDmappInternals->remote_mdh_entries[i], sizeof(DmappMdhEntry), MPI_UNSIGNED_CHAR,
          i, H5FD_DSM_EXCHANGE_TAG, this->InterComm, MPI_STATUS_IGNORE);
      if (status != MPI_SUCCESS) {
        H5FDdsmError("Id = " << this->Id << " MPI_Recv of MDH entry failed");
        return(H5FD_DSM_FAIL);
      }
    }
  } else { // Server
    // Gather local instance IDs
    H5FDdsmDebugLevel(2, "Gather local instance IDs");
    status = this->DmappGatherIntraInstIds();
    if (status != H5FD_DSM_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " GatherIntraInstIds failed");
      return(H5FD_DSM_FAIL);
    }
    //
    if (this->Id == 0) {
      H5FDdsmDebugLevel(2, "Send local instance IDs");
      for (H5FDdsmInt32 i = 0; i < this->InterSize; i++) {
        status = MPI_Send(this->CommDmappInternals->local_inst_ids, this->IntraSize, MPI_INT32_T,
            i, H5FD_DSM_EXCHANGE_TAG, this->InterComm);
        if (status != MPI_SUCCESS) {
          H5FDdsmError("Id = " << this->Id << " MPI_Send of local instance IDs failed");
          return(H5FD_DSM_FAIL);
        }
      }
    }
    this->Barrier(H5FD_DSM_INTRA_COMM);
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
    status = this->DmappGatherIntraMdhEntries();
    if (status != H5FD_DSM_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " GatherIntraMdhEntries failed");
      return(H5FD_DSM_FAIL);
    }
    //
    // Send MDH entry to each remote PE
    H5FDdsmDebugLevel(2, "Send local MDH entry");
    for (H5FDdsmInt32 i = 0; i < this->InterSize; i++) {
      status = MPI_Send(&this->CommDmappInternals->local_mdh_entry, sizeof(DmappMdhEntry), MPI_UNSIGNED_CHAR,
          i, H5FD_DSM_EXCHANGE_TAG, this->InterComm);
      if (status != MPI_SUCCESS) {
        H5FDdsmError("Id = " << this->Id << " MPI_Send of MDH entry failed");
        return(H5FD_DSM_FAIL);
      }
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::DmappWinFree()
{
  dmapp_return_t status;

  status = dmapp_mem_unregister(&this->CommDmappInternals->local_mdh_entry.mdh);
  if (status != DMAPP_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " dmapp_mem_unregister failed: " << status);
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommDmapp::DmappGatherIntraInstIds()
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
H5FDdsmCommDmapp::DmappGatherIntraMdhEntries()
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
  return(H5FD_DSM_SUCCESS);
}
