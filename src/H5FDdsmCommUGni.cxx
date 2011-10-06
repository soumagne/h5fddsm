/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmCommUGni.cxx

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

#include "H5FDdsmCommUGni.h"
#include "H5FDdsmMsg.h"

#include <cstring>
#include <cstdlib>

#include <gni_libonesided_interop.h>

#define GNI_RDMA_OFFLOAD_THRESHOLD 0
#define GNI_FMA_OFFLOAD_THRESHOLD 0
//
typedef struct UGniMdhEntry_
{
  H5FDdsmAddr      addr;
  gni_mem_handle_t mdh;
} UGniMdhEntry;
//
struct H5FDdsmCommUGniInternals
{
  H5FDdsmCommUGniInternals()
  {
    this->ep_handles = NULL;
    this->ep_bound = NULL;
    //
    this->remote_nic_addresses = NULL;
    this->local_nic_addresses = NULL;
    //
    this->remote_inst_ids = NULL;
    this->local_inst_ids = NULL;
    //
    this->remote_mdh_entries = NULL;
    this->IsLocalMemRegistered = H5FD_DSM_FALSE;
  }
  ~H5FDdsmCommUGniInternals()
  {
    if (this->ep_handles) free(this->ep_handles);
    this->ep_handles = NULL;
    if (this->ep_bound) free(this->ep_bound);
    this->ep_bound = NULL;
    //
    if (this->remote_nic_addresses) free(this->remote_nic_addresses);
    this->remote_nic_addresses = NULL;
    if (this->local_nic_addresses) free(this->local_nic_addresses);
    this->local_nic_addresses = NULL;
    if (this->remote_inst_ids) free(this->remote_inst_ids);
    this->remote_inst_ids = NULL;
    if(this->local_inst_ids) free(this->local_inst_ids);
    this->local_inst_ids = NULL;
  }
  //
  gni_nic_handle_t  nic_handle; // NIC handle used for the communication
  gni_cq_handle_t   cq_handle;  // Completion queue handle
  //
  gni_ep_handle_t  *ep_handles; // List of end points
  H5FDdsmBoolean   *ep_bound;
  //
  H5FDdsmUInt32    *remote_nic_addresses; // Remote NIC addresses
  H5FDdsmUInt32    *local_nic_addresses;  // Local NIC addresses
  //
  H5FDdsmInt32     *remote_inst_ids; // Remote instance IDs
  H5FDdsmInt32     *local_inst_ids;  // Local instance IDs
  //
  UGniMdhEntry     *remote_mdh_entries; // Server memory descriptor handles
  UGniMdhEntry      local_mdh_entry;    // Local storage memory descriptor handle
  H5FDdsmBoolean    IsLocalMemRegistered;
};
//----------------------------------------------------------------------------
H5FDdsmCommUGni::H5FDdsmCommUGni()
{
  this->InterCommType = H5FD_DSM_COMM_UGNI;
  this->UseOneSidedComm = H5FD_DSM_TRUE;
  this->CommUGniInternals = new H5FDdsmCommUGniInternals;
}

//----------------------------------------------------------------------------
H5FDdsmCommUGni::~H5FDdsmCommUGni()
{
  gni_return_t status;
  //
  if (this->IsCommInitialized) {
    status = GNI_CqDestroy(this->CommUGniInternals->cq_handle);
    if (status != GNI_RC_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " GNI_CqDestroy failed: " << status);
    }
  }
  delete this->CommUGniInternals;
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::Init()
{
  H5FDdsmInt32 nhandles;
  gni_nic_handle_t *nic_hndls;
  gni_return_t gni_status;
  H5FDdsmInt32 status;
  //
  if (H5FDdsmCommMpi::Init() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  // Get NIC handles already attached to communication domain created by MPI
  status = MPIU_nem_gni_libonesided_interop_get_num_nic_hndls(&nhandles);
  if (status < 0) {
    H5FDdsmError("Id = " << this->Id << " MPIU_nem_gni_libonesided_interop_get_num_nic_hndls failed: " << status);
    return(H5FD_DSM_FAIL);
  }
  H5FDdsmDebug("Found " << nhandles << " GNI NIC handle(s)");
  if (nhandles > 1) {
    H5FDdsmError("Id = " << this->Id << " More than one NIC handle were found!")
    return(H5FD_DSM_FAIL);
  }
  //
  nic_hndls = (gni_nic_handle_t*) malloc(nhandles * sizeof(gni_nic_handle_t));
  //
  status = MPIU_nem_gni_libonesided_nic_hndls(nic_hndls, nhandles);
  if (status < 0) {
    H5FDdsmError("Id = " << this->Id << " MPIU_nem_gni_libonesided_nic_hndls failed: " << status);
    return(H5FD_DSM_FAIL);
  }
  //
  // Assuming a single gemini device, copy the given nic handle
  memcpy(&this->CommUGniInternals->nic_handle, &nic_hndls[0], sizeof(gni_nic_handle_t));
  free(nic_hndls);
  //
  // Create a completion queue
  H5FDdsmDebug("Creating a new CQ");
  gni_status = GNI_CqCreate(this->CommUGniInternals->nic_handle, 10, 0, GNI_CQ_BLOCKING,
      NULL, NULL, &this->CommUGniInternals->cq_handle);
  if (gni_status != GNI_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " GNI_CqCreate failed: " << gni_status);
    return(H5FD_DSM_FAIL);
  }
  //
  this->IsCommInitialized = H5FD_DSM_TRUE;
  H5FDdsmDebug("CommUGni initialized");
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::Put(H5FDdsmMsg *DataMsg)
{
  gni_mem_handle_t src_mem_handle;
  gni_return_t gni_status;
  H5FDdsmInt32 status;
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  //
  if (H5FDdsmCommMpi::Put(DataMsg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  H5FDdsmDebug("Register memory with the NIC");
  status = GNI_MemRegister(this->CommUGniInternals->nic_handle, (uint64_t) DataMsg->Data,
      DataMsg->Length, NULL, GNI_MEM_READ_ONLY, -1, &src_mem_handle);
  if (status != GNI_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " GNI_MemRegister failed: " << status);
    return(H5FD_DSM_FAIL);
  }
  //
  H5FDdsmDebug("Putting " << DataMsg->Length << " Bytes to Address "
      << DataMsg->Address << " to Id = " << DataMsg->Dest);
  if (DataMsg->Length > GNI_RDMA_OFFLOAD_THRESHOLD) {
    status = this->PutRdma(DataMsg, src_mem_handle);
    if (status != H5FD_DSM_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " PutRdma failed");
      ret = H5FD_DSM_FAIL;
    }
  }
  else if (DataMsg->Length > GNI_FMA_OFFLOAD_THRESHOLD) {
    status = this->PutFma(DataMsg, src_mem_handle);
    if (status != H5FD_DSM_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " PutFma failed");
      ret = H5FD_DSM_FAIL;
    }
  }
  //
  status = GNI_MemDeregister(this->CommUGniInternals->nic_handle, &src_mem_handle);
  if (status != GNI_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " GNI_MemDeregister failed: " << status);
    ret = H5FD_DSM_FAIL;
  }
  //
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::Get(H5FDdsmMsg *DataMsg)
{
  if (H5FDdsmCommMpi::Get(DataMsg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  H5FDdsmDebug("Getting " << DataMsg->Length << " Bytes from Address "
      << DataMsg->Address << " from Id = " << DataMsg->Source);
  // TBD
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::WindowSync()
{
  if (H5FDdsmCommMpi::WindowSync() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  MPI_Barrier(this->InterComm);
  //
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::Accept(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize)
{
  gni_return_t gni_status;
  H5FDdsmInt32 status;
  //
  // Create InterComm and initialize InterSize
  if (H5FDdsmCommMpi::Accept(storagePointer, storageSize) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  // Gather local NIC addresses and instance IDs
  // TODO We may not need to do that on the client side
  status = this->GatherIntraNicAddresses();
  if (status != H5FD_DSM_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " GatherIntraNicAddresses failed");
    return(H5FD_DSM_FAIL);
  }
  status = this->GatherIntraInstIds();
  if (status != H5FD_DSM_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " GatherIntraInstIds failed");
    return(H5FD_DSM_FAIL);
  }
  //
  // Send local NIC addresses and local ids
  for (H5FDdsmInt32 i = 0; i < this->InterSize; i++) {
    status = MPI_Send(this->CommUGniInternals->local_nic_addresses, this->IntraSize, MPI_UINT32_T,
        i, H5FD_DSM_EXCHANGE_TAG, this->InterComm);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Send of local NIC addresses failed");
      return(H5FD_DSM_FAIL);
    };
    status = MPI_Send(this->CommUGniInternals->local_inst_ids, this->IntraSize, MPI_INT32_T,
        i, H5FD_DSM_EXCHANGE_TAG, this->InterComm);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Send of local instance IDs failed");
      return(H5FD_DSM_FAIL);
    };
  }
  //
  // Create local MDH entry
  this->CommUGniInternals->local_mdh_entry.addr = (H5FDdsmAddr) storagePointer;
  H5FDdsmDebug("Register memory with the NIC");
  gni_status = GNI_MemRegister(this->CommUGniInternals->nic_handle, (uint64_t) storagePointer,
      storageSize, NULL, GNI_MEM_READWRITE, -1, &this->CommUGniInternals->local_mdh_entry.mdh);
  if (gni_status != GNI_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " GNI_MemRegister failed: " << gni_status);
    return(H5FD_DSM_FAIL);
  }
  this->CommUGniInternals->IsLocalMemRegistered = H5FD_DSM_TRUE;
  //
  // Send MDH entry to each remote PE
  // TODO During a dynamic connection, cannot use MPI_Send
  for (H5FDdsmInt32 i = 0; i < this->InterSize; i++) {
    status = MPI_Send(&this->CommUGniInternals->local_mdh_entry, sizeof(UGniMdhEntry), MPI_UNSIGNED_CHAR,
        i, H5FD_DSM_EXCHANGE_TAG, this->InterComm);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Send of MDH entry failed");
      return(H5FD_DSM_FAIL);
    };
  }
  //
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::Connect()
{
  gni_return_t gni_status;
  H5FDdsmInt32 status;
  MPI_Status mpi_status;
  //
  // Create InterComm and initialize InterSize
  if (H5FDdsmCommMpi::Connect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  // Get remote NIC addresses and instance IDs
  if (this->CommUGniInternals->remote_nic_addresses) free(this->CommUGniInternals->remote_nic_addresses);
  this->CommUGniInternals->remote_nic_addresses = (H5FDdsmUInt32*) malloc(this->InterSize * sizeof(H5FDdsmUInt32));
  if (this->CommUGniInternals->remote_inst_ids) free(this->CommUGniInternals->remote_inst_ids);
  this->CommUGniInternals->remote_inst_ids = (H5FDdsmInt32*) malloc(this->InterSize * sizeof(H5FDdsmInt32));

  for (H5FDdsmInt32 i = 0; i < this->InterSize; i++) {
    status = MPI_Recv(this->CommUGniInternals->remote_nic_addresses, this->InterSize, MPI_UINT32_T,
        i, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &mpi_status);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Send of local NIC addresses failed");
      return(H5FD_DSM_FAIL);
    };
    status = MPI_Recv(this->CommUGniInternals->remote_inst_ids, this->InterSize, MPI_INT32_T,
        i, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &mpi_status);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of local instance IDs failed");
      return(H5FD_DSM_FAIL);
    };
  }
  //
  // Get remote MDH entries
  if (this->CommUGniInternals->remote_mdh_entries) free(this->CommUGniInternals->remote_mdh_entries);
  this->CommUGniInternals->remote_mdh_entries = (UGniMdhEntry*) malloc(this->InterSize * sizeof(UGniMdhEntry));
  for (H5FDdsmInt32 i = 0; i < this->InterSize; i++) {
    status = MPI_Recv(&this->CommUGniInternals->remote_mdh_entries[i], sizeof(UGniMdhEntry), MPI_UNSIGNED_CHAR,
        i, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &mpi_status);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of MDH entry failed");
      return(H5FD_DSM_FAIL);
    };
  }
  //
  // Create end points with remote pairs
  // TODO better handle errors
  if (this->CommUGniInternals->ep_handles) free(this->CommUGniInternals->ep_handles);
  this->CommUGniInternals->ep_handles = (gni_ep_handle_t*) malloc(this->InterSize * sizeof(gni_ep_handle_t));

  if (this->CommUGniInternals->ep_bound) free(this->CommUGniInternals->ep_bound);
  this->CommUGniInternals->ep_bound = (H5FDdsmBoolean*) malloc(this->InterSize * sizeof(H5FDdsmBoolean));
  for (H5FDdsmInt32 i = 0; i < this->InterSize; i++) this->CommUGniInternals->ep_bound[i] = H5FD_DSM_FALSE;

  for (H5FDdsmInt32 i = 0; i < this->InterSize; i++) {
    H5FDdsmDebug("Create End Point with remote pair " << i);
    gni_status = GNI_EpCreate(this->CommUGniInternals->nic_handle, this->CommUGniInternals->cq_handle,
        &this->CommUGniInternals->ep_handles[i]);
    if (gni_status != GNI_RC_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " GNI_EpCreate failed: " << gni_status);
      return(H5FD_DSM_FAIL);
    }
    gni_status = GNI_EpBind(this->CommUGniInternals->ep_handles[i],
        this->CommUGniInternals->remote_nic_addresses[i],
        this->CommUGniInternals->remote_inst_ids[i]);
    if (gni_status != GNI_RC_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " GNI_EpBind failed: " << gni_status);
      return(H5FD_DSM_FAIL);
    }
    this->CommUGniInternals->ep_bound[i] = H5FD_DSM_TRUE;
  }
  //
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::Disconnect()
{
  gni_return_t status;
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  //
  if (H5FDdsmCommMpi::Disconnect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (this->CommUGniInternals->IsLocalMemRegistered) {
    status = GNI_MemDeregister(this->CommUGniInternals->nic_handle, &this->CommUGniInternals->local_mdh_entry.mdh);
    if (status != GNI_RC_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " GNI_MemDeregister failed: " << status);
      ret = H5FD_DSM_FAIL;
    }
  }
  //
  if (this->CommUGniInternals->ep_bound) {
    for (H5FDdsmInt32 i = 0; i < this->InterSize; i++) {
      if (this->CommUGniInternals->ep_bound[i] == H5FD_DSM_TRUE) {
        status = GNI_EpUnbind(this->CommUGniInternals->ep_handles[i]);
        if (status != GNI_RC_SUCCESS) {
          H5FDdsmError("Id = " << this->Id << " GNI_EpUnbind failed: " << status);
          ret = H5FD_DSM_FAIL;
          break;
        }
        status = GNI_EpDestroy(this->CommUGniInternals->ep_handles[i]);
        if (status != GNI_RC_SUCCESS) {
          H5FDdsmError("Id = " << this->Id << " GNI_EpDestroy failed: " << status);
          ret = H5FD_DSM_FAIL;
          break;
        }
        this->CommUGniInternals->ep_bound[i] = H5FD_DSM_FALSE;
      }
    }
  }
  //
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::PutFma(H5FDdsmMsg *DataMsg, gni_mem_handle_t src_mem_handle)
{
  gni_post_descriptor_t  fma_post_desc;
  gni_post_descriptor_t *event_post_desc_ptr;
  gni_cq_entry_t event_data;
  gni_return_t status;
  //
  fma_post_desc.type            = GNI_POST_FMA_PUT;
  fma_post_desc.cq_mode         = GNI_CQMODE_GLOBAL_EVENT;
  fma_post_desc.dlvr_mode       = GNI_DLVMODE_PERFORMANCE;
  fma_post_desc.local_addr      = (uint64_t) DataMsg->Data;
  fma_post_desc.local_mem_hndl  = src_mem_handle;
  fma_post_desc.remote_addr     = this->CommUGniInternals->remote_mdh_entries[DataMsg->Dest].addr;
  fma_post_desc.remote_addr    += DataMsg->Address;
  fma_post_desc.remote_mem_hndl = this->CommUGniInternals->remote_mdh_entries[DataMsg->Dest].mdh;
  fma_post_desc.length          = DataMsg->Length;
  //
  status = GNI_PostFma(this->CommUGniInternals->ep_handles[DataMsg->Dest], &fma_post_desc);
  if (status != GNI_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " GNI_EpPostFma failed: " << status);
    return(H5FD_DSM_FAIL);
  }
  status = GNI_CqWaitEvent(this->CommUGniInternals->cq_handle, -1, &event_data);
  if (status != GNI_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " GNI_CqWaitEvent failed: " << status);
    return(H5FD_DSM_FAIL);
  }
  status = GNI_GetCompleted(this->CommUGniInternals->cq_handle, event_data, &event_post_desc_ptr);
  if (status != GNI_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " GNI_GetCompleted failed: " << status);
    return(H5FD_DSM_FAIL);
  }
  //
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::GetFma(H5FDdsmMsg *DataMsg)
{
  // TBD
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::PutRdma(H5FDdsmMsg *DataMsg, gni_mem_handle_t src_mem_handle)
{
  gni_post_descriptor_t  rdma_post_desc;
  gni_post_descriptor_t *event_post_desc_ptr;
  gni_cq_entry_t event_data;
  gni_return_t status;
  //
  rdma_post_desc.type            = GNI_POST_RDMA_PUT;
  rdma_post_desc.cq_mode         = GNI_CQMODE_GLOBAL_EVENT;
  rdma_post_desc.dlvr_mode       = GNI_DLVMODE_PERFORMANCE;
  rdma_post_desc.local_addr      = (uint64_t) DataMsg->Data;
  rdma_post_desc.local_mem_hndl  = src_mem_handle;
  rdma_post_desc.remote_addr     = this->CommUGniInternals->remote_mdh_entries[DataMsg->Dest].addr;
  rdma_post_desc.remote_addr    += DataMsg->Address;
  rdma_post_desc.remote_mem_hndl = this->CommUGniInternals->remote_mdh_entries[DataMsg->Dest].mdh;
  rdma_post_desc.length          = DataMsg->Length;
  rdma_post_desc.src_cq_hndl     = this->CommUGniInternals->cq_handle;
  //
  status = GNI_PostRdma(this->CommUGniInternals->ep_handles[DataMsg->Dest], &rdma_post_desc);
  if (status != GNI_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " GNI_EpPostRdma failed: " << status);
    return(H5FD_DSM_FAIL);
  }
  status = GNI_CqWaitEvent(this->CommUGniInternals->cq_handle, -1, &event_data);
  if (status != GNI_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " GNI_CqWaitEvent failed: " << status);
    return(H5FD_DSM_FAIL);
  }
  status = GNI_GetCompleted(this->CommUGniInternals->cq_handle, event_data, &event_post_desc_ptr);
  if (status != GNI_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " GNI_GetCompleted failed: " << status);
    return(H5FD_DSM_FAIL);
  }
  //
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::GetRdma(H5FDdsmMsg *DataMsg)
{
  // TBD
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
// Methods below are derived from utility_functions.h (courtesy of Cray Inc.)
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::GetGniNicAddress(H5FDdsmInt32 device_id, H5FDdsmUInt32 *address)
{
  H5FDdsmInt32    alps_address = -1;
  H5FDdsmInt32    alps_dev_id = -1;
  H5FDdsmUInt32   cpu_id;
  gni_return_t    status;
  H5FDdsmInt32    i;
  H5FDdsmString   token, p_ptr;
  //
  p_ptr = getenv("PMI_GNI_DEV_ID");
  if (!p_ptr) {
    // Get the nic address for the specified device.
    if (GNI_CdmGetNicAddress(device_id, address, &cpu_id) != GNI_RC_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " GNI_CdmGetNicAddress failed: " << status);
      return(H5FD_DSM_FAIL);
    }
  } else {
    // Get the ALPS device id from the PMI_GNI_DEV_ID environment variable.
    while ((token = strtok(p_ptr, ":")) != NULL) {
      alps_dev_id = atoi(token);
      if (alps_dev_id == device_id) {
        break;
      }
      p_ptr = NULL;
    }

    if (alps_dev_id == -1) {
      H5FDdsmError("Id = " << this->Id << " Cannot retrieve ALPS device id");
      return(H5FD_DSM_FAIL);
    }

    p_ptr = getenv("PMI_GNI_LOC_ADDR");
    if (!p_ptr) {
      H5FDdsmError("Id = " << this->Id << " PMI_GNI_LOC_ADDR not set");
      return(H5FD_DSM_FAIL);
    }

    i = 0;

    // Get the nic address for the ALPS device.
    while ((token = strtok(p_ptr, ":")) != NULL) {
      if (i == alps_dev_id) {
        alps_address = atoi(token);
        break;
      }

      p_ptr = NULL;
      ++i;
    }

    if (alps_address == -1) {
      H5FDdsmError("Id = " << this->Id << " Cannot retrieve GNI NIC address");
      return(H5FD_DSM_FAIL);
    }
    *address = alps_address;
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::GatherIntraNicAddresses()
{
  H5FDdsmUInt32 local_addr;
  H5FDdsmInt32  status;
  //
  // Assuming a single gemini device.
  status = this->GetGniNicAddress(0, &local_addr);
  if (status != H5FD_DSM_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " GetGniNicAddress failed");
    return(H5FD_DSM_FAIL);
  }
  //
  // Allocate a buffer to hold the NIC addresses from all of the other ranks.
  if (this->CommUGniInternals->local_nic_addresses) free(this->CommUGniInternals->local_nic_addresses);
  this->CommUGniInternals->local_nic_addresses = (H5FDdsmUInt32 *) malloc(sizeof(H5FDdsmUInt32) * this->IntraSize);

  status = MPI_Allgather(&local_addr, 1, MPI_UINT32_T,
      this->CommUGniInternals->local_nic_addresses, 1, MPI_UINT32_T,
      this->IntraComm);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Gather of NIC addresses failed");
    return(H5FD_DSM_FAIL);
  }
  //
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::GatherIntraInstIds()
{
  H5FDdsmInt32 local_id;
  H5FDdsmInt32 status;
  //
  // Instance IDs are ranks given by PMI
  // TODO We need something better to dynamically join the communicators.
  MPI_Comm_rank(MPI_COMM_WORLD, &local_id);
  //
  // Allocate a buffer to hold the instance IDs from all of the other ranks.
  if (this->CommUGniInternals->local_inst_ids) free(this->CommUGniInternals->local_inst_ids);
  this->CommUGniInternals->local_inst_ids = (H5FDdsmInt32 *) malloc(sizeof(H5FDdsmInt32) * this->IntraSize);

  status = MPI_Allgather(&local_id, 1, MPI_INT32_T,
      this->CommUGniInternals->local_inst_ids, 1, MPI_INT32_T,
      this->IntraComm);
  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Gather of instance IDs failed");
    return(H5FD_DSM_FAIL);
  }
  //
  return(H5FD_DSM_SUCCESS);
}
