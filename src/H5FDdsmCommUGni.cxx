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
#include <vector>

#include <gni_libonesided_interop.h>

#define GNI_RDMA_OFFLOAD_THRESHOLD 0
#define GNI_FMA_OFFLOAD_THRESHOLD 0

struct H5FDdsmCommUGniInternals
{
  H5FDdsmCommUGniInternals()
  {
    this->ep_handles = NULL;
  }
  ~H5FDdsmCommUGniInternals()
  {
    if (this->ep_handles) free(this->ep_handles);
    this->ep_handles = NULL;
  }

  struct UGniMdhEntry
  {
    H5FDdsmInt32     pe;
    H5FDdsmAddr      addr;
    gni_mem_handle_t mdh;
  };

  typedef std::vector<UGniMdhAddr> UGniMdhEntries;
  //
  gni_nic_handle_t  nic_handle; // NIC handle used for the communication
  gni_cq_handle_t   cq_handle;
  gni_ep_handle_t  *ep_handles;
  UGniMdhEntries    mdh_entries;
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
  delete this->CommUGniInternals;
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::Init()
{
  int nhandles;
  gni_nic_handle_t *nic_hndls;
  gni_return_t status;
  //
  if (H5FDdsmCommMpi::Init() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
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
  memcpy(&this->CommUGniInternals->nic_handle, nic_hndls[0], sizeof(gni_nic_handle_t));
  free(nic_hndls);

  // init NTT index table
//  ntt_index_table = malloc(numprocs * sizeof(H5FDdsmInt32));
//  MPI_Allgather(&local_ntt_index, 1, MPI_INT, ntt_index_table, 1, MPI_INT,
//      MPI_COMM_WORLD);

  // Create a completion queue
  H5FDdsmDebug("Creating a new CQ");
  status = GNI_CqCreate(this->CommUGniInternals->nic_handle, 10, 0, GNI_CQ_BLOCKING,
      NULL, NULL, &this->CommUGniInternals->cq_handle);
  if (status != GNI_RC_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " GNI_CqCreate failed: " << status);
    return(H5FD_DSM_FAIL);
  }

  H5FDdsmDebug("CommUGni initialized");
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::Put(H5FDdsmMsg *DataMsg)
{
  gni_mem_handle_t src_mem_handle;
  gni_return_t status;
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
    this->PutRdma(DataMsg, src_mem_handle);
  }
  else if (DataMsg->Length > GNI_FMA_OFFLOAD_THRESHOLD) {
    this->PutFma(DataMsg, src_mem_handle);
  }

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::Get(H5FDdsmMsg *DataMsg)
{
  if (H5FDdsmCommMpi::Get(DataMsg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

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

  MPI_Barrier(this->InterComm);

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::Accept(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize)
{
  // Create an MPI InterComm
  if (H5FDdsmCommMpi::Accept(storagePointer, storageSize) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::Connect()
{
  // Create an MPI InterComm
  if (H5FDdsmCommMpi::Connect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  // Create a logical EndPoint and bind it to each remote NIC
  H5FDdsmDebug("Creating a new logical EP");
  status = GNI_EpCreate(this->CommUGniInternals->nic_handle, this->CommUGniInternals->cq_handle,
      &this->CommUGniInternals->ep_handles[i]);
  if (status != GNI_RC_SUCCESS) {
    fprintf(stderr, " GNI_EpCreate failed: %d\n", status);
    exit(1);
  }
  remote_id = numprocs - myid - 1;
  if (verb) {
    printf("(%d) Binding the EP created to %d (NTT index: %d)\n", myid,
        remote_id, ntt_index_table[remote_id]);
  }
  status = GNI_EpBind(ep_handle, ntt_index_table[remote_id], remote_id);
  if (status != GNI_RC_SUCCESS) {
    fprintf(stderr, " GNI_EpBind failed: %d\n", status);
    exit(1);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommUGni::Disconnect()
{
  if (H5FDdsmCommMpi::Disconnect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  return(H5FD_DSM_SUCCESS);
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
  fma_post_desc.remote_addr     = this->CommUGniInternals->mdh_entries[DataMsg->Dest].addr;
  fma_post_desc.remote_addr    += DataMsg->Address;
  fma_post_desc.remote_mem_hndl = this->CommUGniInternals->mdh_entries[DataMsg->Dest].hndl;
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
  rdma_post_desc.remote_addr     = this->CommUGniInternals->mdh_entries[DataMsg->Dest].addr;
  rdma_post_desc.remote_addr    += DataMsg->Address;
  rdma_post_desc.remote_mem_hndl = this->CommUGniInternals->mdh_entries[DataMsg->Dest].hndl;
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
