/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmBuffer.cxx

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
#include "H5FDdsmBuffer.h"
#include "H5FDdsmBufferService.h"
#include "H5FDdsmComm.h"
#include "H5FDdsmMsg.h"
#include "H5FDdsmStorage.h"
#include "H5FDdsmStorageMpi.h"
#include "H5FDdsmAddressMapper.h"

#ifdef _WIN32
#include <windows.h>
#include <io.h>
  #define access _access
  #define atoll _atoi64 
#else
#include <unistd.h>
#endif

//----------------------------------------------------------------------------
// Declare extra debug info 
#undef H5FDdsmDebugLevel
#ifdef H5FDdsm_DEBUG_GLOBAL
#define H5FDdsmDebugLevel(level, x) \
{ if (this->DebugLevel >= level) { \
    std::cout << "H5FD_DSM Debug Level " << level << ": " \
      << (this->IsServer ? "Server " : "Client ") << this->Comm->GetId() \
      << " : " << x << std::endl; \
  } \
}
#else
#define H5FDdsmDebugLevel(level, x) \
{ if (this->Debug && this->DebugLevel >= level) { \
    std::cout << "H5FD_DSM Debug Level " << level << ": " \
      << (this->IsServer ? "Server " : "Client ") << this->Comm->GetId() \
      << " : " << x << std::endl; \
  } \
}
#endif
//----------------------------------------------------------------------------

// Align
typedef struct {
  H5FDdsmInt32 Opcode;
  H5FDdsmInt32 Source;
  H5FDdsmInt32 Target;
  H5FDdsmAddr  Address;
  H5FDdsmInt32 Length;
  H5FDdsmInt64 Parameters[10];
} H5FDdsmCommand;

typedef struct {
  H5FDdsmInt32  type;
  H5FDdsmUInt64 length;
  H5FDdsmUInt64 total_length;
  H5FDdsmUInt64 block_length;
  H5FDdsmInt32  start_server_id;
  H5FDdsmInt32  end_server_id;
} H5FDdsmInfo;

//----------------------------------------------------------------------------
H5FDdsmBuffer::H5FDdsmBuffer()
{
  this->IsServer      = H5FD_DSM_TRUE;
  this->StartAddress  = this->EndAddress = 0;
  this->StartServerId = this->EndServerId = -1;
  // For Alignment
  this->Length = 0;
  this->TotalLength = 0;
  this->BlockLength = 0;
  this->Storage = NULL;
  this->Comm = NULL;
  this->DataPointer = NULL;
  this->AddressMapper = new H5FDdsmAddressMapper(this);
}

//----------------------------------------------------------------------------
H5FDdsmBuffer::~H5FDdsmBuffer()
{
  if (this->Storage) delete this->Storage;
  this->Storage = NULL;
  delete this->AddressMapper;
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBuffer::GetDsmType()
{
  return((this->AddressMapper) ? this->AddressMapper->GetDsmType() : H5FD_DSM_FAIL);
}
//----------------------------------------------------------------------------
void
H5FDdsmBuffer::SetDsmType(H5FDdsmInt32 dsmType)
{
  this->AddressMapper->SetDsmType(dsmType);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBuffer::SetStorage(H5FDdsmStorage *aStorage)
{
  if (this->Storage) delete this->Storage;
  this->Storage = aStorage;
  this->DataPointer = (H5FDdsmByte *)this->Storage->GetDataPointer();
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBuffer::ClearStorage()
{
  if (this->Storage) {
    H5FDdsmDebug("Resetting storage");
    H5FDdsmDebug("start address: " << this->StartAddress);
    H5FDdsmDebug("end address: " << this->EndAddress);
    this->SetLength(this->Length);
    this->DataPointer = (H5FDdsmByte *)this->Storage->GetDataPointer();
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBuffer::ConfigureUniform(H5FDdsmComm *aComm, H5FDdsmUInt64 aLength,
    H5FDdsmInt32 startId, H5FDdsmInt32 endId, H5FDdsmUInt64 aBlockLength,
    H5FDdsmBoolean random)
{
  if (startId < 0) startId = 0;
  if (endId < 0) endId = aComm->GetIntraSize() - 1;
  this->SetDsmType(H5FD_DSM_TYPE_UNIFORM_RANGE);
  if ((startId == 0) && (endId == aComm->GetIntraSize() - 1)) {
    this->SetDsmType(H5FD_DSM_TYPE_UNIFORM);
  }
  if (aBlockLength) {
    if (!random) {
      this->SetDsmType(H5FD_DSM_TYPE_BLOCK_CYCLIC);
    } else {
      this->SetDsmType(H5FD_DSM_TYPE_BLOCK_RANDOM);
    }
    this->SetBlockLength(aBlockLength);
  }
  this->StartServerId = startId;
  this->EndServerId = endId;
  this->SetComm(aComm);
  if ((aComm->GetId() >= startId) && (aComm->GetId() <= endId)) {
    if (aBlockLength) {
      // For optimization we make the DSM length fit to a multiple of block size
      this->SetLength((H5FDdsmUInt64(aLength / aBlockLength)) * aBlockLength);
    } else {
      this->SetLength(aLength);
    }
    this->StartAddress = (aComm->GetId() - startId) * aLength;
    this->EndAddress = this->StartAddress + aLength - 1;
  } else {
    if (aBlockLength) {
      this->Length = (H5FDdsmUInt64(aLength / aBlockLength)) * aBlockLength;
    } else {
      this->Length = aLength;
    }
  }
  this->TotalLength = this->GetLength() * (endId - startId + 1);
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBuffer::SetLength(H5FDdsmUInt64 aLength, H5FDdsmBoolean allowAllocate)
{
  if (!this->Storage) {
    if (this->Comm) {
      switch (this->Comm->GetInterCommType()) {
        case H5FD_DSM_COMM_MPI_RMA:
        case H5FD_DSM_COMM_DMAPP:
          this->Storage = new H5FDdsmStorageMpi;
          H5FDdsmDebug("Using MPI Storage...");
          break;
        default:
          this->Storage = new H5FDdsmStorage;
          break;
      }
    } else {
      H5FDdsmError("DSM communicator has not been initialized");
      return(H5FD_DSM_FAIL);
    }
  }
  if (this->Storage->SetLength(aLength, allowAllocate) != H5FD_DSM_SUCCESS) {
    H5FDdsmError("Cannot set DSM Length to " << Length);
    return(H5FD_DSM_FAIL);
  }
  this->Length = aLength;
  this->DataPointer = (H5FDdsmByte *)this->Storage->GetDataPointer();
  // If we are using one-sided communication, allocate here the local memory
  // window for one-sided accesses
  if (this->Comm->GetUseOneSidedComm()) {
    H5FDdsmUInt64 storageSize = allowAllocate ? this->Length : 0;
    this->Comm->WinCreateData(this->DataPointer, storageSize, H5FD_DSM_INTRA_COMM);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBuffer::SendCommandHeader(H5FDdsmInt32 opcode, H5FDdsmInt32 dest,
    H5FDdsmAddr address, H5FDdsmInt32 aLength, H5FDdsmInt32 comm)
{
  H5FDdsmCommand  cmd;
  H5FDdsmInt32 status;

  H5FDdsmMsg msg;

  memset(&cmd, 0, sizeof(H5FDdsmCommand));
  cmd.Opcode = opcode;
  cmd.Source = this->Comm->GetId();
  cmd.Target = dest;
  cmd.Address = address;
  cmd.Length = aLength;

  msg.SetSource(this->Comm->GetId());
  msg.SetDest(dest);
  msg.SetTag(H5FD_DSM_COMMAND_TAG);
  msg.SetLength(sizeof(H5FDdsmCommand));
  msg.SetData(&cmd);
  msg.SetCommunicator(comm);

  status = this->Comm->Send(&msg);
  H5FDdsmDebug("Sent opcode " << H5FDdsmOpcodeToString(cmd.Opcode) << " to "
      << dest << " on " << H5FDdsmCommToString(comm));
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBuffer::ReceiveCommandHeader(H5FDdsmInt32 *opcode, H5FDdsmInt32 *source,
    H5FDdsmAddr *address, H5FDdsmInt32 *aLength, H5FDdsmInt32 comm,
    H5FDdsmInt32 remoteSource)
{
  H5FDdsmCommand  cmd;
  H5FDdsmInt32    status = H5FD_DSM_FAIL;

  H5FDdsmMsg msg;

  msg.SetSource((remoteSource >= 0) ? remoteSource : H5FD_DSM_ANY_SOURCE);
  msg.SetLength(sizeof(H5FDdsmCommand));
  msg.SetTag(H5FD_DSM_COMMAND_TAG);
  msg.SetData(&cmd);
  msg.SetCommunicator(comm);

  memset(&cmd, 0, sizeof(H5FDdsmCommand));

  status  = this->Comm->Receive(&msg);

  if (status == H5FD_DSM_FAIL) {
    H5FDdsmError("Communicator Receive Failed");
    return(H5FD_DSM_FAIL);
  } else {
    *opcode  = cmd.Opcode;
    *source  = cmd.Source;
    *address = cmd.Address;
    *aLength = cmd.Length;
    status = H5FD_DSM_SUCCESS;

    H5FDdsmDebug("Got opcode " << H5FDdsmOpcodeToString(cmd.Opcode) << " from "
        << *source << " on " << H5FDdsmCommToString(comm));
  }
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBuffer::BroadcastComm(H5FDdsmInt32 *comm, H5FDdsmInt32 root)
{
  H5FDdsmMsg msg;
  H5FDdsmInt32 status;

  msg.SetSource(root);
  msg.SetLength(sizeof(H5FDdsmInt32));
  msg.SetData(comm);
  msg.SetCommunicator(H5FD_DSM_INTRA_COMM);

  status = this->Comm->Broadcast(&msg);
  if (status != H5FD_DSM_SUCCESS) H5FDdsmError("Broadcast of Comm failed");
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBuffer::SendData(H5FDdsmInt32 dest, H5FDdsmPointer data,
    H5FDdsmInt32 aLength, H5FDdsmInt32 tag, H5FDdsmAddr aAddress, H5FDdsmInt32 comm)
{
  H5FDdsmMsg msg;

  msg.SetSource(this->Comm->GetId());
  msg.SetDest(dest);
  msg.SetLength(aLength);
  msg.SetTag(tag);
  msg.SetAddress(aAddress);
  msg.SetData(data);
  msg.SetCommunicator(comm);
  if (this->Comm->GetUseOneSidedComm()) {
    return(this->Comm->PutData(&msg));
  } else {
    return(this->Comm->Send(&msg));
  }
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBuffer::ReceiveData(H5FDdsmInt32 source, H5FDdsmPointer data,
    H5FDdsmInt32 aLength, H5FDdsmInt32 tag, H5FDdsmAddr aAddress, H5FDdsmInt32 comm)
{
  H5FDdsmMsg msg;

  msg.SetSource(source);
  msg.SetLength(aLength);
  msg.SetTag(tag);
  msg.SetAddress(aAddress);
  msg.SetData(data);
  msg.SetCommunicator(comm);
  if (this->Comm->GetUseOneSidedComm()) {
    return(this->Comm->GetData(&msg));
  } else {
    return(this->Comm->Receive(&msg));
  }
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBuffer::SendAcknowledgment(H5FDdsmInt32 dest, H5FDdsmInt32 data,
    H5FDdsmInt32 tag, H5FDdsmInt32 comm)
{
  H5FDdsmInt32 status;
  H5FDdsmMsg   msg;

  msg.SetDest(dest);
  msg.SetLength(sizeof(H5FDdsmInt32));
  msg.SetTag(tag);
  msg.SetData(&data);
  msg.SetCommunicator(comm);

  status = this->Comm->Send(&msg);
  H5FDdsmDebugLevel(1,"Sent Acknowledgment Tag " << H5FDdsmTagToString(tag)
      << " to " << dest << " on " << H5FDdsmCommToString(comm));
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBuffer::ReceiveAcknowledgment(H5FDdsmInt32 source, H5FDdsmInt32 &data,
    H5FDdsmInt32 tag, H5FDdsmInt32 comm)
{
  H5FDdsmInt32 status;
  H5FDdsmMsg   msg;

  msg.SetSource(source);
  msg.SetLength(sizeof(H5FDdsmInt32));
  msg.SetTag(tag);
  msg.SetData(&data);
  msg.SetCommunicator(comm);

  status = this->Comm->Receive(&msg);
  H5FDdsmDebugLevel(1,"Received Acknowledgment " << H5FDdsmTagToString(tag)
      << " from " << source << " on " <<  H5FDdsmCommToString(comm));
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBuffer::SendInfo()
{
  H5FDdsmInfo  dsmInfo;
  H5FDdsmInt32 status;
  H5FDdsmMsg   msg;

  memset(&dsmInfo, 0, sizeof(H5FDdsmInfo));
  dsmInfo.type = this->GetDsmType();
  dsmInfo.length = this->GetLength();
  dsmInfo.total_length = this->GetTotalLength();
  dsmInfo.block_length = this->GetBlockLength();
  dsmInfo.start_server_id = this->GetStartServerId();
  dsmInfo.end_server_id = this->GetEndServerId();

  msg.SetSource(this->Comm->GetId());
  msg.SetDest(0);
  msg.SetTag(H5FD_DSM_EXCHANGE_TAG);
  msg.SetLength(sizeof(H5FDdsmInfo));
  msg.SetData(&dsmInfo);
  msg.SetCommunicator(H5FD_DSM_INTER_COMM);

  if (this->Comm->GetId() == 0)  {
    H5FDdsmDebug("Sending DSM Info...");
    status = this->Comm->Send(&msg);
    if (status != H5FD_DSM_SUCCESS) H5FDdsmError("Send of Info failed");
    H5FDdsmDebug("Send DSM Info Completed");
  }
  status = this->Comm->Barrier();
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBuffer::ReceiveInfo()
{
  H5FDdsmInfo  dsmInfo;
  H5FDdsmInt32 status = H5FD_DSM_FAIL;
  H5FDdsmMsg   msg;

  memset(&dsmInfo, 0, sizeof(H5FDdsmInfo));

  if (this->Comm->GetId() == 0) {
    msg.SetSource(0);
    msg.SetLength(sizeof(H5FDdsmInfo));
    msg.SetTag(H5FD_DSM_EXCHANGE_TAG);
    msg.SetData(&dsmInfo);
    msg.SetCommunicator(H5FD_DSM_INTER_COMM);

    H5FDdsmDebug("Receiving DSM Info...");
    status = this->Comm->Receive(&msg);
    if (status != H5FD_DSM_SUCCESS) H5FDdsmError("Receive of Info failed");
    H5FDdsmDebug("Recv DSM Info Completed");
  }

  msg.SetSource(0);
  msg.SetLength(sizeof(H5FDdsmInfo));
  msg.SetData(&dsmInfo);
  msg.SetCommunicator(H5FD_DSM_INTRA_COMM);

  status = this->Comm->Broadcast(&msg);
  if (status != H5FD_DSM_SUCCESS) H5FDdsmError("Broadcast of Info failed");

  this->SetDsmType(dsmInfo.type);
  // We are a client so don't allocate anything but only set a virtual remote length
  this->SetLength(dsmInfo.length, H5FD_DSM_FALSE);
  this->TotalLength = dsmInfo.total_length;
  this->SetBlockLength(dsmInfo.block_length);
  this->StartServerId = dsmInfo.start_server_id;
  this->EndServerId = dsmInfo.end_server_id;

  H5FDdsmDebug("Type received: " << this->GetDsmType());
  H5FDdsmDebug("Length received: " << this->GetLength());
  H5FDdsmDebug("totalLength received: " << this->GetTotalLength());
  H5FDdsmDebug("blockLength received: " << this->GetBlockLength());
  H5FDdsmDebug("startServerId received: " << this->GetStartServerId());
  H5FDdsmDebug("endServerId received: " << this->GetEndServerId());

  return(status);
}
