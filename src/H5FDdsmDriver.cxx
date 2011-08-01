/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmDriver.cxx

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
#include "H5FDdsmDriver.h"
#include "H5FDdsmComm.h"
#include "H5FDdsmMsg.h"
#include "H5FDdsmStorage.h"
#include "H5FDdsmStorageMpiRma.h"
#include "H5FDdsmAddressMapper.h"

// Align
typedef struct {
    H5FDdsmInt32 Opcode;
    H5FDdsmInt32 Source;
    H5FDdsmInt32 Target;
    H5FDdsmAddr  Address;
    H5FDdsmInt32 Length;
    H5FDdsmInt64 Parameters[10];
} H5FDdsmCommand;

//----------------------------------------------------------------------------
H5FDdsmDriver::H5FDdsmDriver()
{
    this->StartAddress = this->EndAddress = 0;
    this->StartServerId = this->EndServerId = -1;
    // For Alignment
    this->Length = 0;
    this->TotalLength = 0;
    this->BlockLength = 0;
    this->MaskLength  = 0;
    this->Storage = NULL;
    this->Comm = NULL;
    this->DataPointer = NULL;
    this->AddressMapper = new H5FDdsmAddressMapper(this);
}

//----------------------------------------------------------------------------
H5FDdsmDriver::~H5FDdsmDriver()
{
    if (this->Storage) delete this->Storage;
    this->Storage = NULL;
    delete this->AddressMapper;
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDriver::GetDsmType()
{
    return((this->AddressMapper) ? this->AddressMapper->GetDsmType() : H5FD_DSM_FAIL);
}
//----------------------------------------------------------------------------
void
H5FDdsmDriver::SetDsmType(H5FDdsmInt32 DsmType)
{
  this->AddressMapper->SetDsmType(DsmType);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDriver::SetMaskLength(H5FDdsmUInt64 dataSize)
{
  this->MaskLength = this->Length - (dataSize / (this->EndServerId - this->StartServerId + 1));
  // Do not make the mask fit exactly to the size of data to be written so that
  // additional space is left for metadata
  this->Length -= (this->MaskLength - H5FD_DSM_ALIGNMENT);
  this->TotalLength = this->Length * (this->EndServerId - this->StartServerId + 1);
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDriver::SetStorage(H5FDdsmStorage *aStorage)
{
    if (this->Storage) delete this->Storage;
    this->Storage = aStorage;
    this->DataPointer = (H5FDdsmByte *)this->Storage->GetDataPointer();
    return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDriver::ClearStorage()
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
H5FDdsmDriver::ConfigureUniform(H5FDdsmComm *aComm, H5FDdsmUInt64 aLength, H5FDdsmInt32 StartId, H5FDdsmInt32 EndId, 
  H5FDdsmUInt64 aBlockLength, H5FDdsmBoolean random)
{
    if (StartId < 0) StartId = 0;
    if (EndId < 0) EndId = aComm->GetIntraSize() - 1;
    this->SetDsmType(H5FD_DSM_TYPE_UNIFORM_RANGE);
    if ((StartId == 0) && (EndId == aComm->GetIntraSize() - 1)) {
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
    this->StartServerId = StartId;
    this->EndServerId = EndId;
    this->SetComm(aComm);
    if ((aComm->GetId() >= StartId) && (aComm->GetId() <= EndId)) {
        if (aBlockLength) {
          // For optimization we make the DSM length fit to a multiple of block size
          this->SetLength((H5FDdsmUInt64(aLength / aBlockLength)) * aBlockLength, 1);
        } else {
          this->SetLength(aLength, 1);
        }
        this->StartAddress = (aComm->GetId() - StartId) * aLength;
        this->EndAddress = this->StartAddress + aLength - 1;
    } else {
      if (aBlockLength) {
        this->Length = (H5FDdsmUInt64(aLength / aBlockLength)) * aBlockLength;
      } else {
        this->Length = aLength;
      }
    }
    this->TotalLength = this->GetLength() * (EndId - StartId + 1);
    return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDriver::SendDone()
{
    H5FDdsmInt32   who, status = H5FD_DSM_SUCCESS;

    for(who = this->StartServerId ; who <= this->EndServerId ; who++) {
      status = this->SendCommandHeader(H5FD_DSM_OPCODE_DONE, who, 0, 0);
      if (status != H5FD_DSM_SUCCESS) {
        H5FDdsmError("Cannot send termination command to DSM process " << who);
        return(status);
      }
    }
    return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDriver::SetLength(H5FDdsmUInt64 aLength, H5FDdsmBoolean AllowAllocate)
{
  if (!this->Storage) {
    if (this->Comm) {
      if (this->Comm->GetUseOneSidedComm()) {
        this->Storage = new H5FDdsmStorageMpiRma;
      } else {
        this->Storage = new H5FDdsmStorage;
      }
    } else {
      H5FDdsmError("Storage has not been initialized");
      return(H5FD_DSM_FAIL);
    }
  }
  if (this->Storage->SetLength(aLength, AllowAllocate) != H5FD_DSM_SUCCESS) {
    H5FDdsmError("Cannot set Dsm Length to " << Length);
    return(H5FD_DSM_FAIL);
  }
  this->Length = aLength;
  this->DataPointer = (H5FDdsmByte *)this->Storage->GetDataPointer();
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDriver::ProbeCommandHeader(H5FDdsmInt32 *Source)
{
  H5FDdsmInt32 status = H5FD_DSM_FAIL;
  H5FDdsmMsg Msg;

  Msg.SetTag(H5FD_DSM_COMMAND_TAG);
  status = this->Comm->Probe(&Msg);
  if (status != H5FD_DSM_FAIL) *Source = Msg.Source;
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDriver::SendCommandHeader(H5FDdsmInt32 Opcode, H5FDdsmInt32 Dest, H5FDdsmAddr Address, H5FDdsmInt32 aLength)
{
    H5FDdsmCommand  Cmd;
    H5FDdsmInt32 Status;

    H5FDdsmMsg Msg;

    memset(&Cmd, 0, sizeof(Cmd));
    Cmd.Opcode = Opcode;
    Cmd.Source = this->Comm->GetId();
    Cmd.Target = Dest;
    Cmd.Address = Address;
    Cmd.Length = aLength;

    Msg.SetSource(this->Comm->GetId());
    Msg.SetDest(Dest);
    Msg.SetTag(H5FD_DSM_COMMAND_TAG);
    Msg.SetLength(sizeof(Cmd));
    Msg.SetData(&Cmd);

    Status = this->Comm->Send(&Msg);
    H5FDdsmDebug("(" << this->Comm->GetId() << ") sent opcode " << Cmd.Opcode);
    return(Status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDriver::ReceiveCommandHeader(H5FDdsmInt32 *Opcode, H5FDdsmInt32 *Source,
    H5FDdsmAddr *Address, H5FDdsmInt32 *aLength, H5FDdsmInt32 IsRemoteService, H5FDdsmInt32 RemoteSource)
{
  H5FDdsmCommand  Cmd;
  H5FDdsmInt32    status = H5FD_DSM_FAIL;

  H5FDdsmMsg Msg;

  Msg.Source = (RemoteSource>=0) ? RemoteSource : H5FD_DSM_ANY_SOURCE;
  Msg.SetLength(sizeof(Cmd));
  Msg.SetTag(H5FD_DSM_COMMAND_TAG);
  Msg.SetData(&Cmd);

  memset(&Cmd, 0, sizeof(H5FDdsmCommand));

  if (IsRemoteService) {
    status  = this->Comm->Receive(&Msg, H5FD_DSM_INTER_COMM);
  } else {
    status  = this->Comm->Receive(&Msg);
  }
  if (status == H5FD_DSM_FAIL) {
    H5FDdsmError("Communicator Receive Failed");
    return(H5FD_DSM_FAIL);
  } else {
    *Opcode = Cmd.Opcode;
    *Source = Cmd.Source;
    *Address = Cmd.Address;
    *aLength = Cmd.Length;
    status = H5FD_DSM_SUCCESS;
    if (IsRemoteService) {
      H5FDdsmDebug("(Remote Service Server " << this->Comm->GetId() << ") got opcode " << Cmd.Opcode);
    } else {
      H5FDdsmDebug("(Server " << this->Comm->GetId() << ") got opcode " << Cmd.Opcode);
    }
  }
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDriver::SendInfo()
{
  H5FDdsmInfo dsmInfo;

  dsmInfo.type = this->GetDsmType();
  dsmInfo.length = this->GetLength();
  dsmInfo.total_length = this->GetTotalLength();
  dsmInfo.block_length = this->GetBlockLength();
  dsmInfo.start_server_id = this->GetStartServerId();
  dsmInfo.end_server_id = this->GetEndServerId();

  return(this->Comm->SendInfo(&dsmInfo));
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDriver::ReceiveInfo()
{
  H5FDdsmInfo dsmInfo;

  if (this->Comm->RecvInfo(&dsmInfo) != H5FD_DSM_SUCCESS) {
    return(H5FD_DSM_FAIL);
  }
  this->SetDsmType(dsmInfo.type);
  H5FDdsmDebug("Type received: " << this->GetDsmType());

  this->SetLength(dsmInfo.length, 0);
  H5FDdsmDebug("Length received: " << this->GetLength());

  this->TotalLength = dsmInfo.total_length;
  H5FDdsmDebug("totalLength received: " << this->GetTotalLength());

  this->SetBlockLength(dsmInfo.block_length);
  H5FDdsmDebug("blockLength received: " << this->GetBlockLength());

  this->StartServerId = dsmInfo.start_server_id;
  H5FDdsmDebug("startServerId received: " << this->GetStartServerId());

  this->EndServerId = dsmInfo.end_server_id;
  H5FDdsmDebug("endServerId received: " << this->GetEndServerId());

  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDriver::SendMaskLength()
{
  H5FDdsmInfo dsmInfo;

  dsmInfo.length = this->GetMaskLength();

  return(this->Comm->SendInfo(&dsmInfo));
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDriver::ReceiveMaskLength()
{
  H5FDdsmInfo dsmInfo;

  if (this->Comm->RecvInfo(&dsmInfo) != H5FD_DSM_SUCCESS) {
    return(H5FD_DSM_FAIL);
  }

  if (this->Comm->GetId() == 0) {
    H5FDdsmDebug("Old Length: " << this->GetLength());
    H5FDdsmDebug("Old Total Length: " << this->GetTotalLength());
  }
  this->MaskLength = dsmInfo.length;
  this->Length -= (this->MaskLength - H5FD_DSM_ALIGNMENT);
  this->TotalLength = this->Length * (this->EndServerId - this->StartServerId + 1);
  if (this->Comm->GetId() == 0) {
    H5FDdsmDebug("Mask Length received: " << this->GetMaskLength());
    H5FDdsmDebug("New Length: " << this->GetLength());
    H5FDdsmDebug("New Total Length: " << this->GetTotalLength());
  }

  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDriver::SendData(H5FDdsmInt32 Dest, H5FDdsmPointer Data,
    H5FDdsmInt32 aLength, H5FDdsmInt32 Tag, H5FDdsmAddr aAddress)
{
  H5FDdsmMsg Msg;

  Msg.SetSource(this->Comm->GetId());
  Msg.SetDest(Dest);
  Msg.SetLength(aLength);
  Msg.SetTag(Tag);
  Msg.SetAddress(aAddress);
  Msg.SetData(Data);
  if (this->Comm->GetUseOneSidedComm()) {
    return(this->Comm->Put(&Msg));
  }
  else {
    return(this->Comm->Send(&Msg));
  }
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmDriver::ReceiveData(H5FDdsmInt32 Source, H5FDdsmPointer Data,
    H5FDdsmInt32 aLength, H5FDdsmInt32 Tag, H5FDdsmAddr aAddress)
{
  H5FDdsmMsg Msg;

  Msg.SetSource(Source);
  Msg.SetLength(aLength);
  Msg.SetTag(Tag);
  Msg.SetAddress(aAddress);
  Msg.SetData(Data);
  if (this->Comm->GetUseOneSidedComm()) {
    return(this->Comm->Get(&Msg));
  }
  else {
    return(this->Comm->Receive(&Msg));
  }
}
