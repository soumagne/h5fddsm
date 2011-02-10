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

// Align
typedef struct {
    H5FDdsmInt32   Opcode;
    H5FDdsmInt32   Source;
    H5FDdsmInt32   Target;
    H5FDdsmInt64   Address;
    H5FDdsmInt32   Length;
    H5FDdsmInt64   Parameters[10];
} H5FDdsmCommand;

H5FDdsmDriver::H5FDdsmDriver() {
    this->DsmType = H5FD_DSM_TYPE_UNIFORM;
    this->Storage = new H5FDdsmStorage;
    this->StorageIsMine = 1;
    this->Locks = 0;
    // For Alignment
    this->Length = 0;
    this->DataPointer = (H5FDdsmByte *)this->Storage->GetDataPointer();
    this->StartAddress = this->EndAddress = 0;
    this->Comm = 0;
    this->StartServerId = this->EndServerId = -1;
}

H5FDdsmDriver::~H5FDdsmDriver() {
    if(this->Storage && this->StorageIsMine) delete this->Storage;
}

H5FDdsmInt32
H5FDdsmDriver::Copy(H5FDdsmDriver *Source){
    this->Debug = Source->Debug;
    this->DsmType = Source->DsmType;
    if (this->Storage) delete this->Storage;
    this->Storage = Source->GetStorage();
    this->StorageIsMine = 0;
    this->DataPointer = (H5FDdsmByte *)this->Storage->GetDataPointer();
    // For Alignment
    this->Length = Source->Length;
    this->TotalLength = Source->TotalLength;
    this->StartAddress = Source->StartAddress;
    this->EndAddress = Source->EndAddress;
    this->Comm = Source->Comm;
    this->StartServerId = Source->StartServerId;
    this->EndServerId = Source->EndServerId;
    this->Locks = Source->Locks;
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmDriver::SetStorage(H5FDdsmStorage *aStorage){
    if(this->Storage && this->StorageIsMine) delete this->Storage;
    this->Storage = aStorage;
    this->DataPointer = (H5FDdsmByte *)this->Storage->GetDataPointer();
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmDriver::ClearStorage() {
  if(this->Storage && this->StorageIsMine) {
    H5FDdsmDebug("Resetting storage");
    H5FDdsmDebug("start address: " << this->StartAddress);
    H5FDdsmDebug("end address: " << this->EndAddress);
    this->SetLength(this->Length);
    this->DataPointer = (H5FDdsmByte *)this->Storage->GetDataPointer();
  }
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmDriver::ConfigureUniform(H5FDdsmComm *aComm, H5FDdsmInt64 aLength, H5FDdsmInt32 StartId, H5FDdsmInt32 EndId){
    if(StartId < 0) StartId = 0;
    if(EndId < 0) EndId = aComm->GetTotalSize() - 1;
    this->SetDsmType(H5FD_DSM_TYPE_UNIFORM_RANGE);
    if((StartId == 0) && (EndId == aComm->GetTotalSize() - 1)){
        this->SetDsmType(H5FD_DSM_TYPE_UNIFORM);
    }
    this->SetStartServerId(StartId);
    this->SetEndServerId(EndId);
    this->SetComm(aComm);
    if((aComm->GetId() >= StartId) && (aComm->GetId() <= EndId)){
        this->Storage->SetComm(aComm);
        this->SetLength(aLength, 1);
        this->StartAddress = (aComm->GetId() - StartId) * aLength;
        this->EndAddress = this->StartAddress + aLength - 1;
    }else{
        this->Length = aLength;
    }
//    this->ServiceMsg->Source = this->Msg->Source = this->RemoteServiceMsg->Source = this->Comm->GetId();
    this->TotalLength = aLength * (EndId - StartId + 1);
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmDriver::GetAddressRangeForId(H5FDdsmInt32 Id, H5FDdsmInt64 *Start, H5FDdsmInt64 *End, H5FDdsmInt64 Address){
    switch(this->DsmType) {
        case H5FD_DSM_TYPE_UNIFORM :
        case H5FD_DSM_TYPE_UNIFORM_RANGE :
            // All Servers have same length
            *Start = (Id - this->StartServerId) * this->Length;
            *End = *Start + Length - 1;
            break;
        case H5FD_DSM_TYPE_BLOCK_CYCLIC :
          *Start = ((H5FDdsmInt32)(Address / H5FD_DSM_BLOCK_LENGTH)) * H5FD_DSM_BLOCK_LENGTH;
          *End = ((H5FDdsmInt32)(Address / H5FD_DSM_BLOCK_LENGTH) + 1) * H5FD_DSM_BLOCK_LENGTH - 1;
          break;
        default :
            // Not Implemented
            H5FDdsmError("DsmType " << this->DsmType << " not yet implemented");
            return(H5FD_DSM_FAIL);
            break;
    }
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmDriver::AddressToId(H5FDdsmInt64 Address){
    H5FDdsmInt32 ServerId = H5FD_DSM_FAIL;

    switch(this->DsmType) {
        case H5FD_DSM_TYPE_UNIFORM :
        case H5FD_DSM_TYPE_UNIFORM_RANGE :
            // All Servers have same length
            ServerId = this->StartServerId + (H5FDdsmInt32)(Address / this->Length);
            if(ServerId > this->EndServerId ){
                H5FDdsmError("ServerId " << ServerId << " for Address " << Address << " is larger than EndServerId " << this->EndServerId);
            }
            break;
        case H5FD_DSM_TYPE_BLOCK_CYCLIC :
          // Keep a uniform DSM but add block cyclic distribution
          if (Address > this->EndAddress*(this->EndServerId - this->StartServerId + 1)) {
            H5FDdsmError("Address " << Address << " is larger than end address of EndServerId " << this->EndServerId);
          }
          ServerId = this->StartServerId + ((H5FDdsmInt32)(Address / H5FD_DSM_BLOCK_LENGTH) % (this->EndServerId - this->StartServerId + 1));
          break;
        default :
            // Not Implemented
            H5FDdsmError("DsmType " << this->DsmType << " not yet implemented");
            break;
    }
    return(ServerId);
}

H5FDdsmInt32
H5FDdsmDriver::SendDone(){
    H5FDdsmInt32   who, status = H5FD_DSM_SUCCESS;

    switch(this->DsmType) {
        case H5FD_DSM_TYPE_UNIFORM :
        case H5FD_DSM_TYPE_UNIFORM_RANGE :
            for(who = this->StartServerId ; who <= this->EndServerId ; who++){
                status = this->SendCommandHeader(H5FD_DSM_OPCODE_DONE, who, 0, 0);
            }
            break;
        default :
            // Not Implemented
            H5FDdsmError("DsmType " << this->DsmType << " not yet implemented");
            break;
    }
    return(status);
}

H5FDdsmInt32
H5FDdsmDriver::SetLength(H5FDdsmInt64 aLength, H5FDdsmBoolean AllowAllocate){
    if(this->Storage->SetNumberOfElements(aLength, AllowAllocate) != H5FD_DSM_SUCCESS) {
        H5FDdsmError("Cannot set Dsm Length to " << Length);
        return(H5FD_DSM_FAIL);
    }
    this->Length = aLength;
    this->DataPointer = (H5FDdsmByte *)this->Storage->GetDataPointer();
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmDriver::ProbeCommandHeader(H5FDdsmInt32 *Source){
  H5FDdsmInt32 status = H5FD_DSM_FAIL;
  H5FDdsmMsg Msg;

  Msg.SetTag(H5FD_DSM_COMMAND_TAG);
  status = this->Comm->Probe(&Msg);
  if (status != H5FD_DSM_FAIL) *Source = Msg.Source;
  return(status);
}

H5FDdsmInt32
H5FDdsmDriver::SendCommandHeader(H5FDdsmInt32 Opcode, H5FDdsmInt32 Dest, H5FDdsmInt64 Address, H5FDdsmInt32 aLength){
    H5FDdsmCommand  Cmd;
    H5FDdsmInt32 Status;

    H5FDdsmMsg Msg;

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

H5FDdsmInt32
H5FDdsmDriver::ReceiveCommandHeader(H5FDdsmInt32 *Opcode, H5FDdsmInt32 *Source, H5FDdsmInt64 *Address, H5FDdsmInt32 *aLength, H5FDdsmInt32 IsRemoteService, H5FDdsmInt32 RemoteSource, H5FDdsmInt32 Block){
    H5FDdsmCommand  Cmd;
    H5FDdsmInt32       status = H5FD_DSM_FAIL;

    H5FDdsmMsg Msg;

    Msg.Source = (RemoteSource>=0) ? RemoteSource : H5FD_DSM_ANY_SOURCE;
    Msg.SetLength(sizeof(Cmd));
    Msg.SetTag(H5FD_DSM_COMMAND_TAG);
    Msg.SetData(&Cmd);

    memset(&Cmd, 0, sizeof(H5FDdsmCommand));
    // TODO Do not probe by default
    status = this->Comm->Probe(&Msg);
    if ((status != H5FD_DSM_FAIL) || Block){
        if (IsRemoteService) {
          status  = this->Comm->Receive(&Msg, H5FD_DSM_COMM_CHANNEL_REMOTE);
        } else {
          status  = this->Comm->Receive(&Msg);
        }
        if (status == H5FD_DSM_FAIL){
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
    }
    return(status);
}

H5FDdsmInt32
H5FDdsmDriver::SendData(H5FDdsmInt32 Dest, void *Data, H5FDdsmInt32 aLength, H5FDdsmInt32 Tag){

    H5FDdsmMsg Msg;

    Msg.SetSource(this->Comm->GetId());
    Msg.SetDest(Dest);
    Msg.SetLength(aLength);
    Msg.SetTag(Tag);
    Msg.SetData(Data);
    return(this->Comm->Send(&Msg));
}

H5FDdsmInt32
H5FDdsmDriver::ReceiveData(H5FDdsmInt32 Source, void *Data, H5FDdsmInt32 aLength, H5FDdsmInt32 Tag, H5FDdsmInt32 Block){
    H5FDdsmInt32   Status = H5FD_DSM_FAIL;

    H5FDdsmMsg Msg;

    Msg.SetSource(Source);
    Msg.SetLength(aLength);
    Msg.SetTag(Tag);
    Msg.SetData(Data);
    // TODO Do not probe by default
    if(Block){
        Status = this->Comm->Receive(&Msg);
    }else{
        Status = this->Comm->Probe(&Msg);
        if(Status == H5FD_DSM_SUCCESS){
            Status = this->Comm->Receive(&Msg);
        }
    }
    return(Status);
}

H5FDdsmInt32
H5FDdsmDriver::PutData(H5FDdsmInt32 Dest, void *Data, H5FDdsmInt32 aLength, H5FDdsmInt64 aAddress){

    H5FDdsmMsg Msg;

    Msg.SetSource(this->Comm->GetId());
    Msg.SetDest(Dest);
    Msg.SetLength(aLength);
    Msg.SetAddress(aAddress);
    Msg.SetData(Data);
    return(this->Comm->PutData(&Msg));
}

H5FDdsmInt32
H5FDdsmDriver::GetData(H5FDdsmInt32 Source, void *Data, H5FDdsmInt32 aLength, H5FDdsmInt64 aAddress){

    H5FDdsmMsg Msg;

    Msg.SetSource(Source);
    Msg.SetLength(aLength);
    Msg.SetAddress(aAddress);
    Msg.SetData(Data);
    return(this->Comm->GetData(&Msg));
}
