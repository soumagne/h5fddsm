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
#include "H5FDdsmComm.h"
#include "H5FDdsmMsg.h"
#include <algorithm>

#define H5FD_DSM_OPCODE_PUT          0x01
#define H5FD_DSM_OPCODE_GET          0x02
#define H5FD_DSM_SEMA_AQUIRE         0x03
#define H5FD_DSM_SEMA_RELEASE        0x04
#define H5FD_DSM_REMOTE_CHANNEL      0x05
#define H5FD_DSM_LOCAL_CHANNEL       0x06
#define H5FD_DSM_DISCONNECT          0x07
#define H5FD_DSM_XML_EXCHANGE        0x08
#define H5FD_DSM_CLEAR_STORAGE       0x09
#define H5FD_DSM_PIPELINE_UPDATE     0x10

int ForceCommunication = 0;

extern "C"{
H5FDdsm_EXPORT void *
H5FDdsmBufferServiceThread(void *DsmObj){
    H5FDdsmBuffer *Dsm = (H5FDdsmBuffer *)DsmObj;
    return(Dsm->ServiceThread());
}
}

H5FDdsmBuffer::H5FDdsmBuffer() {
    H5FDdsmInt64 i;
    this->ThreadDsmReady = 0;
    this->DataPointer = 0;
    this->IsAutoAllocated = false;
    this->IsServer = true;
    this->IsConnected = false;
    this->IsUpdateReady = false;
    this->IsReadOnly = false;
    this->Locks = new H5FDdsmInt64[H5FD_DSM_MAX_LOCKS];
    for(i=0;i < H5FD_DSM_MAX_LOCKS;i++) this->Locks[i] = -1;

    this->ServiceThreadUseCopy = 1;
    this->XMLDescription = NULL;

    this->Steerer = new H5FDdsmSteerer();
}

H5FDdsmBuffer::~H5FDdsmBuffer() {
    if (this->StorageIsMine) delete[] this->Locks;
    if (this->XMLDescription) delete[] this->XMLDescription;
    if (this->Steerer) delete this->Steerer;
}

H5FDdsmInt32
H5FDdsmBuffer::ClearStorage()
{
  if (H5FDdsmDriver::ClearStorage() != H5FD_DSM_SUCCESS) return H5FD_DSM_FAIL;
  return H5FD_DSM_SUCCESS;
}

H5FDdsmInt32
H5FDdsmBuffer::SetComm(H5FDdsmComm *comm)
{
  if (H5FDdsmDriver::SetComm(comm) != H5FD_DSM_SUCCESS) return H5FD_DSM_FAIL;
  if (this->Steerer) this->Steerer->SetComm(comm);
  return H5FD_DSM_SUCCESS;
}

/*
H5FDdsmInt32
H5FDdsmBuffer::Copy(H5FDdsmBuffer *Source){
    cout << "Copying" << endl;
    if (H5FDdsm::Copy((H5FDdsm *)Source) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
    cout << "Setting locks to " << Source->Locks << endl;
    this->Locks = Source->Locks;
    return(H5FD_DSM_SUCCESS);
}
*/

void *
H5FDdsmBuffer::ServiceThread(){
  H5FDdsmInt32   ReturnOpcode;

  if (this->ServiceThreadUseCopy) {
    // Create a copy of myself to get a Unique H5FDdsmMessage
    H5FDdsmBuffer   UniqueBuffer;

    if (UniqueBuffer.Locks) delete[] UniqueBuffer.Locks;
    UniqueBuffer.Copy(this);
    H5FDdsmDebug("Starting DSM Service on node " << UniqueBuffer.GetComm()->GetId());
    this->ThreadDsmReady = 1;
    UniqueBuffer.ServiceLoop(&ReturnOpcode);
    this->ThreadDsmReady = 0;
    H5FDdsmDebug("Ending DSM Service on node " << UniqueBuffer.GetComm()->GetId() << " last op = " << ReturnOpcode);
  } else {
    // Do not use copy in order to use shared memory space
    H5FDdsmDebug("Starting DSM Service on node " << this->Comm->GetId());
    this->ThreadDsmReady = 1;
    this->ServiceLoop(&ReturnOpcode);
    this->ThreadDsmReady = 0;
    H5FDdsmDebug("Ending DSM Service on node " << this->Comm->GetId() << " last op = " << ReturnOpcode);
  }
  return((void *)this);
}

H5FDdsmInt32
H5FDdsmBuffer::ServiceInit(){
    H5FDdsmInt32   status = H5FD_DSM_SUCCESS;

    return(status);
}

H5FDdsmInt32
H5FDdsmBuffer::ServiceOnce(H5FDdsmInt32 *ReturnOpcode){
    H5FDdsmInt32   status = H5FD_DSM_FAIL;

    // this->Msg->SetTag(H5FD_DSM_COMMAND_TAG);
    // status = this->Comm->Check(this->Msg);
    if (status != H5FD_DSM_SUCCESS){
        // Nothing to do
        return(H5FD_DSM_SUCCESS);
    }
    // Service One Call
    H5FDdsmDebug(".... Service a Call");
    return(this->Service(ReturnOpcode));
}

H5FDdsmInt32
H5FDdsmBuffer::ServiceUntilIdle(H5FDdsmInt32 *ReturnOpcode){
    H5FDdsmInt32   status = H5FD_DSM_SUCCESS;

    while(status == H5FD_DSM_SUCCESS){
        // this->Msg->SetTag(H5FD_DSM_COMMAND_TAG);
        // status = this->Comm->Check(this->Msg);
        if (status != H5FD_DSM_SUCCESS){
            // Nothing to do
            return(H5FD_DSM_SUCCESS);
        }
        // Service One Call
        status = this->Service(ReturnOpcode);
        if (status != H5FD_DSM_SUCCESS){
            H5FDdsmError("ServiceUntilIdle detected error in Service() Method");
            return(H5FD_DSM_FAIL);
        }
    }
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmBuffer::ServiceLoop(H5FDdsmInt32 *ReturnOpcode){
    H5FDdsmInt32   op, status = H5FD_DSM_SUCCESS;

    while(status == H5FD_DSM_SUCCESS){
        status = this->Service(&op);
        if (status != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
        if (ReturnOpcode) *ReturnOpcode = op;
        if (op == H5FD_DSM_OPCODE_DONE) return(H5FD_DSM_SUCCESS);
    }
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmBuffer::Service(H5FDdsmInt32 *ReturnOpcode){
    H5FDdsmInt32   Opcode, who, value, status = H5FD_DSM_FAIL;
    H5FDdsmInt64         aLength;
    H5FDdsmInt64       Address;
    H5FDdsmByte   *datap;
    H5FDdsmInt32   IsService = 1;

    status = this->ReceiveCommandHeader(&Opcode, &who, &Address, &aLength);
    if (status == H5FD_DSM_FAIL){
        H5FDdsmError("Error Receiving Command Header");
        return(H5FD_DSM_FAIL);
    }
    switch(Opcode){
        case H5FD_DSM_OPCODE_PUT :
            H5FDdsmDebug("PUT request from " << who << " for " << aLength << " bytes @ " << Address);
            if (aLength > (this->EndAddress - Address + 1)){
                H5FDdsmError("Length too long");
                return(H5FD_DSM_FAIL);
            }
            datap = this->DataPointer;
            datap += Address - this->StartAddress;
            status = this->ReceiveData(who, datap, aLength, H5FD_DSM_PUT_DATA_TAG, IsService);
            if (status == H5FD_DSM_FAIL){
                H5FDdsmError("ReceiveData() failed");
                return(H5FD_DSM_FAIL);
            }
            H5FDdsmDebug("Serviced PUT request from " << who << " for " << aLength << " bytes @ " << Address);
            break;
        case H5FD_DSM_OPCODE_GET :
            H5FDdsmDebug("(Server " << this->Comm->GetId() << ") Get request from " << who << " for " << aLength << " bytes @ " << Address);
            if (aLength > (this->EndAddress - Address + 1)){
                H5FDdsmError("Length " << aLength << " too long for address of len " << this->EndAddress - Address);
                H5FDdsmError("Server Start = " << this->StartAddress << " End = " << this->EndAddress);
                return(H5FD_DSM_FAIL);
            }
            if ((datap = this->DataPointer) == NULL)
              H5FDdsmError("Null Data Pointer when trying to get data");
            datap += Address - this->StartAddress;
            status = this->SendData(who, datap, aLength, H5FD_DSM_GET_DATA_TAG, IsService);
            if (status == H5FD_DSM_FAIL){
                H5FDdsmError("SendData() failed");
                return(H5FD_DSM_FAIL);
            }
            H5FDdsmDebug("(Server " << this->Comm->GetId() << ") Serviced GET request from " << who << " for " << aLength << " bytes @ " << Address);
            break;
        case H5FD_DSM_SEMA_AQUIRE :
            H5FDdsmDebug("Sema " << Address << " Aquire");
            if ((Address < 0) || (Address >= H5FD_DSM_MAX_LOCKS)){
                H5FDdsmError("Invalid Sema Request " << Address);
                value = H5FD_DSM_FAIL;
            }else{
                H5FDdsmDebug("Server Locks[" << Address << "] = " << this->Locks[Address]);
                if (this->Locks[Address] == -1){
                    H5FDdsmDebug("Remote " << who << " Aquired Lock " << Address);
                    this->Locks[Address] = who;
                    value = H5FD_DSM_SUCCESS;
                }else{
                    H5FDdsmDebug("Remote " << who << " did not Aquired Lock " << Address << " already locked by " << this->Locks[Address]);
                    value = H5FD_DSM_FAIL;
                }
            }
            status = this->SendData(who, (H5FDdsmByte *)&value, sizeof(H5FDdsmInt32), H5FD_DSM_RESPONSE_TAG, IsService);
            if (status == H5FD_DSM_FAIL){
                H5FDdsmError("SemaAquire Response Failed");
                return(H5FD_DSM_FAIL);
            }
            break;
        case H5FD_DSM_SEMA_RELEASE :
            H5FDdsmDebug("Sema " << Address << " Release");
            if ((Address < 0) || (Address >= H5FD_DSM_MAX_LOCKS)){
                H5FDdsmError("Invalid Sema Request " << Address);
                value = H5FD_DSM_FAIL;
            }else{
                if (this->Locks[Address] == who){
                    H5FDdsmDebug("P(" << who << ")" << " Released Lock " << Address);
                    this->Locks[Address] = -1;
                    value = H5FD_DSM_SUCCESS;
                }else{
                    H5FDdsmDebug("P(" << who << ")" << " did not Release Lock " << Address << " already locked by " << this->Locks[Address]);
                    value = H5FD_DSM_FAIL;
                }
            }
            status = this->SendData(who, (H5FDdsmByte *)&value, sizeof(H5FDdsmInt32), H5FD_DSM_RESPONSE_TAG, IsService);
            if (status == H5FD_DSM_FAIL){
                H5FDdsmError("SemaAquire Response Failed");
                return(H5FD_DSM_FAIL);
            }
            break;
        case H5FD_DSM_OPCODE_DONE :
            break;
        case H5FD_DSM_REMOTE_CHANNEL:
          H5FDdsmDebug("Switching to Remote channel");
          if (!this->IsConnected) {
            this->Comm->RemoteCommAccept();
            // send DSM information
            this->Comm->RemoteCommSendInfo(&this->Length, &this->TotalLength, &this->StartServerId, &this->EndServerId);
            this->IsConnected = true;
          }
          this->Comm->SetCommChannel(H5FD_DSM_COMM_CHANNEL_REMOTE);
          this->Comm->RemoteCommSendReady();
          // TODO Send steering order here for now, the previous ready should now go away
          this->Steerer->SendSteeringCommands();
          H5FDdsmDebug("Switched to Remote channel");
          break;
        case H5FD_DSM_LOCAL_CHANNEL: // Should be used only when going back to remote after that
          H5FDdsmDebug("Switching to Local channel");
          this->Comm->SetCommChannel(H5FD_DSM_COMM_CHANNEL_LOCAL);
          H5FDdsmDebug("Switched to Local channel");
          break;
        case H5FD_DSM_DISCONNECT:
          H5FDdsmDebug("( " << this->Comm->GetId() << " ) Freeing now remote channel");
          this->Comm->RemoteCommDisconnect();
          this->SetIsConnected(false);
          H5FDdsmDebug("DSM disconnected on " << this->Comm->GetId() << ", Switched to Local channel");
          break;
        case H5FD_DSM_XML_EXCHANGE:
          // Receive XML File and put it in a DSM buffer field for further reading
          this->Comm->RemoteCommRecvXML(&this->XMLDescription);
          break;
        case H5FD_DSM_CLEAR_STORAGE:
          H5FDdsmDebug("(" << this->Comm->GetId() << ") " << "Clearing DSM");
          this->Comm->Barrier();
          this->ClearStorage();
          this->Comm->Barrier();
          break;
        case H5FD_DSM_PIPELINE_UPDATE:
          if (!this->Comm->HasStillData() || !this->IsConnected) {
            this->Comm->Barrier();
            this->Comm->SetCommChannel(H5FD_DSM_COMM_CHANNEL_LOCAL);
            this->Comm->Barrier();
            this->SetIsUpdateReady(true);
            this->Comm->Barrier();
            H5FDdsmDebug("(" << this->Comm->GetId() << ") " << "IsUpdateReady, Switched to Local channel");
          }
          break;
        default :
            H5FDdsmError("Unknown Opcode " << Opcode);
            return(H5FD_DSM_FAIL);
    }
    if (ReturnOpcode) *ReturnOpcode = Opcode;
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmBuffer::Aquire(H5FDdsmInt64 Index){
    H5FDdsmInt32   who, MyId = this->Comm->GetId();
    H5FDdsmInt32   RemoteStatus;

    who = this->AddressToId(0);
    H5FDdsmDebug("Aquire :: MyId = " << MyId << " who = " << who);
    if (who == H5FD_DSM_FAIL){
        H5FDdsmError("Address Error");
        return(H5FD_DSM_FAIL);
    }
    if ((Index < 0) || (Index >= H5FD_DSM_MAX_LOCKS)){
        H5FDdsmError("Invalid Sema Request " << Index);
        return(H5FD_DSM_FAIL);
    }
    if (who == MyId){
        H5FDdsmDebug("Local Locks[" << Index << "] = " << this->Locks[Index]);
        if ((this->Locks[Index] == -1) || (this->Locks[Index] == MyId)){
            this->Locks[Index] = MyId;
            H5FDdsmDebug("P(" << who << ")" << " Aquired own lock " << Index);
            return(H5FD_DSM_SUCCESS);
        }else{
            H5FDdsmDebug("P(" << who << ")" << " did not Aquired own lock " << Index);
            return(H5FD_DSM_FAIL);
        }
    }else{
        H5FDdsmInt32   status;

        H5FDdsmDebug("Sending Header");
        status = this->SendCommandHeader(H5FD_DSM_SEMA_AQUIRE, who, Index, sizeof(H5FDdsmInt64));
        if (status == H5FD_DSM_FAIL){
            H5FDdsmError("Failed to send Aquire Header to " << who);
            return(H5FD_DSM_FAIL);
        }
        H5FDdsmDebug("Getting Response");
        status = this->ReceiveData(who, &RemoteStatus, sizeof(H5FDdsmInt32), H5FD_DSM_RESPONSE_TAG);
        if (status == H5FD_DSM_FAIL){
            H5FDdsmError("Failed to Aquire " << Index << " Response From " << who);
            return(H5FD_DSM_FAIL);
        }
        H5FDdsmDebug("RemoteStatus = " << RemoteStatus);
        return(RemoteStatus);
    }
    return(H5FD_DSM_FAIL);
}

H5FDdsmInt32
H5FDdsmBuffer::Release(H5FDdsmInt64 Index){
    H5FDdsmInt32   who, MyId = this->Comm->GetId();
    H5FDdsmInt32   RemoteStatus;

    who = this->AddressToId(0);
    if (who == H5FD_DSM_FAIL){
        H5FDdsmError("Address Error");
        return(H5FD_DSM_FAIL);
    }
    if ((Index < 0) || (Index >= H5FD_DSM_MAX_LOCKS)){
        H5FDdsmError("Invalid Sema Request " << Index);
        return(H5FD_DSM_FAIL);
    }
    if (who == MyId){
        if ((this->Locks[Index] == -1) || (this->Locks[Index] == MyId)){
            this->Locks[Index] = -1;
            H5FDdsmDebug("P(" << who << ")" << " Released own lock " << Index);
            return(H5FD_DSM_SUCCESS);
        }else{
            H5FDdsmDebug("P(" << who << ")" << " did not Released own lock " << Index);
            return(H5FD_DSM_FAIL);
        }
    }else{
        H5FDdsmInt32   status;

        H5FDdsmDebug("Sending Release Header");
        status = this->SendCommandHeader(H5FD_DSM_SEMA_RELEASE, who, Index, sizeof(H5FDdsmInt64));
        if (status == H5FD_DSM_FAIL){
            H5FDdsmError("Failed to send Release Header to " << who);
            return(H5FD_DSM_FAIL);
        }
        H5FDdsmDebug("Receiving Release Response ");
        status = this->ReceiveData(who, &RemoteStatus, sizeof(H5FDdsmInt32), H5FD_DSM_RESPONSE_TAG);
        if (status == H5FD_DSM_FAIL){
            H5FDdsmError("Failed to Release " << Index << " Response From " << who);
            return(H5FD_DSM_FAIL);
        }
        H5FDdsmDebug("Release Response = " << RemoteStatus);
        return(RemoteStatus);
    }
    return(H5FD_DSM_FAIL);
}

H5FDdsmInt32
H5FDdsmBuffer::Put(H5FDdsmInt64 Address, H5FDdsmInt64 aLength, void *Data){
  H5FDdsmInt32   who, MyId = this->Comm->GetId();
  H5FDdsmInt64       astart, aend;
  H5FDdsmInt64         len;
  H5FDdsmByte    *datap = (H5FDdsmByte *)Data;

  while(aLength){

    who = this->AddressToId(Address);
    if (who == H5FD_DSM_FAIL){
      H5FDdsmError("Address Error");
      return(H5FD_DSM_FAIL);
    }
    this->GetAddressRangeForId(who, &astart, &aend);
    len = static_cast<int>(std::min(aLength, aend - Address + 1));
    H5FDdsmDebug("Put " << len << " Bytes to Address " << Address << " Id = " << who);
    if (!ForceCommunication && (who == MyId && !this->IsConnected)) { // check if a remote DSM is connected
      H5FDdsmByte *dp;
      dp = this->DataPointer;
      dp += Address - this->StartAddress;
      memcpy(dp, datap, len);

    }else{
      H5FDdsmInt32   status;

      status = this->SendCommandHeader(H5FD_DSM_OPCODE_PUT, who, Address, len);
      if (status == H5FD_DSM_FAIL){
        H5FDdsmError("Failed to send PUT Header to " << who);
        return(H5FD_DSM_FAIL);
      }
      status = this->SendData(who, datap, len, H5FD_DSM_PUT_DATA_TAG);
      if (status == H5FD_DSM_FAIL){
        H5FDdsmError("Failed to send " << len << " bytes of data to " << who);
        return(H5FD_DSM_FAIL);
      }

    }
    aLength -= len;
    Address += len;
    datap += len;
  }
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmBuffer::Get(H5FDdsmInt64 Address, H5FDdsmInt64 aLength, void *Data){
    H5FDdsmInt32   who, MyId = this->Comm->GetId();
    H5FDdsmInt64       astart, aend;
    H5FDdsmInt64        len;
    H5FDdsmByte    *datap = (H5FDdsmByte *)Data;

    while(aLength){
        who = this->AddressToId(Address);
        if (who == H5FD_DSM_FAIL){
            H5FDdsmError("Address Error");
            return(H5FD_DSM_FAIL);
        }
        this->GetAddressRangeForId(who, &astart, &aend);
        len = static_cast<int>(std::min(aLength, aend - Address + 1));
        H5FDdsmDebug("Get " << len << " Bytes from Address " << Address << " Id = " << who);
        if (!ForceCommunication && (who == MyId)){
            H5FDdsmByte *dp;
            dp = this->DataPointer;
            dp += Address - this->StartAddress;
            memcpy(datap, dp, len);

        }else{
            H5FDdsmInt32   status;

            status = this->SendCommandHeader(H5FD_DSM_OPCODE_GET, who, Address, len);
            if (status == H5FD_DSM_FAIL){
                H5FDdsmError("Failed to send GET Header to " << who);
                return(H5FD_DSM_FAIL);
            }
            status = this->ReceiveData(who, datap, len, H5FD_DSM_GET_DATA_TAG);
            if (status == H5FD_DSM_FAIL){
                H5FDdsmError("Failed to receive " << len << " bytes of data from " << who);
                return(H5FD_DSM_FAIL);
            }

        }
        aLength -= len;
        Address += len;
        datap += len;
    }
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmBuffer::RequestRemoteChannel() {

  H5FDdsmDebug("Send request remote channel to " << this->Comm->GetId());
  this->SendCommandHeader(H5FD_DSM_REMOTE_CHANNEL, this->Comm->GetId(), 0, 0);
  return H5FD_DSM_SUCCESS;
}

H5FDdsmInt32
H5FDdsmBuffer::RequestLocalChannel() {

  int commServerSize = this->GetEndServerId() - this->GetStartServerId() + 1;

  if (this->Comm->GetId() == 0) {
    for (int i=0; i<commServerSize; i++) {
      H5FDdsmDebug("Send request local channel to " << i);
      this->SendCommandHeader(H5FD_DSM_LOCAL_CHANNEL, i, 0, 0);
    }
  }
  this->Comm->Barrier();
  return H5FD_DSM_SUCCESS;
}

H5FDdsmInt32
H5FDdsmBuffer::RequestDisconnection() {

  int commServerSize = this->GetEndServerId() - this->GetStartServerId() + 1;

  if (this->Comm->GetId() == 0) {
    for (int i=0; i<commServerSize; i++) {
      H5FDdsmDebug("Send disconnection request to " << i);
      this->SendCommandHeader(H5FD_DSM_DISCONNECT, i, 0, 0);
    }
  }
  this->Comm->RemoteCommDisconnect();
  this->SetIsConnected(false);
  H5FDdsmDebug("DSM disconnected on " << this->Comm->GetId());
  this->Comm->Barrier();
  return H5FD_DSM_SUCCESS;
}

H5FDdsmInt32
H5FDdsmBuffer::RequestXMLExchange() {

  int commServerSize = this->GetEndServerId() - this->GetStartServerId() + 1;
  // Send XML Command header and send then effective data using
  // the same communicator as the one used for data transmission
  if (this->Comm->GetId() == 0) {
    for (int i=0; i<commServerSize; i++) {
      H5FDdsmDebug("Send request xml channel to " << i);
      this->SendCommandHeader(H5FD_DSM_XML_EXCHANGE, i, 0, 0);
    }
  }
  this->Comm->Barrier();
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmBuffer::RequestClearStorage() {

  int commServerSize = this->GetEndServerId() - this->GetStartServerId() + 1;

  if (this->Comm->GetId() == 0) {
    for (int i=0; i<commServerSize; i++) {
      H5FDdsmDebug("Send request clear storage to " << i);
      this->SendCommandHeader(H5FD_DSM_CLEAR_STORAGE, i, 0, 0);
    }
  }
  this->Comm->Barrier();
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmBuffer::RequestPipelineUpdate() {

  int commServerSize = this->GetEndServerId() - this->GetStartServerId() + 1;

  for (int i=0; i<commServerSize; i++) {
    H5FDdsmDebug("Send request pipeline update to " << i);
    this->SendCommandHeader(H5FD_DSM_PIPELINE_UPDATE, i, 0, 0);
  }
  this->Comm->Barrier();
  return(H5FD_DSM_SUCCESS);
}