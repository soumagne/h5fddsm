/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmBufferService.cxx

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
#include "H5FDdsmBufferService.h"
#include "H5FDdsmComm.h"
#include "H5FDdsmMsg.h"
#include "H5FDdsmAddressMapper.h"
#include "H5FDdsmThread.h"
#include "H5FDdsmCondition.h"

#define H5FDdsmLockDebug(x) \
{ \
  if (this->IsServer) { \
    H5FDdsmDebug("(" << this->Comm->GetId() << ") " << "Server LOCK " << x); \
  } else { \
    H5FDdsmDebug("(" << this->Comm->GetId() << ") " << "Client LOCK " << x); \
  } \
}

#define H5FDdsmLockError(x) \
{ \
  if (this->IsServer) { \
    H5FDdsmError("(" << this->Comm->GetId() << ") " << "Server LOCK " << x); \
  } else { \
    H5FDdsmError("(" << this->Comm->GetId() << ") " << "Client LOCK " << x); \
  } \
}

//----------------------------------------------------------------------------
H5FDdsm_EXPORT H5FDdsm_THREAD_RETURN_TYPE H5FDdsmBufferServiceThread(void *dsmObj)
{
  H5FDdsmBufferService *Dsm = (H5FDdsmBufferService *)dsmObj;
  Dsm->BufferServiceThread();
  return(0);
}

//----------------------------------------------------------------------------
H5FDdsm_EXPORT H5FDdsm_THREAD_RETURN_TYPE H5FDdsmBufferRemoteServiceThread(void *dsmObj)
{
  H5FDdsmBufferService *Dsm = (H5FDdsmBufferService *)dsmObj;
  Dsm->RemoteServiceThread();
  return(0);
}

//----------------------------------------------------------------------------
struct H5FDdsmBufferService::H5FDdsmBufferServiceInternals
{
  H5FDdsmBufferServiceInternals() {
    this->IsConnecting    = H5FD_DSM_FALSE;
    this->IsServiceThreadCreated = H5FD_DSM_FALSE;
    this->IsRemoteServiceThreadCreated = H5FD_DSM_FALSE;
  }

  ~H5FDdsmBufferServiceInternals() {

  }

  void SignalServiceThreadCreated() {
    this->ServiceThreadCreatedMutex.Lock();
    this->IsServiceThreadCreated = H5FD_DSM_TRUE;
    this->ServiceThreadCreatedCond.Signal();
    this->ServiceThreadCreatedMutex.Unlock();
  }

  void WaitForServiceThreadCreated() {
    this->ServiceThreadCreatedMutex.Lock();
    while (!this->IsServiceThreadCreated) {
      this->ServiceThreadCreatedCond.Wait(this->ServiceThreadCreatedMutex);
    }
    this->ServiceThreadCreatedMutex.Unlock();
  }

  void SignalRemoteServiceThreadCreated() {
    this->RemoteServiceThreadCreatedMutex.Lock();
    this->IsRemoteServiceThreadCreated = H5FD_DSM_TRUE;
    this->RemoteServiceThreadCreatedCond.Signal();
    this->RemoteServiceThreadCreatedMutex.Unlock();
  }

  void WaitForRemoteServiceThreadCreated() {
    this->RemoteServiceThreadCreatedMutex.Lock();
    while (!this->IsRemoteServiceThreadCreated) {
      this->RemoteServiceThreadCreatedCond.Wait(this->RemoteServiceThreadCreatedMutex);
    }
    this->RemoteServiceThreadCreatedMutex.Unlock();
  }

  // Connection event
  H5FDdsmBoolean          IsConnecting;
  H5FDdsmMutex            ConnectionMutex;
  H5FDdsmCondition        ConnectionCond;

  // Notification event
  H5FDdsmMutex            NotificationMutex;
  H5FDdsmCondition        NotificationCond;

  H5FDdsmMutex            Lock;

  // ServiceThreadCreated event
  H5FDdsmBoolean          IsServiceThreadCreated;
  H5FDdsmMutex            ServiceThreadCreatedMutex;
  H5FDdsmCondition        ServiceThreadCreatedCond;

  // RemoteServiceThreadCreated event
  H5FDdsmBoolean          IsRemoteServiceThreadCreated;
  H5FDdsmMutex            RemoteServiceThreadCreatedMutex;
  H5FDdsmCondition        RemoteServiceThreadCreatedCond;

  H5FDdsmThread           ServiceThread;
  H5FDdsmThread           RemoteServiceThread;
};

//----------------------------------------------------------------------------
H5FDdsmBufferService::H5FDdsmBufferService()
{
  this->DataPointer        = NULL;
  //
  this->CommChannel         = H5FD_DSM_INTRA_COMM;
  this->IsConnected         = H5FD_DSM_FALSE;
  //
  this->IsNotified          = H5FD_DSM_FALSE;
  this->Notification        = 0;
  this->NotificationOnClose = H5FD_DSM_TRUE;
  this->IsDataModified      = H5FD_DSM_FALSE;
  //
  this->IsLocked            = H5FD_DSM_FALSE;
  this->ReleaseLockOnClose  = H5FD_DSM_TRUE;
  //
  this->XMLDescription      = NULL;
  //
  this->BufferServiceInternals = new H5FDdsmBufferServiceInternals;
}

//----------------------------------------------------------------------------
H5FDdsmBufferService::~H5FDdsmBufferService()
{
  if (this->IsServer) this->EndBufferService();
  if (this->XMLDescription) delete[] this->XMLDescription;
  this->XMLDescription = NULL;
  if (this->BufferServiceInternals) delete this->BufferServiceInternals;
  this->BufferServiceInternals = NULL;
}

//----------------------------------------------------------------------------
void
H5FDdsmBufferService::SetIsLocked(H5FDdsmBoolean value)
{
  if (this->IsLocked && value) {
    H5FDdsmLockError("lock already acquired");
  }
  if (!this->IsLocked && !value) {
    H5FDdsmLockError("lock already release");
  }

  this->IsLocked = value;

  if (this->IsLocked) {
    H5FDdsmLockDebug("acquired");
  } else {
    H5FDdsmLockDebug("released");
  }
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::SignalConnection()
{
  this->BufferServiceInternals->ConnectionMutex.Lock();

  this->IsConnected = H5FD_DSM_TRUE;
  this->BufferServiceInternals->ConnectionCond.Signal();
  H5FDdsmDebug("Sent connection condition signal");

  this->BufferServiceInternals->ConnectionMutex.Unlock();
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::WaitForConnection()
{
  H5FDdsmInt32 ret = H5FD_DSM_FAIL;

  this->BufferServiceInternals->ConnectionMutex.Lock();

  while (!this->IsConnected) {
    H5FDdsmDebug("Thread going into wait for connection...");
    this->BufferServiceInternals->ConnectionCond.Wait(
        this->BufferServiceInternals->ConnectionMutex);
    H5FDdsmDebug("Thread received connection signal");
  }
  if (this->IsConnected) {
    ret = H5FD_DSM_SUCCESS;
  }

  this->BufferServiceInternals->ConnectionMutex.Unlock();
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::SignalNotification()
{
  this->BufferServiceInternals->NotificationMutex.Lock();

  this->IsNotified = H5FD_DSM_TRUE;
  this->BufferServiceInternals->NotificationCond.Signal();
  H5FDdsmDebug("Sent notification condition signal");

  this->BufferServiceInternals->NotificationMutex.Unlock();
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::WaitForNotification()
{
  H5FDdsmInt32 ret = H5FD_DSM_FAIL;

  this->BufferServiceInternals->NotificationMutex.Lock();

  while (!this->IsNotified && this->IsConnected) {
    H5FDdsmDebug("Thread going into wait for notification...");
    this->BufferServiceInternals->NotificationCond.Wait(
        this->BufferServiceInternals->NotificationMutex);
    H5FDdsmDebug("Thread received notification signal");
  }
  if (this->IsNotified && this->IsConnected) {
    this->IsNotified = H5FD_DSM_FALSE;
    ret = H5FD_DSM_SUCCESS;
  }

  this->BufferServiceInternals->NotificationMutex.Unlock();
  return(ret);
}

//----------------------------------------------------------------------------
void *
H5FDdsmBufferService::BufferServiceThread()
{
  H5FDdsmInt32   ReturnOpcode;

  H5FDdsmDebug("Starting DSM Service on node " << this->Comm->GetId());
  this->BufferServiceInternals->SignalServiceThreadCreated();
  this->BufferServiceLoop(&ReturnOpcode);
  H5FDdsmDebug("Ending DSM Service on node " << this->Comm->GetId() << " last op = " << ReturnOpcode);
  return((void *)this);
}

//----------------------------------------------------------------------------
void *
H5FDdsmBufferService::RemoteServiceThread()
{
  H5FDdsmInt32   ReturnOpcode;

  H5FDdsmDebug("Starting DSM Remote Service on node " << this->Comm->GetId());
  this->BufferServiceInternals->SignalRemoteServiceThreadCreated();
  this->RemoteService(&ReturnOpcode);
  H5FDdsmDebug("Ending DSM Remote Service on node " << this->Comm->GetId() << " last op = " << ReturnOpcode);
  return((void *)this);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::BufferServiceLoop(H5FDdsmInt32 *returnOpcode)
{
  H5FDdsmInt32   op, status = H5FD_DSM_SUCCESS;

  while(status == H5FD_DSM_SUCCESS) {
    status = this->BufferService(&op);
    if (status != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
    if (returnOpcode) *returnOpcode = op;
    if (op == H5FD_DSM_OPCODE_DONE) return(H5FD_DSM_SUCCESS);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::BufferService(H5FDdsmInt32 *returnOpcode)
{
  H5FDdsmInt32        opcode, who, status = H5FD_DSM_FAIL;
  H5FDdsmInt32        aLength;
  H5FDdsmAddr         address;
  H5FDdsmByte        *datap;
  static H5FDdsmInt32 syncId = -1;

  if (syncId >= 0) {
    H5FDdsmDebug("(" << this->Comm->GetId() << ") " << "Receiving command header from " << syncId);
  } else {
    H5FDdsmDebug("(" << this->Comm->GetId() << ") " << "Receiving command header from anyone");
  }

  status = this->ReceiveCommandHeader(&opcode, &who, &address, &aLength, this->CommChannel, syncId);
  if (status == H5FD_DSM_FAIL) {
    H5FDdsmError("Error Receiving Command Header");
    return(H5FD_DSM_FAIL);
  }

  switch(opcode) {

  // H5FD_DSM_OPCODE_PUT
  case H5FD_DSM_OPCODE_PUT:
    H5FDdsmDebug("PUT request from " << who << " for " << aLength << " bytes @ " << address);
    if (((H5FDdsmUInt32) aLength + address) > this->Length) {
      H5FDdsmError("Length " << aLength << " too long for Address " << address);
      H5FDdsmError("Server Start = " << this->StartAddress << " End = " << this->EndAddress);
      return(H5FD_DSM_FAIL);
    }
    if ((datap = this->DataPointer) == NULL) H5FDdsmError("Null Data Pointer when trying to put data");
    datap += address;
    status = this->ReceiveData(who, datap, aLength, H5FD_DSM_PUT_DATA_TAG, 0, this->CommChannel);
    if (status == H5FD_DSM_FAIL) {
      H5FDdsmError("ReceiveData() failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("Serviced PUT request from " << who << " for " << aLength << " bytes @ " << address);
    break;

  // H5FD_DSM_OPCODE_GET
  case H5FD_DSM_OPCODE_GET:
    H5FDdsmDebug("(Server " << this->Comm->GetId() << ") Get request from " << who << " for " << aLength << " bytes @ " << address);
    if (((H5FDdsmUInt32) aLength + address) > this->Length) {
      H5FDdsmError("Length " << aLength << " too long for Address " << address);
      H5FDdsmError("Server Start = " << this->StartAddress << " End = " << this->EndAddress);
      return(H5FD_DSM_FAIL);
    }
    if ((datap = this->DataPointer) == NULL) H5FDdsmError("Null Data Pointer when trying to get data");
    datap += address;
    status = this->SendData(who, datap, aLength, H5FD_DSM_GET_DATA_TAG, 0, this->CommChannel);
    if (status == H5FD_DSM_FAIL) {
      H5FDdsmError("SendData() failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("(Server " << this->Comm->GetId() << ") Serviced GET request from " << who << " for " << aLength << " bytes @ " << address);
    break;

  // H5FD_DSM_LOCK_RELEASE
  case H5FD_DSM_LOCK_RELEASE:
    if (this->Comm->ChannelSynced(who, &syncId) || !this->IsConnected) {
      this->CommChannel = H5FD_DSM_INTRA_COMM;
      this->SetIsLocked(H5FD_DSM_FALSE);
      this->BufferServiceInternals->Lock.Unlock();
      // If the client releases the lock without explicitly giving it to the server
      // restart the remote service thread so that the client can potentially
      // reacquire the lock.
      if (this->IsConnected) {
        if (!this->BufferServiceInternals->IsRemoteServiceThreadCreated)
          this->StartRemoteService();
      }
    }
    break;

  // H5FD_DSM_NOTIFICATION
  case H5FD_DSM_NOTIFICATION:
    if (this->Comm->ChannelSynced(who, &syncId)) {
      // Receiving a notification from the remote client automatically makes
      // the service thread listen on server until the notification has been
      // processed/finalized.
      this->CommChannel = H5FD_DSM_INTRA_COMM;
      if (address & H5FD_DSM_DATA_MODIFIED) {
        this->IsDataModified = H5FD_DSM_TRUE;
        this->Notification = (H5FDdsmInt32)address - H5FD_DSM_DATA_MODIFIED;
      } else {
        this->Notification = (H5FDdsmInt32)address;
      }
      // When a notification is found, the server keeps the lock
      // and only releases it when the requested task is over.
      this->ReleaseLockOnClose = H5FD_DSM_FALSE;
      H5FDdsmDebug("(" << this->Comm->GetId() << ") " << "Notification " <<
          this->Notification << ", Switched to Local channel");
      this->SignalNotification();
    }
    break;

  // H5FD_DSM_ACCEPT
  case H5FD_DSM_ACCEPT:
    if (!this->IsConnected) {
      status = this->Comm->Accept(this->DataPointer, this->Length);
      if (status == H5FD_DSM_FAIL) {
        H5FDdsmDebug("RemoteCommAccept Failed");
        return(H5FD_DSM_FAIL);
      }
      this->BufferServiceInternals->IsConnecting = H5FD_DSM_FALSE;
      // send DSM information
      this->SendInfo();
      if (this->AddressMapper->GetDsmType() == H5FD_DSM_TYPE_DYNAMIC_MASK) this->ReceiveMaskLength();
      this->SignalConnection();
    }
    if (!this->BufferServiceInternals->IsRemoteServiceThreadCreated) {
      this->StartRemoteService();
    }
    break;

  // H5FD_DSM_DISCONNECT
  case H5FD_DSM_DISCONNECT:
    if (this->Comm->ChannelSynced(who, &syncId)) {
      H5FDdsmDebug("( " << this->Comm->GetId() << " ) Freeing now remote channel");
      this->Comm->Disconnect();
      this->CommChannel = H5FD_DSM_INTRA_COMM;
      this->IsConnected = H5FD_DSM_FALSE;
      H5FDdsmDebug("DSM disconnected on " << this->Comm->GetId() << ", Switched to Local channel");
      // Because we may have been waiting for a notification
      this->SignalNotification();
    }
    break;

  // H5FD_DSM_COMM_SWITCH
  case H5FD_DSM_COMM_SWITCH:
    if (this->CommChannel == H5FD_DSM_INTRA_COMM) {
      // The remote service must be stopped before the main service can start to listen on the inter-communicator
      this->EndRemoteService();
      this->CommChannel = H5FD_DSM_INTER_COMM;
      H5FDdsmDebug("Listening on Remote");
    } else {
      this->CommChannel = H5FD_DSM_INTRA_COMM;
      H5FDdsmError("Listening on Local");
    }
    break;

  // H5FD_DSM_CLEAR_STORAGE
  case H5FD_DSM_CLEAR_STORAGE:
    if (this->Comm->ChannelSynced(who, &syncId)) {
      this->ClearStorage();
    }
    break;

  // H5FD_DSM_OPCODE_DONE
  case H5FD_DSM_OPCODE_DONE:
    break;

  // DEFAULT
  default :
    H5FDdsmError("Unknown Opcode " << opcode);
    return(H5FD_DSM_FAIL);
  }

  if (returnOpcode) *returnOpcode = opcode;
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::StartBufferService()
{
  H5FDdsmDebug("Creating service thread...");
  this->BufferServiceInternals->ServiceThread.SpawnThread(
      H5FDdsmBufferServiceThread, (void *) this);
  // this->BufferServiceInternals->WaitForServiceThreadCreated();
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::EndBufferService()
{
  if (this->BufferServiceInternals->IsServiceThreadCreated) {
    if (this->BufferServiceInternals->IsConnecting && !this->IsConnected) {
      if (this->Comm->GetInterCommType() == H5FD_DSM_COMM_SOCKET) {
        this->Comm->ClosePort();
        this->BufferServiceInternals->ServiceThread.JoinThread();
      } else {
        this->BufferServiceInternals->ServiceThread.TerminateThread();
      }
    } else {
      this->SendDone();
      this->BufferServiceInternals->ServiceThread.JoinThread();
    }
  }
  this->BufferServiceInternals->IsServiceThreadCreated = H5FD_DSM_FALSE;
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::RemoteService(H5FDdsmInt32 *returnOpcode)
{
  H5FDdsmInt32        opcode, who, status = H5FD_DSM_FAIL;
  H5FDdsmInt32        aLength;
  H5FDdsmAddr         address;
  H5FDdsmInt32        syncId = -1;

  for (H5FDdsmInt32 i = 0; i < this->Comm->GetInterSize(); i++) {
    // This receive always gets data from the inter-communicator
    status = this->ReceiveCommandHeader(&opcode, &who, &address, &aLength, H5FD_DSM_INTER_COMM, i);

    if (status == H5FD_DSM_FAIL) {
      H5FDdsmError("Error Receiving Command Header");
      return(H5FD_DSM_FAIL);
    }

    switch(opcode) {
    case H5FD_DSM_LOCK_ACQUIRE:
      if (this->Comm->ChannelSynced(who, &syncId)) {
        // The server may have taken the lock back in the meantime so need to acquire
        // the mutex lock first
        this->BufferServiceInternals->Lock.Lock();
        who = this->Comm->GetId();
        H5FDdsmDebug("Send request communicator switch to " << who);
        // Switch the service thread communicator to listen on the inter-comm
        status = this->SendCommandHeader(H5FD_DSM_COMM_SWITCH, who, 0, 0, H5FD_DSM_INTRA_COMM);
        if (status == H5FD_DSM_FAIL) {
          H5FDdsmError("Error Sending Command Header");
        }
        this->SetIsLocked(H5FD_DSM_TRUE);
        // The lock has been acquired so notify the client back
        if (this->Comm->GetId() == 0) {
          for (H5FDdsmInt32 j = 0; j < this->Comm->GetInterSize(); j++) {
            this->SendAcknowledgment(j, H5FD_DSM_INTER_COMM);
          }
        }
      }
      break;
    default:
      H5FDdsmError("Unknown Remote Service Opcode " << opcode);
      return(H5FD_DSM_FAIL);
    }
  }
  if (returnOpcode) *returnOpcode = opcode;
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::StartRemoteService()
{
  // create a single thread listening on the remote transaction request
  H5FDdsmDebug("Creating remote service thread...");
  this->BufferServiceInternals->RemoteServiceThread.SpawnThread(
      H5FDdsmBufferRemoteServiceThread, (void *) this);
  // this->BufferServiceInternals->WaitForRemoteServiceThreadCreated();
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::EndRemoteService()
{
  if (this->BufferServiceInternals->IsRemoteServiceThreadCreated) {
    this->BufferServiceInternals->RemoteServiceThread.JoinThread();
  }
  this->BufferServiceInternals->IsRemoteServiceThreadCreated = H5FD_DSM_FALSE;
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::SendAccept()
{
  H5FDdsmInt32 who = this->Comm->GetId();
  H5FDdsmInt32 status = H5FD_DSM_SUCCESS;

  if (!this->IsConnected) this->BufferServiceInternals->IsConnecting = H5FD_DSM_TRUE;

  status = this->SendCommandHeader(H5FD_DSM_ACCEPT, who, 0, 0, H5FD_DSM_INTRA_COMM);
  if (status != H5FD_DSM_SUCCESS) {
    H5FDdsmError("Cannot send accept command to DSM process " << who);
    return(status);
  }
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::SendDone()
{
  H5FDdsmInt32 who = this->Comm->GetId();
  H5FDdsmInt32 status = H5FD_DSM_SUCCESS;

  status = this->SendCommandHeader(H5FD_DSM_OPCODE_DONE, who, 0, 0, H5FD_DSM_INTRA_COMM);
  if (status != H5FD_DSM_SUCCESS) {
    H5FDdsmError("Cannot send accept command to DSM process " << who);
    return(status);
  }
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::Put(H5FDdsmAddr address, H5FDdsmUInt64 length,
    H5FDdsmPointer data)
{
  H5FDdsmInt32 myId = this->Comm->GetId();
  std::vector<H5FDdsmMsg> putRequests;

  this->AddressMapper->Translate(address, length, data, putRequests);

  while (!putRequests.empty()) {
    H5FDdsmMsg &putRequest = putRequests.back();
    if ((putRequest.Dest == myId) && !this->IsConnected) { // check if a remote DSM is connected
      H5FDdsmByte *dp;
      dp = this->DataPointer;
      dp += putRequest.Address;
      memcpy(dp, putRequest.Data, putRequest.Length);
    } else {
      H5FDdsmInt32 status;
      H5FDdsmInt32 comm = (this->IsServer) ? H5FD_DSM_INTRA_COMM : H5FD_DSM_INTER_COMM;
      H5FDdsmDebug("PUT request to " << putRequest.Dest << " for "
          << putRequest.Length << " bytes @ " << putRequest.Address);
      if (putRequest.Dest > this->EndServerId || putRequest.Address > this->Length) {
        H5FDdsmError("Exceeded DSM address range");
        return(H5FD_DSM_FAIL);
      }
      if (!this->Comm->GetUseOneSidedComm()) {
        status = this->SendCommandHeader(H5FD_DSM_OPCODE_PUT, putRequest.Dest, putRequest.Address, putRequest.Length, comm);
        if (status == H5FD_DSM_FAIL) {
          H5FDdsmError("Failed to send PUT Header to " << putRequest.Dest);
          return(H5FD_DSM_FAIL);
        }
      }
      status = this->SendData(putRequest.Dest, putRequest.Data, putRequest.Length, H5FD_DSM_PUT_DATA_TAG, putRequest.Address, comm);
      if (status == H5FD_DSM_FAIL) {
        H5FDdsmError("Failed to send " << putRequest.Length << " bytes of data to " << putRequest.Dest);
        return(H5FD_DSM_FAIL);
      }
    }
    putRequests.pop_back();
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::Get(H5FDdsmAddr address, H5FDdsmUInt64 length, H5FDdsmPointer data,
    H5FDdsmBoolean blocking)
{
  H5FDdsmInt32 myId = this->Comm->GetId();
  std::vector<H5FDdsmMsg> getRequests;

  this->AddressMapper->Translate(address, length, data, getRequests);

  while (!getRequests.empty()) {
    H5FDdsmMsg &getRequest = getRequests.back();

    if ((getRequest.Dest == myId) && (!this->IsConnected || this->IsServer)) {
      H5FDdsmByte *dp;
      dp = this->DataPointer;
      dp += getRequest.Address;
      memcpy(getRequest.Data, dp, getRequest.Length);
    } else {
      H5FDdsmInt32   status;
      H5FDdsmInt32 comm = (this->IsServer) ? H5FD_DSM_INTRA_COMM : H5FD_DSM_INTER_COMM;
      H5FDdsmDebug("Get request to " << getRequest.Dest << " for "
          << getRequest.Length << " bytes @ " << getRequest.Address);
      if (getRequest.Dest > this->EndServerId || getRequest.Address > this->Length) {
        H5FDdsmError("Exceeded DSM address range");
        return(H5FD_DSM_FAIL);
      }
      if (!this->Comm->GetUseOneSidedComm()) {
        status = this->SendCommandHeader(H5FD_DSM_OPCODE_GET, getRequest.Dest, getRequest.Address, getRequest.Length, comm);
        if (status == H5FD_DSM_FAIL) {
          H5FDdsmError("Failed to send GET Header to " << getRequest.Dest);
          return(H5FD_DSM_FAIL);
        }
      }
      status = this->ReceiveData(getRequest.Dest, getRequest.Data, getRequest.Length, H5FD_DSM_GET_DATA_TAG, getRequest.Address, comm);
      if (status == H5FD_DSM_FAIL) {
        H5FDdsmError("Failed to receive " << getRequest.Length << " bytes of data from " << getRequest.Dest);
        return(H5FD_DSM_FAIL);
      }
      if (blocking && this->Comm->GetUseOneSidedComm()) {
        this->Comm->WindowSync();
      }
    }
    getRequests.pop_back();
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::RequestLockAcquire()
{
  H5FDdsmInt32 status = H5FD_DSM_SUCCESS;

  if (this->IsServer) {
    // On server, when the mutex is locked, the server is locked
    this->BufferServiceInternals->Lock.Lock();
  } else {
    // On client, we need to check the status of the remote server lock
    for (H5FDdsmInt32 who = this->StartServerId ; who <= this->EndServerId ; who++) {
      H5FDdsmDebug("Send request LOCK acquire to " << who);
      status = this->SendCommandHeader(H5FD_DSM_LOCK_ACQUIRE, who, 0, 0, H5FD_DSM_INTER_COMM);
    }
    status = this->WaitForLockAcquisition();
  }

  this->SetIsLocked(H5FD_DSM_TRUE);
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::WaitForLockAcquisition()
{
  H5FDdsmInt32 status = H5FD_DSM_SUCCESS;

  status = this->ReceiveAcknowledgment(0, H5FD_DSM_INTER_COMM);

  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::RequestLockRelease()
{
  H5FDdsmInt32 status = H5FD_DSM_SUCCESS;

  this->SetIsLocked(H5FD_DSM_FALSE);

  // Complete RMA operations when the file is unlocked
  this->Comm->WindowSync();

  if (this->IsServer) {
    this->BufferServiceInternals->Lock.Unlock();
    // If the server was owning the lock the buffer service thread is listening
    // on the INTRA_COMM so start listening again on INTER_COMM
    if (this->IsConnected &&
        !this->BufferServiceInternals->IsRemoteServiceThreadCreated)
      this->StartRemoteService();
  } else {
    for (H5FDdsmInt32 who = this->StartServerId ; who <= this->EndServerId ; who++) {
      H5FDdsmDebug("Send request LOCK release to " << who);
      status = this->SendCommandHeader(H5FD_DSM_LOCK_RELEASE, who, 0, 0, H5FD_DSM_INTER_COMM);
    }
  }
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::RequestNotification()
{
  H5FDdsmInt32 who, status = H5FD_DSM_SUCCESS;

  // If there were ongoing transactions, finish them
  this->Comm->WindowSync();

  // No mutex here, only informal since it's the client
  this->SetIsLocked(H5FD_DSM_FALSE);

  for (who = this->StartServerId ; who <= this->EndServerId ; who++) {
    H5FDdsmAddr localFlag = 0;
    H5FDdsmDebug("Send request server notification to " << who << " with level " << this->Notification);
    // for convenience
    if (this->IsDataModified) localFlag = H5FD_DSM_DATA_MODIFIED;
    localFlag |= this->Notification;
    status = this->SendCommandHeader(H5FD_DSM_NOTIFICATION, who, localFlag, 0, H5FD_DSM_INTER_COMM);
  }
  if (this->IsDataModified) this->IsDataModified = H5FD_DSM_FALSE;
  if (!this->Notification) this->Notification = 0;

  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::RequestDisconnect()
{
  H5FDdsmInt32 who, status = H5FD_DSM_SUCCESS;

  if (!this->IsLocked) this->RequestLockAcquire();

  for (who = this->StartServerId ; who <= this->EndServerId ; who++) {
    H5FDdsmDebug("Send disconnection request to " << who);
    status = this->SendCommandHeader(H5FD_DSM_DISCONNECT, who, 0, 0, H5FD_DSM_INTER_COMM);
  }
  status = this->Comm->Disconnect();
  this->IsConnected = H5FD_DSM_FALSE;
  H5FDdsmDebug("DSM disconnected on " << this->Comm->GetId());
  return(status);
}
