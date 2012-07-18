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
#include "H5FDdsm.h"
#include "H5FDdsmBufferService.h"
#include "H5FDdsmComm.h"
#include "H5FDdsmMsg.h"
#include "H5FDdsmAddressMapper.h"
#include "H5FDdsmThread.h"
#include "H5FDdsmCondition.h"
#include "H5FDdsmLock.h"

//----------------------------------------------------------------------------
// Declare extra debug info 
#undef H5FDdsmDebugLevel
#ifdef H5FDdsm_DEBUG_GLOBAL
#define H5FDdsmDebugLevel(level, x) \
{ if (this->DebugLevel >= level) { \
  std::cout << "H5FD_DSM Debug Level " << level << ": " << (this->IsServer ? "Server " : "Client ") << this->Comm->GetId() << " : " << x << std::endl; \
  } \
}
#else
#define H5FDdsmDebugLevel(level, x) \
{ if (this->Debug && this->DebugLevel >= level) { \
  std::cout << "H5FD_DSM Debug Level " << level << ": " << (this->IsServer ? "Server " : "Client ") << this->Comm->GetId() << " : " << x << std::endl; \
  } \
}
#endif
//----------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Utility function to return a string for a given tag
//------------------------------------------------------------------------------
#define H5FD_DSM_OPCODE_MACRO(def, value) \
  if (value==def) return #def;

H5FDdsmConstString H5FDdsmOpcodeToString(H5FDdsmInt32 code) {
  H5FD_DSM_OPCODE_MACRO(H5FD_DSM_OPCODE_PUT, code)
  H5FD_DSM_OPCODE_MACRO(H5FD_DSM_OPCODE_GET, code)
  H5FD_DSM_OPCODE_MACRO(H5FD_DSM_LOCK_ACQUIRE, code)
  H5FD_DSM_OPCODE_MACRO(H5FD_DSM_LOCK_ACQUIRE_SERIAL, code)
  H5FD_DSM_OPCODE_MACRO(H5FD_DSM_LOCK_RELEASE, code)
  H5FD_DSM_OPCODE_MACRO(H5FD_DSM_LOCK_RELEASE_SERIAL, code)
  H5FD_DSM_OPCODE_MACRO(H5FD_DSM_ACCEPT, code)
  H5FD_DSM_OPCODE_MACRO(H5FD_DSM_DISCONNECT, code)
  H5FD_DSM_OPCODE_MACRO(H5FD_DSM_CLEAR_STORAGE, code)
  H5FD_DSM_OPCODE_MACRO(H5FD_DSM_OPCODE_DONE, code)

  static char buff[128];
  sprintf(buff,"%s %i", "UNKNOWN OPCODE",code);
  return buff;
}

H5FDdsmConstString H5FDdsmNotificationToString(H5FDdsmInt32 code) {
  H5FD_DSM_OPCODE_MACRO(H5FD_DSM_NOTIFY_NONE, code)
  H5FD_DSM_OPCODE_MACRO(H5FD_DSM_NOTIFY_WAIT, code)
  H5FD_DSM_OPCODE_MACRO(H5FD_DSM_NOTIFY_DATA, code)
  H5FD_DSM_OPCODE_MACRO(H5FD_DSM_NOTIFY_INFORMATION, code)
  H5FD_DSM_OPCODE_MACRO(H5FD_DSM_NOTIFY_USER, code)
  static char buff[128];
  sprintf(buff,"%s %i", "UNKNOWN NOTIFICATION FLAG",code);
  return buff;
}

//----------------------------------------------------------------------------
H5FDdsm_EXPORT H5FDdsm_THREAD_RETURN_TYPE H5FDdsmBufferServiceThread(void *dsmObj)
{
  H5FDdsmBufferService *Dsm = (H5FDdsmBufferService *)dsmObj;
  Dsm->BufferServiceThread();
  return(0);
}

//----------------------------------------------------------------------------
struct H5FDdsmBufferService::H5FDdsmBufferServiceInternals
{
  H5FDdsmBufferServiceInternals() {
    this->IsConnecting           = H5FD_DSM_FALSE;
    this->IsServiceThreadCreated = H5FD_DSM_FALSE;
    this->ConnectionCond.SetName("Connection");
    this->ServiceThreadCreatedCond.SetName("ServiceThreadCreated");
    this->UnlockCondition.SetName("Unlock");
  }

  ~H5FDdsmBufferServiceInternals() {
  }

  void SignalServiceThreadCreated() {
    this->ServiceThreadCreatedCond.LockMutex();
    this->IsServiceThreadCreated = H5FD_DSM_TRUE;
    this->ServiceThreadCreatedCond.Signal();
    this->ServiceThreadCreatedCond.UnlockMutex();
  }

  void WaitForServiceThreadCreated() {
    this->ServiceThreadCreatedCond.LockMutex();
    while (!this->IsServiceThreadCreated) {
      this->ServiceThreadCreatedCond.Wait();
    }
    this->ServiceThreadCreatedCond.UnlockMutex();
  }

  // primary lock for DSM buffer
  H5FDdsmLock             BufferLock;

  // Connection event
  H5FDdsmBoolean          IsConnecting;
  H5FDdsmCondition        ConnectionCond;

  // Unlock event
  H5FDdsmCondition        UnlockCondition;

  // ServiceThreadCreated event
  H5FDdsmBoolean          IsServiceThreadCreated;
  H5FDdsmCondition        ServiceThreadCreatedCond;

  H5FDdsmThread           ServiceThread;
};

//----------------------------------------------------------------------------
H5FDdsmBufferService::H5FDdsmBufferService()
{
  this->DataPointer        = NULL;
  //
  this->CommChannel         = H5FD_DSM_INTRA_COMM;
  this->IsConnected         = H5FD_DSM_FALSE;
  this->IsDisconnected      = H5FD_DSM_FALSE;
  //
  this->UnlockStatus        = H5FD_DSM_NOTIFY_DATA;
  //
  this->ReleaseLockOnClose  = H5FD_DSM_TRUE;
  //
  this->XMLDescription      = NULL;
  //
  this->BufferServiceInternals = new H5FDdsmBufferServiceInternals;
  this->BufferServiceInternals->BufferLock.SetServerSychronizationCount(1);
  this->BufferServiceInternals->BufferLock.SetClientSychronizationCount(1);
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
void H5FDdsmBufferService::SetSychronizationCount(H5FDdsmInt32 count)
{
  if (this->IsServer) {
    this->BufferServiceInternals->BufferLock.SetServerSychronizationCount(count);
  }
  else {
    this->BufferServiceInternals->BufferLock.SetClientSychronizationCount(count);
  }
}

//----------------------------------------------------------------------------
H5FDdsmBoolean H5FDdsmBufferService::GetIsConnected()
{
  return this->IsConnected;
}
//----------------------------------------------------------------------------
H5FDdsmBoolean H5FDdsmBufferService::GetIsDisconnected()
{
  return this->IsDisconnected;
}
//----------------------------------------------------------------------------
H5FDdsmBoolean H5FDdsmBufferService::GetIsLockWaiting(bool clear)
{
  H5FDdsmBoolean ret;
  if (this->IsServer) {
    ret = this->BufferServiceInternals->BufferLock.GetClientUnlockedFlag(clear);
    H5FDdsmDebugLevel(1,"GetClientUnlockedFlag returns " << ret);    
    return ret;
  }
  ret = this->BufferServiceInternals->BufferLock.GetServerUnlockedFlag(clear);
  H5FDdsmDebugLevel(1,"GetServerUnlockedFlag returns " << ret);    
  return ret; 
}
//----------------------------------------------------------------------------
//
// Protect this->IsConnected by mutex in signal and wait functions
//
H5FDdsmInt32
H5FDdsmBufferService::SignalConnection()
{
  this->BufferServiceInternals->ConnectionCond.LockMutex();
  this->IsConnected = H5FD_DSM_TRUE;
  this->BufferServiceInternals->ConnectionCond.Signal();
  this->BufferServiceInternals->ConnectionCond.UnlockMutex();
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::WaitForConnection()
{
  this->BufferServiceInternals->ConnectionCond.LockMutex();
  if (!this->IsConnected && !this->IsDisconnected) {
    this->BufferServiceInternals->ConnectionCond.Wait();
  }
  this->BufferServiceInternals->ConnectionCond.UnlockMutex();
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::SignalUnlock()
{
  this->BufferServiceInternals->UnlockCondition.LockMutex();
  this->BufferServiceInternals->UnlockCondition.Signal();
  this->BufferServiceInternals->UnlockCondition.UnlockMutex();
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::WaitForUnlock()
{
  H5FDdsmInt32 ReturnOpcode = H5FD_DSM_SUCCESS;

  H5FDdsmDebugLevel(1,"WaitForUnlock ********************");
  this->BufferServiceInternals->UnlockCondition.LockMutex();
  // if the client already unlocked and disconnected, exit immediately
  if (this->GetIsLockWaiting(true)) {
    H5FDdsmDebugLevel(1,"Aborted UnlockCondition.Wait() due to IsLockWaiting override");
  }
  // must not wait if client already disconnected
  else {
    if ((this->BufferServiceInternals->IsConnecting || this->IsConnected) && !this->IsDisconnected) {
      H5FDdsmDebugLevel(1,"Entering UnlockCondition.Wait()");
      this->BufferServiceInternals->UnlockCondition.Wait();
      H5FDdsmDebugLevel(1,"Finished UnlockCondition.Wait()");
      if (this->IsDisconnected || !this->IsConnected) {
        // we were disconnected whilst waiting
        H5FDdsmDebugLevel(1,"Unlocked, disconnected during wait, returning Fail");
        if (this->GetIsLockWaiting(true)) {
          H5FDdsmDebugLevel(1,"IsLockWaiting override after disconnect");
          ReturnOpcode = H5FD_DSM_TRUE;
        }
        else {
          ReturnOpcode = H5FD_DSM_FAIL;
        }
      }
    }
    else {
      H5FDdsmDebugLevel(1,"Unlocked, disconnected before wait, returning Fail");
      ReturnOpcode = H5FD_DSM_FAIL;
    }
  }
  this->BufferServiceInternals->UnlockCondition.UnlockMutex();
  //
  H5FDdsmDebugLevel(1,"WaitForUnlock $$$$$$$$$$$$$$$$$$$$$$$$$4");
  return ReturnOpcode;
}

//----------------------------------------------------------------------------
void *
H5FDdsmBufferService::BufferServiceThread()
{
  H5FDdsmInt32   ReturnOpcode;

  H5FDdsmDebug("Starting DSM Service");
  this->BufferServiceInternals->SignalServiceThreadCreated();
  this->BufferServiceLoop(&ReturnOpcode);
  H5FDdsmDebug("Ending DSM Service - last op = " << ReturnOpcode);
  return((void *)this);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::BufferServiceLoop(H5FDdsmInt32 *returnOpcode)
{
  H5FDdsmInt32 op, status = H5FD_DSM_SUCCESS;

  while (status == H5FD_DSM_SUCCESS) {
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
  static H5FDdsmInt32 syncId      = -1;
  static H5FDdsmInt32 syncChannel = -1;

  if (this->CommChannel == H5FD_DSM_ANY_COMM) {
    if (this->Comm->GetId() == 0) {
      status = this->ProbeCommandHeader(&this->CommChannel);
      H5FDdsmDebugLevel(1,"ProbeCommandHeader returns " << status );
    }
    this->BroadcastComm(&this->CommChannel, 0);
  }
  if (syncId >= 0) {
    H5FDdsmDebugLevel(1,"Receiving command header from " << syncId
        << " on " << H5FDdsmCommToString(this->CommChannel));
  } else {
    H5FDdsmDebugLevel(1,"Receiving command header from anyone on " <<
        H5FDdsmCommToString(this->CommChannel));
  }

  status = this->ReceiveCommandHeader(&opcode, &who, &address, &aLength, this->CommChannel, syncId);
  if (status == H5FD_DSM_FAIL) {
    H5FDdsmError("Error Receiving Command Header");
    return(H5FD_DSM_FAIL);
  }

  // connection is an ID for client or server, 
  // we can use the communicator ID interchangably, but if the architecture is altered - be careful
  H5FDdsmInt32 communicatorId = this->CommChannel;

  switch(opcode) {

  // H5FD_DSM_OPCODE_PUT
  case H5FD_DSM_OPCODE_PUT:
    H5FDdsmDebugLevel(2,"PUT request from " << who << " for " << aLength << " bytes @ " << address);
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
    H5FDdsmDebugLevel(2,"Serviced PUT request from " << who << " for " << aLength << " bytes @ " << address);
    break;

  // H5FD_DSM_OPCODE_GET
  case H5FD_DSM_OPCODE_GET:
    H5FDdsmDebugLevel(2,"Get request from " << who << " for " << aLength << " bytes @ " << address);
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
    H5FDdsmDebugLevel(2,"Serviced GET request from " << who << " for " << aLength << " bytes @ " << address);
    break;

  // H5FD_DSM_LOCK_ACQUIRE
  // Comes from client or server depending on communicator
  case H5FD_DSM_LOCK_ACQUIRE:
    // wait for all processes to sync before doing anything
    if (this->Comm->ChannelSynced(who, &syncId, communicatorId)) {
      // only rank 0 actually handles the lock, the other ranks just mimic it later when they get an acknowledgement
      if (this->Comm->GetId()==0) {
        if (this->BufferServiceInternals->BufferLock.Lock(communicatorId)) {
          // notify all other server nodes - to update their local locks to match rank 0
          H5FDdsmInt32 numberOfRanks = this->Comm->GetIntraSize();
          for (H5FDdsmInt32 who=1; who<numberOfRanks; who++) {
            this->SendAcknowledgment(who, H5FD_DSM_INTRA_COMM, communicatorId, "LOCK ACQUIRE (server)", H5FD_DSM_RESPONSE_TAG+1);
          }
          // notify the ranks that made the request
          numberOfRanks = (communicatorId==H5FDdsm_SERVER_ID) ? this->Comm->GetIntraSize() : this->Comm->GetInterSize();
          for (H5FDdsmInt32 who=0; who<numberOfRanks; who++) {
            this->SendAcknowledgment(who, this->CommChannel, communicatorId, "LOCK ACQUIRE (user)", H5FD_DSM_RESPONSE_TAG+2);
          }
        }
        //  we were not given the lock, so go back to listening for anyone
        else {
          this->CommChannel = H5FD_DSM_ANY_COMM;
          // notify all other server nodes that lock request failed and to change communicator
          H5FDdsmInt32 numberOfRanks = this->Comm->GetIntraSize();
          for (H5FDdsmInt32 who=1; who<numberOfRanks; who++) {
            this->SendAcknowledgment(who, H5FD_DSM_INTRA_COMM, -1, "LOCK ACQUIRE (server)", H5FD_DSM_RESPONSE_TAG+1);
          }
        }
      }
      else {
        // all server nodes need to update their local locks to match rank 0
        this->ReceiveAcknowledgment(0, H5FD_DSM_INTRA_COMM, communicatorId, "LOCK ACQUIRE (server)", H5FD_DSM_RESPONSE_TAG+1);
        // the lock request failed, so we don't give the lock to the requestor
        if (communicatorId==-1) {
          this->CommChannel = H5FD_DSM_ANY_COMM;
        }
        else {
          this->BufferServiceInternals->BufferLock.Lock(communicatorId);
        }
      }
    }
    break;

  // H5FD_DSM_LOCK_RELEASE
  // Comes from client or server depending on communicator
  case H5FD_DSM_LOCK_RELEASE:
    // wait for all processes to sync before doing anything
    if (this->Comm->ChannelSynced(who, &syncId, communicatorId)) {
      // the address flag hold our unlock status
      this->SetUnlockStatus(address);
      // only rank 0 actually handles the lock, the other ranks just mimic it later when they get an acknowledgement
      H5FDdsmInt32 newLockOwner = -1;
      if (this->Comm->GetId()==0) {
        // When we release the lock, it may be passed straight to the next owner, 
        // if this happens, we must inform the other server nodes who the owner is
        newLockOwner = this->BufferServiceInternals->BufferLock.Unlock(communicatorId);
        H5FDdsmInt32 numberOfRanks = this->Comm->GetIntraSize();
        for (H5FDdsmInt32 who=1; who<numberOfRanks; who++) {
          this->SendAcknowledgment(who, H5FD_DSM_INTRA_COMM, newLockOwner, "LOCK RELEASE");
        }
      }
      else {
        // all server nodes need to update their local locks to match rank 0
        this->ReceiveAcknowledgment(0, H5FD_DSM_INTRA_COMM, newLockOwner, "LOCK RELEASE");
        this->BufferServiceInternals->BufferLock.Unlock(communicatorId);
      }

      //
      // the lock has been released : if the client unlocked, wake up waiting server app thread
      // note that a lock count decrease returns the same lock owner, so we don't trigger on that event
      //
      if (newLockOwner!=communicatorId && communicatorId==H5FDdsm_CLIENT_ID) {
        this->SignalUnlock();
      }
      //
      // if it has been taken by another communicator/connection, do what's needed
      //
      if (newLockOwner==-1) {
        this->CommChannel = H5FD_DSM_ANY_COMM;
         H5FDdsmDebug("Lock released, Switched to " << H5FDdsmCommToString(this->CommChannel));
      }
      else if (newLockOwner!=communicatorId) {
        this->CommChannel = newLockOwner;
        H5FDdsmDebug("Lock retaken, Switched to " << H5FDdsmCommToString(this->CommChannel));
        if (this->Comm->GetId()!=0) {
          newLockOwner = this->BufferServiceInternals->BufferLock.Lock(newLockOwner);
        }
        if (this->Comm->GetId()==0) {
          // notify the ranks that made the original lock request
          H5FDdsmInt32 numberOfRanks = (newLockOwner==H5FDdsm_SERVER_ID) ? this->Comm->GetIntraSize() : this->Comm->GetInterSize();
          for (H5FDdsmInt32 who=0; who<numberOfRanks; who++) {
            this->SendAcknowledgment(who, this->CommChannel, newLockOwner, "LOCK ACQUIRE (user)", H5FD_DSM_RESPONSE_TAG+2);
          }
        }
      }
    }
    break;

  // H5FD_DSM_LOCK_ACQUIRE_SERIAL
  // Comes from client or server depending on communicator
  case H5FD_DSM_LOCK_ACQUIRE_SERIAL:
    if (this->Comm->GetId()==0) {
      if (this->BufferServiceInternals->BufferLock.Lock(communicatorId)) {
        H5FDdsmDebugLevel(1,"Serial mode lock acquired");
        this->SendAcknowledgment(who, this->CommChannel, communicatorId, "LOCK ACQUIRE SERIAL", H5FD_DSM_RESPONSE_TAG+2);
      }
      else {
        H5FDdsmError("User must collectively call H5FD_dsm_lock() before using serial mode");
      }
    }
    else {
      H5FDdsmError("H5FD_DSM_LOCK_ACQUIRE_SERIAL should not be called on rank > 0");
    }
    break;

  // H5FD_DSM_LOCK_RELEASE_SERIAL
  // Comes from client or server depending on communicator
  case H5FD_DSM_LOCK_RELEASE_SERIAL:
    if (this->Comm->GetId()==0) {
      if (this->BufferServiceInternals->BufferLock.Unlock(communicatorId)==communicatorId) {
        H5FDdsmDebugLevel(1,"Serial mode lock release");
      }
      else {
        H5FDdsmError("User must collectively call H5FD_dsm_lock() before using serial mode");
      }
    }
    else {
      H5FDdsmError("H5FD_DSM_LOCK_RELEASE_SERIAL should not be called on ranks > 0");
    }
    break;

  // H5FD_DSM_ACCEPT
  // Always received from server
  case H5FD_DSM_ACCEPT:
    if (!this->IsConnected) {
      status = this->Comm->Accept();
      if (status == H5FD_DSM_FAIL) {
        H5FDdsmDebug("RemoteCommAccept Failed");
        return(H5FD_DSM_FAIL);
      }
      // For one-sided communication create InterWin
      this->Comm->WinCreateData(this->DataPointer, this->Length, H5FD_DSM_INTER_COMM);
//      this->Comm->WinCreateNotification(&this->UnlockStatus, sizeof(H5FDdsmInt32), H5FD_DSM_INTER_COMM);
//      this->Comm->WinCreateLock(&this->IsClientLocked, sizeof(H5FDdsmBoolean), H5FD_DSM_INTER_COMM);
      // Send DSM information
      this->SendInfo();
      // Signal new connection
      this->SignalConnection();
      this->BufferServiceInternals->IsConnecting = H5FD_DSM_FALSE;
    }
    this->CommChannel = H5FD_DSM_ANY_COMM;
    break;

  // H5FD_DSM_DISCONNECT
  // Always received from client
  case H5FD_DSM_DISCONNECT:
    // wait for all processes to sync before doing anything
    if (this->Comm->ChannelSynced(who, &syncId, communicatorId)) {
      H5FDdsmDebug("Disconnecting");
      //if (this->Comm->GetId()==0) {
      //  // notify the ranks that made the request
      //  H5FDdsmInt32 numberOfRanks = (communicatorId==H5FDdsm_SERVER_ID) ? this->Comm->GetIntraSize() : this->Comm->GetInterSize();
      //  for (H5FDdsmInt32 who=0; who<numberOfRanks; who++) {
      //    this->SendAcknowledgment(who, this->CommChannel, communicatorId, "DISCONNECT");
      //  }
      //}
      this->IsConnected = H5FD_DSM_FALSE;
      this->IsDisconnected = H5FD_DSM_TRUE;
      this->Comm->Disconnect();
      this->CommChannel = H5FD_DSM_INTRA_COMM;
      H5FDdsmDebug("DSM disconnected, Switched to " << H5FDdsmCommToString(this->CommChannel));
      // just in case anyone is waiting, we must unlock them 
      this->SignalUnlock();
    }
    break;

  // H5FD_DSM_CLEAR_STORAGE
  case H5FD_DSM_CLEAR_STORAGE:
    // wait for all processes to sync before doing anything
    if (this->Comm->ChannelSynced(who, &syncId, communicatorId)) {
      this->ClearStorage();
    }
    break;

  // H5FD_DSM_OPCODE_DONE
  // Always received from server
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
  this->BufferServiceInternals->BufferLock.SetRank(this->Comm->GetId());
  this->BufferServiceInternals->BufferLock.SetMaster(this->IsServer);
  this->BufferServiceInternals->ServiceThread.SpawnThread(
      H5FDdsmBufferServiceThread, (void *) this);
   this->BufferServiceInternals->WaitForServiceThreadCreated();
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
H5FDdsmBufferService::ProbeCommandHeader(H5FDdsmInt32 *comm)
{
  H5FDdsmMsg msg;
  H5FDdsmInt32 status = H5FD_DSM_FAIL;

  msg.SetSource(H5FD_DSM_ANY_SOURCE);
  msg.SetTag(H5FD_DSM_ANY_TAG);
  msg.SetCommunicator(H5FD_DSM_INTRA_COMM);

  // Spin until a message is found on one of the communicators
  while (status != H5FD_DSM_SUCCESS) {
    status = this->Comm->Probe(&msg);
    if (status != H5FD_DSM_SUCCESS && this->IsConnected) {
      msg.SetCommunicator((msg.Communicator == H5FD_DSM_INTRA_COMM) ? H5FD_DSM_INTER_COMM : H5FD_DSM_INTRA_COMM);
    }
  }
  *comm = msg.Communicator;
  return(H5FD_DSM_SUCCESS);
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
    if ((putRequest.Dest == myId) && this->IsServer) {
      H5FDdsmByte *dp;
      dp = this->DataPointer;
      dp += putRequest.Address;
      memcpy(dp, putRequest.Data, putRequest.Length);
    } else {
      H5FDdsmInt32 status;
      H5FDdsmInt32 comm = (this->IsServer) ? H5FD_DSM_INTRA_COMM : H5FD_DSM_INTER_COMM;
      H5FDdsmDebugLevel(4,"PUT request to " << putRequest.Dest << " for "
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

    if ((getRequest.Dest == myId) && (/*!this->IsConnected || */this->IsServer)) {
      H5FDdsmByte *dp;
      dp = this->DataPointer;
      dp += getRequest.Address;
      memcpy(getRequest.Data, dp, getRequest.Length);
    } else {
      H5FDdsmInt32   status;
      H5FDdsmInt32 comm = (this->IsServer) ? H5FD_DSM_INTRA_COMM : H5FD_DSM_INTER_COMM;
      H5FDdsmDebugLevel(4,"Get request to " << getRequest.Dest << " for "
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
H5FDdsmBufferService::RequestLockAcquire(bool parallel)
{
  H5FDdsmInt32 status = H5FD_DSM_SUCCESS;
  
  // Which communicator are we sending our request with
  H5FDdsmInt32 comm = (this->IsServer) ? H5FD_DSM_INTRA_COMM : H5FD_DSM_INTER_COMM;

  if (parallel) {
    // Send a lock acquire request to each rank of the server
    for (H5FDdsmInt32 who = this->StartServerId; who <= this->EndServerId; who++) {
      status = this->SendCommandHeader(H5FD_DSM_LOCK_ACQUIRE, who, 0, 0, comm);
    }
  }
  else {
   // Send a lock acquire (serial) request to rank 0 of the server
   status = this->SendCommandHeader(H5FD_DSM_LOCK_ACQUIRE_SERIAL, 0, 0, 0, comm);
  }

  // Server Rank 0 will send an acknowledgement when the lock is given
  H5FDdsmInt32 communicatorId;
  status = this->ReceiveAcknowledgment(0, comm, communicatorId, "RequestLockAcquire", H5FD_DSM_RESPONSE_TAG+2);

  // server manages the lock, client nodes should copy the flags to their local lock (proxy)
  if (!this->IsServer) {
    if (!this->BufferServiceInternals->BufferLock.Lock(communicatorId)) {
      status = H5FD_DSM_FAIL;
    }
  }
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::RequestLockRelease(bool parallel)
{
  H5FDdsmInt32 status = H5FD_DSM_SUCCESS;
  
  // Which communicator are we sending our request with
  H5FDdsmInt32 comm = (this->IsServer) ? H5FD_DSM_INTRA_COMM : H5FD_DSM_INTER_COMM;

  // Complete RMA operations when the file is unlocked
  this->Comm->WindowSync();

  if (parallel) {
    // Send a lock release request to each rank of the server
    for (H5FDdsmInt32 who = this->StartServerId; who <= this->EndServerId; who++) {
      status = this->SendCommandHeader(H5FD_DSM_LOCK_RELEASE, who, this->UnlockStatus, 0, comm);
    }
  }
  else {
    // Send a lock release (serial) request to rank 0 of the server
    status = this->SendCommandHeader(H5FD_DSM_LOCK_RELEASE_SERIAL, 0, this->UnlockStatus, 0, comm);
  }

  // server manages the lock, client nodes should copy the flags to their local lock (proxy)
  if (!this->IsServer) {
    this->BufferServiceInternals->BufferLock.Unlock(H5FDdsm_CLIENT_ID);
  }
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::RequestDisconnect()
{
  H5FDdsmInt32 who, status = H5FD_DSM_SUCCESS;

  for (who = this->StartServerId; who <= this->EndServerId; who++) {
    H5FDdsmDebugLevel(1,"Send disconnection request to " << who);
    status = this->SendCommandHeader(H5FD_DSM_DISCONNECT, who, 0, 0, H5FD_DSM_INTER_COMM);
  }
  
  // Server Rank 0 will send an acknowledgement when disconnect is complete
//  H5FDdsmInt32 unused;
//  status = this->ReceiveAcknowledgment(0, 0, unused, "Disconnect");
  this->IsConnected = H5FD_DSM_FALSE;
  this->IsDisconnected = H5FD_DSM_TRUE;
  status = this->Comm->Disconnect();
  H5FDdsmDebugLevel(0,"DSM disconnected");
  return(status);
}
