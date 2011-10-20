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

#define H5FD_DSM_OPCODE_PUT          0x01
#define H5FD_DSM_OPCODE_GET          0x02

#define H5FD_DSM_LOCK_ACQUIRE        0x03
#define H5FD_DSM_LOCK_RELEASE        0x04

#define H5FD_DSM_ACCEPT              0x05
#define H5FD_DSM_DISCONNECT          0x06

#define H5FD_DSM_COMM_SWITCH         0x07
#define H5FD_DSM_NOTIFICATION        0x08

#define H5FD_DSM_XML_EXCHANGE        0x09
#define H5FD_DSM_CLEAR_STORAGE       0x10

#define H5FD_DSM_DATA_MODIFIED       0x100

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
extern "C"{
#ifdef _WIN32
  H5FDdsm_EXPORT DWORD WINAPI H5FDdsmBufferServiceThread(void *DsmObj)
  {
    H5FDdsmBufferService *Dsm = (H5FDdsmBufferService *)DsmObj;
    Dsm->ServiceThread();
    return(0);
  }
#else
  H5FDdsm_EXPORT void *
  H5FDdsmBufferServiceThread(void *DsmObj)
  {
    H5FDdsmBufferService *Dsm = (H5FDdsmBufferService *)DsmObj;
    return(Dsm->ServiceThread());
  }
#endif
  //----------------------------------------------------------------------------
#ifdef _WIN32
  H5FDdsm_EXPORT DWORD WINAPI H5FDdsmBufferRemoteServiceThread(void *DsmObj)
  {
    H5FDdsmBufferService *Dsm = (H5FDdsmBufferService *)DsmObj;
    Dsm->RemoteServiceThread();
    return(0);
  }
#else
  H5FDdsm_EXPORT void *
  H5FDdsmBufferRemoteServiceThread(void *DsmObj)
  {
    H5FDdsmBufferService *Dsm = (H5FDdsmBufferService *)DsmObj;
    return(Dsm->RemoteServiceThread());
  }
#endif
}
//----------------------------------------------------------------------------
H5FDdsmBufferService::H5FDdsmBufferService()
{
  this->DataPointer     = NULL;

  this->IsServer        = H5FD_DSM_TRUE;

  this->IsConnecting    = H5FD_DSM_FALSE;
  this->IsConnected     = H5FD_DSM_FALSE;
#ifdef _WIN32
#if (WINVER < H5FD_DSM_CONDVAR_MINVER)
  this->ConnectionEvent = CreateEvent(NULL, TRUE, FALSE, TEXT("ConnectionEvent"));
#else
  InitializeCriticalSection  (&this->ConnectionCritSection);
  InitializeConditionVariable(&this->ConnectionCond);
#endif
#else
  pthread_mutex_init(&this->ConnectionMutex, NULL);
  pthread_cond_init (&this->ConnectionCond, NULL);
#endif

  this->IsNotified      = H5FD_DSM_FALSE;
#ifdef _WIN32
#if (WINVER < H5FD_DSM_CONDVAR_MINVER)
  this->NotificationEvent = CreateEvent(NULL, TRUE, FALSE, TEXT("NotificationEvent"));
#else
  InitializeCriticalSection  (&this->NotificationCritSection);
  InitializeConditionVariable(&this->NotificationCond);
#endif
#else
  pthread_mutex_init(&this->NotificationMutex, NULL);
  pthread_cond_init (&this->NotificationCond, NULL);
#endif

  this->Notification        = 0;
  this->NotificationOnClose = H5FD_DSM_TRUE;
  this->IsDataModified      = H5FD_DSM_FALSE;

  this->IsLocked            = H5FD_DSM_FALSE;
#ifdef _WIN32
  this->Lock = CreateMutex(NULL, FALSE, NULL);
#else
  pthread_mutex_init(&this->Lock, NULL);
#endif

  this->ReleaseLockOnClose  = H5FD_DSM_TRUE;
  this->IsSyncRequired      = H5FD_DSM_TRUE;

  this->XMLDescription      = NULL;

  this->ThreadDsmReady               = H5FD_DSM_FALSE;
  this->ThreadRemoteDsmReady         = H5FD_DSM_FALSE;
  this->ServiceThreadPtr             = 0;
  this->RemoteServiceThreadPtr       = 0;
#ifdef _WIN32
  this->ServiceThreadHandle          = NULL;
  this->RemoteServiceThreadHandle    = NULL;
#endif
}

//----------------------------------------------------------------------------
H5FDdsmBufferService::~H5FDdsmBufferService()
{
  if (this->IsServer) this->EndService();

#ifdef _WIN32
#if (WINVER < H5FD_DSM_CONDVAR_MINVER)
  CloseHandle(this->ConnectionEvent);
  CloseHandle(this->NotificationEvent);
#else
  DeleteCriticalSection(&this->ConnectionCritSection);
  DeleteCriticalSection(&this->NotificationCritSection);
#endif
#else
  pthread_mutex_destroy(&this->ConnectionMutex);
  pthread_cond_destroy (&this->ConnectionCond);
  pthread_mutex_destroy(&this->NotificationMutex);
  pthread_cond_destroy (&this->NotificationCond);
#endif

#ifdef _WIN32
  CloseHandle(this->Lock);
#else
  pthread_mutex_destroy(&this->Lock);
#endif

  if (this->XMLDescription) delete[] this->XMLDescription;
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::SignalConnection()
{
#ifdef _WIN32
#if (WINVER >= H5FD_DSM_CONDVAR_MINVER)
  EnterCriticalSection(&this->ConnectionCritSection);
#endif
#else
  pthread_mutex_lock(&this->ConnectionMutex);
#endif
  this->IsConnected = H5FD_DSM_TRUE;
#ifdef _WIN32
#if (WINVER < H5FD_DSM_CONDVAR_MINVER)
  SetEvent(this->ConnectionEvent);
#else
  WakeConditionVariable(&this->ConnectionCond);
  LeaveCriticalSection (&this->ConnectionCritSection);
#endif
#else
  pthread_cond_signal(&this->ConnectionCond);
  H5FDdsmDebug("Sent connection condition signal");
  pthread_mutex_unlock(&this->ConnectionMutex);
#endif
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::WaitForConnection()
{
  H5FDdsmInt32 ret = H5FD_DSM_FAIL;

#ifdef _WIN32
#if (WINVER >= H5FD_DSM_CONDVAR_MINVER)
  EnterCriticalSection(&this->ConnectionCritSection);
#endif
#else
  pthread_mutex_lock(&this->ConnectionMutex);
#endif
  while (!this->IsConnected) {
    H5FDdsmDebug("Thread going into wait for connection...");
#ifdef _WIN32
#if (WINVER < H5FD_DSM_CONDVAR_MINVER)
    WaitForSingleObject(this->ConnectionEvent, INFINITE);
    ResetEvent(this->ConnectionEvent);
#else
    SleepConditionVariableCS(&this->ConnectionCond, &this->ConnectionCritSection, INFINITE);
#endif
#else
    pthread_cond_wait(&this->ConnectionCond, &this->ConnectionMutex);
#endif
    H5FDdsmDebug("Thread received connection signal");
  }
  if (this->IsConnected) {
    ret = H5FD_DSM_SUCCESS;
  }
#ifdef _WIN32
#if (WINVER >= H5FD_DSM_CONDVAR_MINVER)
  LeaveCriticalSection(&this->ConnectionCritSection);
#endif
#else
  pthread_mutex_unlock(&this->ConnectionMutex);
#endif
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::SignalNotification()
{
#ifdef _WIN32
#if (WINVER >= H5FD_DSM_CONDVAR_MINVER)
  EnterCriticalSection(&this->NotificationCritSection);
#endif
#else
  pthread_mutex_lock(&this->NotificationMutex);
#endif
  this->IsNotified = H5FD_DSM_TRUE;
#ifdef _WIN32
#if (WINVER < H5FD_DSM_CONDVAR_MINVER)
  SetEvent(this->NotificationEvent);
#else
  WakeConditionVariable(&this->NotificationCond);
  LeaveCriticalSection (&this->NotificationCritSection);
#endif
#else
  pthread_cond_signal(&this->NotificationCond);
  H5FDdsmDebug("Sent notification condition signal");
  pthread_mutex_unlock(&this->NotificationMutex);
#endif
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::WaitForNotification()
{
  H5FDdsmInt32 ret = H5FD_DSM_FAIL;

#ifdef _WIN32
#if (WINVER >= H5FD_DSM_CONDVAR_MINVER)
  EnterCriticalSection(&this->NotificationCritSection);
#endif
#else
  pthread_mutex_lock(&this->NotificationMutex);
#endif
  while (!this->IsNotified && this->IsConnected) {
    H5FDdsmDebug("Thread going into wait for notification...");
#ifdef _WIN32
#if (WINVER < H5FD_DSM_CONDVAR_MINVER)
    WaitForSingleObject(this->NotificationEvent, INFINITE);
    ResetEvent(this->NotificationEvent);
#else
    SleepConditionVariableCS(&this->NotificationCond, &this->NotificationCritSection, INFINITE);
#endif
#else
    pthread_cond_wait(&this->NotificationCond, &this->NotificationMutex);
#endif
    H5FDdsmDebug("Thread received notification signal");
  }
  if (this->IsNotified && this->IsConnected) {
    this->IsNotified = H5FD_DSM_FALSE;
    ret = H5FD_DSM_SUCCESS;
  }
#ifdef _WIN32
#if (WINVER >= H5FD_DSM_CONDVAR_MINVER)
  LeaveCriticalSection(&this->NotificationCritSection);
#endif
#else
  pthread_mutex_unlock(&this->NotificationMutex);
#endif
  return(ret);
}

//----------------------------------------------------------------------------
void *
H5FDdsmBufferService::ServiceThread()
{
  H5FDdsmInt32   ReturnOpcode;

  H5FDdsmDebug("Starting DSM Service on node " << this->Comm->GetId());
  this->ThreadDsmReady = H5FD_DSM_TRUE;
  this->ServiceLoop(&ReturnOpcode);
  this->ThreadDsmReady = H5FD_DSM_FALSE;
//  H5FDdsmDebug("Ending DSM Service on node " << this->Comm->GetId() << " last op = " << ReturnOpcode);
  return((void *)this);
}

//----------------------------------------------------------------------------
void *
H5FDdsmBufferService::RemoteServiceThread()
{
  H5FDdsmInt32   ReturnOpcode;

  H5FDdsmDebug("Starting DSM Remote Service on node " << this->Comm->GetId());
  this->ThreadRemoteDsmReady = H5FD_DSM_TRUE;
  this->RemoteService(&ReturnOpcode);
  this->ThreadRemoteDsmReady = H5FD_DSM_FALSE;
  H5FDdsmDebug("Ending DSM Remote Service on node " << this->Comm->GetId() << " last op = " << ReturnOpcode);
  return((void *)this);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::ServiceLoop(H5FDdsmInt32 *ReturnOpcode)
{
  H5FDdsmInt32   op, status = H5FD_DSM_SUCCESS;

  while(status == H5FD_DSM_SUCCESS) {
    status = this->Service(&op);
    if (status != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
    if (ReturnOpcode) *ReturnOpcode = op;
    if (op == H5FD_DSM_OPCODE_DONE) return(H5FD_DSM_SUCCESS);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::Service(H5FDdsmInt32 *ReturnOpcode)
{
  H5FDdsmInt32        Opcode, who, status = H5FD_DSM_FAIL;
  H5FDdsmInt32        aLength;
  H5FDdsmAddr         Address;
  H5FDdsmByte        *datap;
  static H5FDdsmInt32 syncId = -1;

  if (syncId >= 0) {
    H5FDdsmDebug("(" << this->Comm->GetId() << ") " << "Receiving command header from " << syncId);
    status = this->ReceiveCommandHeader(&Opcode, &who, &Address, &aLength, 0, syncId);
  } else {
    H5FDdsmDebug("(" << this->Comm->GetId() << ") " << "Receiving command header from anyone");
    status = this->ReceiveCommandHeader(&Opcode, &who, &Address, &aLength);
  }
  if (status == H5FD_DSM_FAIL) {
    H5FDdsmError("Error Receiving Command Header");
    return(H5FD_DSM_FAIL);
  }
  switch(Opcode) {
  // H5FD_DSM_OPCODE_PUT
  case H5FD_DSM_OPCODE_PUT:
    H5FDdsmDebug("PUT request from " << who << " for " << aLength << " bytes @ " << Address);
    if (((H5FDdsmUInt32) aLength + Address) > this->Length) {
      H5FDdsmError("Length " << aLength << " too long for Address " << Address);
      H5FDdsmError("Server Start = " << this->StartAddress << " End = " << this->EndAddress);
      return(H5FD_DSM_FAIL);
    }
    if ((datap = this->DataPointer) == NULL) H5FDdsmError("Null Data Pointer when trying to put data");
    datap += Address;
    status = this->ReceiveData(who, datap, aLength, H5FD_DSM_PUT_DATA_TAG);
    if (status == H5FD_DSM_FAIL) {
      H5FDdsmError("ReceiveData() failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("Serviced PUT request from " << who << " for " << aLength << " bytes @ " << Address);
    break;
  // H5FD_DSM_OPCODE_GET
  case H5FD_DSM_OPCODE_GET:
    H5FDdsmDebug("(Server " << this->Comm->GetId() << ") Get request from " << who << " for " << aLength << " bytes @ " << Address);
    if (((H5FDdsmUInt32) aLength + Address) > this->Length) {
      H5FDdsmError("Length " << aLength << " too long for Address " << Address);
      H5FDdsmError("Server Start = " << this->StartAddress << " End = " << this->EndAddress);
      return(H5FD_DSM_FAIL);
    }
    if ((datap = this->DataPointer) == NULL) H5FDdsmError("Null Data Pointer when trying to get data");
    datap += Address;
    status = this->SendData(who, datap, aLength, H5FD_DSM_GET_DATA_TAG);
    if (status == H5FD_DSM_FAIL) {
      H5FDdsmError("SendData() failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("(Server " << this->Comm->GetId() << ") Serviced GET request from " << who << " for " << aLength << " bytes @ " << Address);
    break;
  // H5FD_DSM_OPCODE_DONE
  case H5FD_DSM_OPCODE_DONE:
    break;
#ifdef H5FD_DSM_HAVE_STEERING
  // H5FD_DSM_COMM_SWITCH
  case H5FD_DSM_COMM_SWITCH:
    if (this->Comm->GetCommChannel() == H5FD_DSM_INTRA_COMM) {
      // The remote service must be stopped before the main service can start to listen on the inter-communicator
      this->EndRemoteService();
      this->Comm->SetCommChannel(H5FD_DSM_INTER_COMM);
      H5FDdsmDebug("Listening on Remote");
    } else {
      this->Comm->SetCommChannel(H5FD_DSM_INTRA_COMM);
      H5FDdsmDebug("Listening on Local");
    }
    break;
  // H5FD_DSM_LOCK_RELEASE
  case H5FD_DSM_LOCK_RELEASE:
    if (this->Comm->ChannelSynced(who, &syncId) || !this->IsConnected) {
      this->Comm->SetCommChannel(H5FD_DSM_INTRA_COMM);
      if (!this->IsLocked) {
        H5FDdsmLockError("already released");
      } else {
        this->IsLocked = H5FD_DSM_FALSE;
        H5FDdsmLockDebug("released");
      }
#ifdef _WIN32
      ReleaseMutex(this->Lock);
#else
      pthread_mutex_unlock(&this->Lock);
#endif
      this->Comm->Barrier();
      if (this->IsConnected) {
          if (!this->RemoteServiceThreadPtr) this->StartRemoteService();
      }
    }
    break;
#endif
  // H5FD_DSM_ACCEPT
  case H5FD_DSM_ACCEPT:
    if (!this->IsConnected) {
      status = this->Comm->Accept(this->DataPointer, this->Length);
      if (status == H5FD_DSM_FAIL) {
        H5FDdsmDebug("RemoteCommAccept Failed");
        return(H5FD_DSM_FAIL);
      }
      this->IsConnecting = H5FD_DSM_FALSE;
      // send DSM information
      this->SendInfo();
      if (this->AddressMapper->GetDsmType() == H5FD_DSM_TYPE_DYNAMIC_MASK) this->ReceiveMaskLength();
      this->SignalConnection();
    }
    if (this->Comm->GetUseOneSidedComm()) {
      this->Comm->SetCommChannel(H5FD_DSM_INTER_COMM);
      this->Comm->WindowSync();
    } else {
#ifdef H5FD_DSM_HAVE_STEERING
      if (!this->RemoteServiceThreadPtr) this->StartRemoteService();
#else
      this->Comm->SetCommChannel(H5FD_DSM_INTER_COMM);
      this->Comm->Barrier();
#endif
    }
    break;
  // H5FD_DSM_DISCONNECT
  case H5FD_DSM_DISCONNECT:
    if (this->Comm->ChannelSynced(who, &syncId)) {
      H5FDdsmDebug("( " << this->Comm->GetId() << " ) Freeing now remote channel");
      this->Comm->Disconnect();
      this->IsConnected = H5FD_DSM_FALSE;
      H5FDdsmDebug("DSM disconnected on " << this->Comm->GetId() << ", Switched to Local channel");
      // Because we may have been waiting for a notification
      this->SignalNotification();
    }
    break;
  // H5FD_DSM_NOTIFICATION
  case H5FD_DSM_NOTIFICATION:
    if (this->Comm->ChannelSynced(who, &syncId)) {
      this->Comm->SetCommChannel(H5FD_DSM_INTRA_COMM);
      if (Address & H5FD_DSM_DATA_MODIFIED) {
        this->IsDataModified = H5FD_DSM_TRUE;
        this->Notification = (H5FDdsmInt32)Address - H5FD_DSM_DATA_MODIFIED;
      } else {
        this->Notification = (H5FDdsmInt32)Address;
      }
      // When a notification is found, the server keeps the lock
      // and only releases it when the requested task is over
      this->ReleaseLockOnClose = H5FD_DSM_FALSE;
#ifdef H5FD_DSM_HAVE_STEERING
      if (this->Comm->GetUseOneSidedComm()) this->IsLocked = H5FD_DSM_TRUE;
#else
      this->IsLocked = H5FD_DSM_TRUE;
#endif
      H5FDdsmDebug("(" << this->Comm->GetId() << ") " << "Notification " <<
          this->Notification << ", Switched to Local channel");
      this->Comm->Barrier();
      this->SignalNotification();
    }
    break;
  // H5FD_DSM_CLEAR_STORAGE
  case H5FD_DSM_CLEAR_STORAGE:
    if (this->Comm->ChannelSynced(who, &syncId)) {
      this->ClearStorage();
    }
    break;
  // H5FD_DSM_XML_EXCHANGE
  case H5FD_DSM_XML_EXCHANGE:
    // Receive XML File and put it in a DSM buffer field for further reading
    this->Comm->RecvXML(&this->XMLDescription);
    break;
  // DEFAULT
  default :
    H5FDdsmError("Unknown Opcode " << Opcode);
    return(H5FD_DSM_FAIL);
  }
  if (ReturnOpcode) *ReturnOpcode = Opcode;
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::StartService()
{
  H5FDdsmDebug("Creating service thread...");
#ifdef _WIN32
  this->ServiceThreadHandle = CreateThread(NULL, 0, H5FDdsmBufferServiceThread, (void *) this, 0, &this->ServiceThreadPtr);
#else
  // Start another thread to handle DSM requests from other nodes
  pthread_create(&this->ServiceThreadPtr, NULL, &H5FDdsmBufferServiceThread, (void *) this);
#endif
  // Spinning creates deadlocks
  // while (!this->ThreadDsmReady) {
  // Spin until service initialized
  // }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::EndService()
{
  if (this->ServiceThreadPtr) {
    if (this->IsConnecting && !this->IsConnected) {
      if (this->Comm->GetInterCommType() == H5FD_DSM_COMM_SOCKET) {
        this->Comm->ClosePort();
#ifdef _WIN32
        WaitForSingleObject(this->ServiceThreadHandle, INFINITE);
#else
        pthread_join(this->ServiceThreadPtr, NULL);
#endif
      } else {
#ifdef _WIN32
        WaitForSingleObject(this->ServiceThreadHandle, 0);
#else
        pthread_cancel(this->ServiceThreadPtr);
#endif
      }
    } else {
      if (this->Comm->GetId() == 0) this->SendDone();
#ifdef _WIN32
      WaitForSingleObject(this->ServiceThreadHandle, INFINITE);
    }
    CloseHandle(this->ServiceThreadHandle);
    this->ServiceThreadHandle = NULL;
#else
      pthread_join(this->ServiceThreadPtr, NULL);
    }
#endif
    this->ServiceThreadPtr = 0;
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::RemoteService(H5FDdsmInt32 *ReturnOpcode)
{
  H5FDdsmInt32        Opcode, who, status = H5FD_DSM_FAIL;
  H5FDdsmInt32        aLength;
  H5FDdsmAddr         Address;
  H5FDdsmInt32        syncId = -1;

  for (H5FDdsmInt32 i = 0; i < this->Comm->GetInterSize(); i++) {
    // This receive always gets data from the inter-communicator
    H5FDdsmInt32 useInterCommunicator = 1;
    status = this->ReceiveCommandHeader(&Opcode, &who, &Address, &aLength, useInterCommunicator, i);

    if (status == H5FD_DSM_FAIL) {
      H5FDdsmError("Error Receiving Command Header");
      return(H5FD_DSM_FAIL);
    }

    switch(Opcode) {
    case H5FD_DSM_LOCK_ACQUIRE:
      if (this->Comm->ChannelSynced(who, &syncId)) {
#ifdef _WIN32
        WaitForSingleObject(this->Lock, INFINITE);
#else
        pthread_mutex_lock(&this->Lock);
#endif
        if (this->IsLocked) {
          H5FDdsmLockError("already acquired");
        } else {
          this->IsLocked = H5FD_DSM_TRUE;
          H5FDdsmLockDebug("acquired");
        }
        who = this->Comm->GetId();
        status = H5FD_DSM_SUCCESS;
        H5FDdsmDebug("Send request communicator switch to " << who);
        status = this->SendCommandHeader(H5FD_DSM_COMM_SWITCH, who, 0, 0);
        if (status == H5FD_DSM_FAIL) {
          H5FDdsmError("Error Sending Command Header");
          return(H5FD_DSM_FAIL);
        }
      }
      break;
    default:
      H5FDdsmError("Unknown Remote Service Opcode " << Opcode);
      return(H5FD_DSM_FAIL);
    }
  }
  if (ReturnOpcode) *ReturnOpcode = Opcode;
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::StartRemoteService()
{
  // create a single thread listening on the remote transaction request
  H5FDdsmDebug("Creating remote service thread...");
#ifdef _WIN32
  this->RemoteServiceThreadHandle = CreateThread(NULL, 0, H5FDdsmBufferRemoteServiceThread, (void *) this, 0, &this->RemoteServiceThreadPtr);
#else
  // Start another thread to handle DSM requests from other nodes
  pthread_create(&this->RemoteServiceThreadPtr, NULL, &H5FDdsmBufferRemoteServiceThread, (void *) this);
#endif
  // Spinning creates deadlocks
  // while (!this->ThreadRemoteDsmReady) {
  // Spin until service initialized
  // }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::EndRemoteService()
{
#ifdef _WIN32
  if (this->RemoteServiceThreadPtr) {
    WaitForSingleObject(this->RemoteServiceThreadHandle, INFINITE);
    CloseHandle(this->RemoteServiceThreadHandle);
    this->RemoteServiceThreadPtr = 0;
    this->RemoteServiceThreadHandle = NULL;
  }
#else
  if (this->RemoteServiceThreadPtr) {
    pthread_join(this->RemoteServiceThreadPtr, NULL);
    this->RemoteServiceThreadPtr = 0;
  }
#endif
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::Put(H5FDdsmAddr address, H5FDdsmUInt64 length, H5FDdsmPointer data)
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
      H5FDdsmDebug("PUT request to " << putRequest.Dest << " for "
          << putRequest.Length << " bytes @ " << putRequest.Address);
      if (putRequest.Dest > this->EndServerId || putRequest.Address > this->Length) {
        H5FDdsmError("Exceeded DSM address range");
        return(H5FD_DSM_FAIL);
      }
      if (!this->Comm->GetUseOneSidedComm()) {
        status = this->SendCommandHeader(H5FD_DSM_OPCODE_PUT, putRequest.Dest, putRequest.Address, putRequest.Length);
        if (status == H5FD_DSM_FAIL) {
          H5FDdsmError("Failed to send PUT Header to " << putRequest.Dest);
          return(H5FD_DSM_FAIL);
        }
      }
      status = this->SendData(putRequest.Dest, putRequest.Data, putRequest.Length, H5FD_DSM_PUT_DATA_TAG, putRequest.Address);
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
H5FDdsmBufferService::Get(H5FDdsmAddr address, H5FDdsmUInt64 length, H5FDdsmPointer data)
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
      H5FDdsmDebug("Get request to " << getRequest.Dest << " for "
          << getRequest.Length << " bytes @ " << getRequest.Address);
      if (getRequest.Dest > this->EndServerId || getRequest.Address > this->Length) {
        H5FDdsmError("Exceeded DSM address range");
        return(H5FD_DSM_FAIL);
      }
      if (!this->Comm->GetUseOneSidedComm()) {
        status = this->SendCommandHeader(H5FD_DSM_OPCODE_GET, getRequest.Dest, getRequest.Address, getRequest.Length);
        if (status == H5FD_DSM_FAIL) {
          H5FDdsmError("Failed to send GET Header to " << getRequest.Dest);
          return(H5FD_DSM_FAIL);
        }
      }
      status = this->ReceiveData(getRequest.Dest, getRequest.Data, getRequest.Length, H5FD_DSM_GET_DATA_TAG, getRequest.Address);
      if (status == H5FD_DSM_FAIL) {
        H5FDdsmError("Failed to receive " << getRequest.Length << " bytes of data from " << getRequest.Dest);
        return(H5FD_DSM_FAIL);
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
#ifdef _WIN32
    WaitForSingleObject(this->Lock, INFINITE);
#else
    pthread_mutex_lock(&this->Lock);
    // TODO should we add a sync here
#endif
  } else {
    if (this->Comm->GetUseOneSidedComm()) {
      // After possible RMA put, need to sync windows before further operations
      if (this->IsSyncRequired) this->Comm->WindowSync();
      this->IsSyncRequired = H5FD_DSM_FALSE;
    } else {
#ifdef H5FD_DSM_HAVE_STEERING
      for (H5FDdsmInt32 who = this->StartServerId ; who <= this->EndServerId ; who++) {
        H5FDdsmDebug("Send request LOCK acquire to " << who);
        status = this->SendCommandHeader(H5FD_DSM_LOCK_ACQUIRE, who, 0, 0);
      }
#else
      this->IsSyncRequired = H5FD_DSM_FALSE;
#endif
    }
  }

  if (this->IsLocked) {
    H5FDdsmLockError("already acquired");
  } else {
    this->IsLocked = H5FD_DSM_TRUE;
    H5FDdsmLockDebug("acquired");
  }

  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::RequestLockRelease()
{
  H5FDdsmInt32 status = H5FD_DSM_SUCCESS;

  if (!this->IsLocked) {
    H5FDdsmLockError("already released");
  } else {
    this->IsLocked = H5FD_DSM_FALSE;
    H5FDdsmLockDebug("released");
  }

  if (this->IsServer) {
#ifdef _WIN32
    ReleaseMutex(this->Lock);
#else
    pthread_mutex_unlock(&this->Lock);
#endif
    if (this->IsConnected) {
      if (this->Comm->GetUseOneSidedComm()) {
        this->RequestAccept();
      } else {
#ifdef H5FD_DSM_HAVE_STEERING
        if (!this->RemoteServiceThreadPtr) this->StartRemoteService();
#else
        this->RequestAccept();
#endif
      }
    }
  } else {
    if (this->Comm->GetUseOneSidedComm()) {
      // Nothing for now
    } else {
#ifdef H5FD_DSM_HAVE_STEERING
      for (H5FDdsmInt32 who = this->StartServerId ; who <= this->EndServerId ; who++) {
        H5FDdsmDebug("Send request LOCK release to " << who);
        status = this->SendCommandHeader(H5FD_DSM_LOCK_RELEASE, who, 0, 0);
      }
#endif
    }
  }

  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::RequestAccept()
{
  H5FDdsmInt32 who, status = H5FD_DSM_SUCCESS;

  who = this->Comm->GetId();
  H5FDdsmDebug("Send request accept to " << who);
  if (!this->IsConnected) this->IsConnecting = H5FD_DSM_TRUE;
  status = this->SendCommandHeader(H5FD_DSM_ACCEPT, who, 0, 0);

  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::RequestDisconnect()
{
  H5FDdsmInt32 who, status = H5FD_DSM_SUCCESS;

  this->RequestLockAcquire();

  for (who = this->StartServerId ; who <= this->EndServerId ; who++) {
    H5FDdsmDebug("Send disconnection request to " << who);
    status = this->SendCommandHeader(H5FD_DSM_DISCONNECT, who, 0, 0);
  }
  status = this->Comm->Disconnect();
  this->IsConnected = H5FD_DSM_FALSE;
  H5FDdsmDebug("DSM disconnected on " << this->Comm->GetId());
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::RequestNotification()
{
  H5FDdsmInt32 who, status = H5FD_DSM_SUCCESS;

  // On next lock acquire, a synchronization is required
#ifdef H5FD_DSM_HAVE_STEERING
  if (this->Comm->GetUseOneSidedComm()) this->IsSyncRequired = H5FD_DSM_TRUE;
#else
  this->IsSyncRequired = H5FD_DSM_TRUE;
#endif

  // No mutex here, only informal since it's the client
  if (!this->IsLocked) {
    H5FDdsmLockError("released or notification already released");
  } else {
    this->IsLocked = H5FD_DSM_FALSE;
    H5FDdsmLockDebug("released");
  }

  for (who = this->StartServerId ; who <= this->EndServerId ; who++) {
    H5FDdsmAddr localFlag = 0;
    H5FDdsmDebug("Send request server notification to " << who << " with level " << this->Notification);
    // for convenience
    if (this->IsDataModified) localFlag = H5FD_DSM_DATA_MODIFIED;
    localFlag |= this->Notification;
    status = this->SendCommandHeader(H5FD_DSM_NOTIFICATION, who, localFlag, 0);
  }
  if (this->IsDataModified) this->IsDataModified = H5FD_DSM_FALSE;
  if (!this->Notification) this->Notification = 0;

  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::RequestClearStorage()
{
  H5FDdsmInt32 who, status = H5FD_DSM_SUCCESS;

  for (who = this->StartServerId ; who <= this->EndServerId ; who++) {
    H5FDdsmDebug("Send request clear storage to " << who);
    status = this->SendCommandHeader(H5FD_DSM_CLEAR_STORAGE, who, 0, 0);
  }
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmBufferService::RequestXMLExchange()
{
  H5FDdsmInt32 who, status = H5FD_DSM_SUCCESS;

  if (this->Comm->GetId() == 0) {
    for (who = this->StartServerId ; who <= this->EndServerId ; who++) {
      H5FDdsmDebug("Send request xml channel to " << who);
      status = this->SendCommandHeader(H5FD_DSM_XML_EXCHANGE, who, 0, 0);
    }
  }
  return(status);
}
