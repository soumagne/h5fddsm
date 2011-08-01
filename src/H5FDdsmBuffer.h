/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmBuffer.h

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
#ifndef __H5FDdsmBuffer_h
#define __H5FDdsmBuffer_h

#include "H5FDdsmDriver.h"

#ifdef _WIN32
  #include <windows.h>
  #define H5FD_DSM_CONDVAR_MINVER _WIN32_WINNT_LONGHORN
#else
  #include <pthread.h>
#endif

#ifdef H5FD_DSM_HAVE_STEERING
class H5FDdsmSteerer;
#endif

//! Base comm object for Distributed Shared Memory implementation
/*!
*/
class H5FDdsm_EXPORT H5FDdsmBuffer : public H5FDdsmDriver {

  public:
    H5FDdsmBuffer();
    virtual ~H5FDdsmBuffer();

    // Is the DSMBuffer auto allocated within the driver or not
    H5FDdsmGetValueMacro(IsAutoAllocated, H5FDdsmBoolean);
    H5FDdsmSetValueMacro(IsAutoAllocated, H5FDdsmBoolean);

    // Is the DSMBuffer in server or client mode
    H5FDdsmGetValueMacro(IsServer, H5FDdsmBoolean);
    H5FDdsmSetValueMacro(IsServer, H5FDdsmBoolean);

    // Is the DSMBuffer connected
    H5FDdsmGetValueMacro(IsConnected, H5FDdsmBoolean);
    H5FDdsmSetValueMacro(IsConnected, H5FDdsmBoolean);
    H5FDdsmInt32 SignalConnection();
    H5FDdsmInt32 WaitForConnection();

    // Is a DSM notification set
    H5FDdsmGetValueMacro(IsNotified, H5FDdsmBoolean);
    H5FDdsmSetValueMacro(IsNotified, H5FDdsmBoolean);
    H5FDdsmInt32 SignalNotification();
    H5FDdsmInt32 WaitForNotification();

    // Set/Get Notification
    H5FDdsmGetValueMacro(Notification, H5FDdsmInt32);
    H5FDdsmSetValueMacro(Notification, H5FDdsmInt32);

    // Is the server automatically notified on H5Fclose or not
    H5FDdsmGetValueMacro(NotificationOnClose, H5FDdsmBoolean);
    H5FDdsmSetValueMacro(NotificationOnClose, H5FDdsmBoolean);

    // Has the data been modified
    H5FDdsmGetValueMacro(IsDataModified, H5FDdsmBoolean);
    H5FDdsmSetValueMacro(IsDataModified, H5FDdsmBoolean);

    // Is the DSM locked
    H5FDdsmGetValueMacro(IsLocked, H5FDdsmBoolean);
    H5FDdsmSetValueMacro(IsLocked, H5FDdsmBoolean);

    // Releases the lock automatically on H5Fclose or not
    H5FDdsmGetValueMacro(ReleaseLockOnClose, H5FDdsmBoolean);
    H5FDdsmSetValueMacro(ReleaseLockOnClose, H5FDdsmBoolean);

    // Is the DSMBuffer open for Read Only operations
    H5FDdsmGetValueMacro(IsReadOnly, H5FDdsmBoolean);
    H5FDdsmSetValueMacro(IsReadOnly, H5FDdsmBoolean);

    // Is synchronization required
    H5FDdsmGetValueMacro(IsSyncRequired, H5FDdsmBoolean);
    H5FDdsmSetValueMacro(IsSyncRequired, H5FDdsmBoolean);

    // Debug: add ability to send xml string
    H5FDdsmGetStringMacro(XMLDescription);
    H5FDdsmSetStringMacro(XMLDescription);

    void *         ServiceThread();
    void *         RemoteServiceThread();

    H5FDdsmInt32   ServiceLoop(H5FDdsmInt32 *ReturnOpcode=0);
    H5FDdsmInt32   Service(H5FDdsmInt32 *ReturnOpcode=0);
    H5FDdsmInt32   StartService();
    H5FDdsmInt32   EndService();

    H5FDdsmInt32   RemoteService(H5FDdsmInt32 *ReturnOpcode=0);
    H5FDdsmInt32   StartRemoteService();
    H5FDdsmInt32   EndRemoteService();

    H5FDdsmInt32   Put(H5FDdsmAddr Address, H5FDdsmUInt64 Length, H5FDdsmPointer Data);
    H5FDdsmInt32   Get(H5FDdsmAddr Address, H5FDdsmUInt64 Length, H5FDdsmPointer Data);

    H5FDdsmInt32   RequestLockAcquire();
    H5FDdsmInt32   RequestLockRelease();

    H5FDdsmInt32   RequestAccept();
    H5FDdsmInt32   RequestDisconnect();

    H5FDdsmInt32   RequestNotification();

    H5FDdsmInt32   RequestClearStorage();
    H5FDdsmInt32   RequestXMLExchange();

#ifdef H5FD_DSM_HAVE_STEERING
    H5FDdsmSteerer *GetSteerer() { return(Steerer); }
#endif

  protected:
    H5FDdsmBoolean          IsAutoAllocated;
    H5FDdsmBoolean          IsServer;

    H5FDdsmBoolean          IsConnecting;
    H5FDdsmBoolean          IsConnected;
#ifdef _WIN32
#if (WINVER <= H5FD_DSM_CONDVAR_MINVER)
    HANDLE                  ConnectionEvent;
#else
    CRITICAL_SECTION        ConnectionCritSection;
    CONDITION_VARIABLE      ConnectionCond;
#endif
#else
    pthread_mutex_t         ConnectionMutex;
    pthread_cond_t          ConnectionCond;
#endif

    H5FDdsmBoolean          IsNotified;
#ifdef _WIN32
#if (WINVER <= H5FD_DSM_CONDVAR_MINVER)
    HANDLE                  NotificationEvent;
#else
    CRITICAL_SECTION        NotificationCritSection;
    CONDITION_VARIABLE      NotificationCond;
#endif
#else
    pthread_mutex_t         NotificationMutex;
    pthread_cond_t          NotificationCond;
#endif

    H5FDdsmInt32            Notification;
    H5FDdsmBoolean          NotificationOnClose;
    H5FDdsmBoolean          IsDataModified;

    H5FDdsmBoolean          IsLocked;
#ifdef _WIN32
    HANDLE                  Lock;
#else
    pthread_mutex_t         Lock;
#endif
    H5FDdsmBoolean          ReleaseLockOnClose;
    H5FDdsmBoolean          IsReadOnly;
    H5FDdsmBoolean          IsSyncRequired;

    H5FDdsmString           XMLDescription;

#ifdef H5FD_DSM_HAVE_STEERING
    H5FDdsmSteerer         *Steerer;
#endif

    H5FDdsmBoolean          ThreadDsmReady;
    H5FDdsmBoolean          ThreadRemoteDsmReady;
#ifdef _WIN32
    DWORD                   ServiceThreadPtr;
    HANDLE                  ServiceThreadHandle;
    DWORD                   RemoteServiceThreadPtr;
    HANDLE                  RemoteServiceThreadHandle;
#else
    pthread_t               ServiceThreadPtr;
    pthread_t               RemoteServiceThreadPtr;
#endif
};

#endif // __H5FDdsmBuffer_h
