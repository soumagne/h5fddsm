/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmBufferService.h

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
#ifndef __H5FDdsmBufferService_h
#define __H5FDdsmBufferService_h

#include "H5FDdsmBuffer.h"

// Buffer service opcodes
#define H5FD_DSM_OPCODE_PUT          0x01
#define H5FD_DSM_OPCODE_GET          0x02

#define H5FD_DSM_LOCK_ACQUIRE        0x03
#define H5FD_DSM_LOCK_ACQUIRE_SERIAL 0x04
#define H5FD_DSM_LOCK_RELEASE        0x05
#define H5FD_DSM_LOCK_RELEASE_SERIAL 0x06

#define H5FD_DSM_ACCEPT              0x10
#define H5FD_DSM_DISCONNECT          0x11

#define H5FD_DSM_CLEAR_STORAGE       0x20

#define H5FD_DSM_OPCODE_DONE         0xFF

H5FDdsmConstString H5FDdsmOpcodeToString(H5FDdsmInt32 code);
H5FDdsmConstString H5FDdsmNotificationToString(H5FDdsmInt32 code);

//! Base comm object for Distributed Shared Memory implementation
/*!
*/
class H5FDdsm_EXPORT H5FDdsmBufferService : public H5FDdsmBuffer {

  public:
    H5FDdsmBufferService();
    virtual ~H5FDdsmBufferService();

    H5FDdsmInt32 WaitForConnection();
    H5FDdsmInt32 WaitForUnlock();

    // Set/Get UnlockStatus
    H5FDdsmGetValueMacro(UnlockStatus, H5FDdsmInt32);
    H5FDdsmSetValueMacro(UnlockStatus, H5FDdsmInt32);

    void SetSychronizationCount(H5FDdsmInt32 count);

    // Debug: add ability to send xml string
    H5FDdsmGetStringMacro(XMLDescription);
    H5FDdsmSetStringMacro(XMLDescription);

    void *         BufferServiceThread();

    H5FDdsmInt32   BufferServiceLoop(H5FDdsmInt32 *returnOpcode=0);
    H5FDdsmInt32   BufferService(H5FDdsmInt32 *returnOpcode=0);
    H5FDdsmInt32   StartBufferService();
    H5FDdsmInt32   EndBufferService();

    // Set the Service thread in listening mode for new connections
    H5FDdsmInt32   SendAccept();

    // End the Service thread
    H5FDdsmInt32   SendDone();

    // Put/Get Data of size Lenght at address Address
    H5FDdsmInt32   Put(H5FDdsmAddr address, H5FDdsmUInt64 length, H5FDdsmPointer data);
    H5FDdsmInt32   Get(H5FDdsmAddr address, H5FDdsmUInt64 length, H5FDdsmPointer data, H5FDdsmBoolean blocking=H5FD_DSM_TRUE);

    H5FDdsmInt32   RequestLockAcquire(H5FDdsmBoolean parallel=H5FD_DSM_TRUE);
    H5FDdsmInt32   RequestLockRelease(H5FDdsmBoolean parallel=H5FD_DSM_TRUE);

    H5FDdsmInt32   RequestDisconnect();

  protected:
    H5FDdsmGetValueMacro(IsConnected, H5FDdsmBoolean);
    H5FDdsmGetValueMacro(IsDisconnected, H5FDdsmBoolean);
    H5FDdsmInt32   SignalConnection();
    //
    H5FDdsmBoolean GetIsUnlocked();
    H5FDdsmInt32   SignalUnlock(H5FDdsmBoolean isDisconnected);
    //
    H5FDdsmInt32   ProbeCommandHeader(H5FDdsmInt32 *comm);

    H5FDdsmInt32            CommChannel;
    H5FDdsmBoolean          IsConnected;
    H5FDdsmBoolean          IsDisconnected;
    H5FDdsmInt32            UnlockStatus;
    //
    H5FDdsmString           XMLDescription;
    //
    struct H5FDdsmBufferServiceInternals;
    H5FDdsmBufferServiceInternals *BufferServiceInternals;
    friend class H5FDdsmManager;
};

#endif // __H5FDdsmBufferService_h
