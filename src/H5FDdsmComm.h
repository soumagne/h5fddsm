/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmComm.h

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
#ifndef __H5FDdsmComm_h
#define __H5FDdsmComm_h

#include "H5FDdsmObject.h"

#include <mpi.h>

//! Base comm object for Distributed Shared Memory implementation

// Macros to choose inter communication system to use
#define H5FD_DSM_COMM_SOCKET    0x10
#define H5FD_DSM_COMM_MPI       0x11
#define H5FD_DSM_COMM_MPI_RMA   0x12
#define H5FD_DSM_COMM_DMAPP     0x13
#define H5FD_DSM_COMM_UGNI      0x14

// Macros to switch between intra/inter communicators
#define H5FD_DSM_ANY_COMM    0x20
#define H5FD_DSM_INTRA_COMM  0x21
#define H5FD_DSM_INTER_COMM  0x22

struct H5FDdsmMsg;

H5FDdsm_EXPORT H5FDdsmConstString H5FDdsmCommToString(H5FDdsmInt32 tag);

class H5FDdsm_EXPORT H5FDdsmComm : public H5FDdsmObject {

public:
  H5FDdsmComm();
  virtual ~H5FDdsmComm();

  // If set to true, one sided comm is used
  H5FDdsmGetValueMacro(UseOneSidedComm, H5FDdsmBoolean);

  // Get the internal MPI Communicator
  H5FDdsmGetValueMacro(IntraComm, MPI_Comm);
  // Duplicate and Set the internal MPI Communicator
  H5FDdsmInt32 DupComm(MPI_Comm source);

  // Get the Internal MPI Window
  H5FDdsmGetValueMacro(IntraWin, MPI_Win);

  // Get Id (local to Intra-Communicator)
  H5FDdsmGetValueMacro(Id, H5FDdsmInt32);

  // Get IntraSize
  H5FDdsmGetValueMacro(IntraSize, H5FDdsmInt32);

  // If set to true, do not use dynamic connection
  H5FDdsmSetValueMacro(UseStaticInterComm, H5FDdsmBoolean);
  H5FDdsmGetValueMacro(UseStaticInterComm, H5FDdsmBoolean);

  // Get InterCommType
  H5FDdsmGetValueMacro(InterCommType, H5FDdsmInt32);

  // Get InterSize
  H5FDdsmGetValueMacro(InterSize, H5FDdsmInt32);

  virtual H5FDdsmInt32   Init();

  // Point to point methods
  virtual H5FDdsmInt32   Send(H5FDdsmMsg *msg);
  virtual H5FDdsmInt32   Receive(H5FDdsmMsg *msg);
  virtual H5FDdsmInt32   Probe(H5FDdsmMsg *msg);

  // Additional methods for one sided communications
  virtual H5FDdsmInt32   WinCreateData(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm);
  virtual H5FDdsmInt32   PutData(H5FDdsmMsg *msg);
  virtual H5FDdsmInt32   GetData(H5FDdsmMsg *msg);
  virtual H5FDdsmInt32   WindowSyncData();
  // TODO For compat - will be removed later
  H5FDdsmInt32   WindowSync();

  // Notification
  virtual H5FDdsmInt32   WinCreateNotification(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm);
  virtual H5FDdsmInt32   PutNotification(H5FDdsmMsg *msg);
  virtual H5FDdsmInt32   GetNotification(H5FDdsmMsg *msg);
  // Lock
  virtual H5FDdsmInt32   WinCreateLock(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm);
  virtual H5FDdsmInt32   PutLock(H5FDdsmMsg *msg);
  virtual H5FDdsmInt32   GetLock(H5FDdsmMsg *msg);

  // Collective methods
  virtual H5FDdsmInt32   Barrier(H5FDdsmInt32 comm);
  virtual H5FDdsmInt32   Broadcast(H5FDdsmMsg *msg);
  // TODO For compat - will be removed later
  H5FDdsmInt32   Barrier();

  // InterComm creation methods
  virtual H5FDdsmInt32   OpenPort();
  virtual H5FDdsmInt32   ClosePort();
  virtual H5FDdsmInt32   Accept();
  virtual H5FDdsmInt32   Connect();
  virtual H5FDdsmInt32   Disconnect();

  // Sync two sided channels
  H5FDdsmInt32           ChannelSynced(H5FDdsmInt32 who, H5FDdsmInt32 *syncId, H5FDdsmBoolean fromServer=H5FD_DSM_FALSE);

protected:
  // Wrappers to MPI functions
  H5FDdsmInt32       MpiCommFree(MPI_Comm *comm);
  H5FDdsmInt32       MpiSend(H5FDdsmMsg *msg, MPI_Comm comm);
  H5FDdsmInt32       MpiReceive(H5FDdsmMsg *msg, MPI_Comm comm);
  H5FDdsmInt32       MpiProbe(H5FDdsmMsg *msg, MPI_Comm comm);
  H5FDdsmInt32       MpiWinCreateLocal(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, MPI_Win *win);
  H5FDdsmInt32       MpiWinFree(MPI_Win *win);
  H5FDdsmInt32       MpiPut(H5FDdsmMsg *msg, MPI_Win win);
  H5FDdsmInt32       MpiGet(H5FDdsmMsg *msg, MPI_Win win);
  H5FDdsmInt32       MpiBarrier(MPI_Comm comm);
  H5FDdsmInt32       MpiBroadcast(H5FDdsmMsg *msg, MPI_Comm comm);

  H5FDdsmBoolean     UseOneSidedComm;

  MPI_Comm           IntraComm;
  MPI_Win            IntraWin;
  H5FDdsmInt32       Id;
  H5FDdsmInt32       IntraSize;

  H5FDdsmBoolean     UseStaticInterComm;
  H5FDdsmInt32       InterCommType;
  H5FDdsmInt32       InterSize;

  H5FDdsmInt32       SyncChannels;
};

#endif // __H5FDdsmComm_h
