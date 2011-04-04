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

// Macros to choose communication system to use
#define H5FD_DSM_COMM_MPI        0x10
#define H5FD_DSM_COMM_SOCKET     0x11
#define H5FD_DSM_COMM_MPI_RMA    0x12
#define H5FD_DSM_COMM_DMAPP      0x13

// Macros to switch between remote/local channels
#define H5FD_DSM_COMM_CHANNEL_LOCAL  0x20
#define H5FD_DSM_COMM_CHANNEL_REMOTE 0x21

typedef struct {
  H5FDdsmInt32  type;
  H5FDdsmUInt64 length;
  H5FDdsmUInt64 total_length;
  H5FDdsmUInt64 block_length;
  H5FDdsmInt32  start_server_id;
  H5FDdsmInt32  end_server_id;
} H5FDdsmInfo;

class H5FDdsmMsg;

class H5FDdsm_EXPORT H5FDdsmComm : public H5FDdsmObject {

public:
  H5FDdsmComm();
  virtual ~H5FDdsmComm();

  // Set/Get the Internal MPI Communicator
  H5FDdsmSetValueMacro(Comm, MPI_Comm);
  H5FDdsmGetValueMacro(Comm, MPI_Comm);

  // Id
  H5FDdsmGetValueMacro(Id, H5FDdsmInt32);
  H5FDdsmSetValueMacro(Id, H5FDdsmInt32);

  // Total
  H5FDdsmGetValueMacro(TotalSize, H5FDdsmInt32);
  H5FDdsmSetValueMacro(TotalSize, H5FDdsmInt32);

  // InterSize
  H5FDdsmGetValueMacro(InterSize, H5FDdsmInt32);
  H5FDdsmSetValueMacro(InterSize, H5FDdsmInt32);

  //! CommType
  H5FDdsmGetValueMacro(CommType, H5FDdsmInt32);
  H5FDdsmSetValueMacro(CommType, H5FDdsmInt32);

  //! CommChannel
  H5FDdsmGetValueMacro(CommChannel, H5FDdsmInt32);
  H5FDdsmSetValueMacro(CommChannel, H5FDdsmInt32);

  // If set to true, use one sided comm
  H5FDdsmSetValueMacro(UseOneSidedComm, H5FDdsmBoolean);
  H5FDdsmGetValueMacro(UseOneSidedComm, H5FDdsmBoolean);

  // If set to true, do not use dynamic connection
  H5FDdsmSetValueMacro(UseStaticInterComm, H5FDdsmBoolean);
  H5FDdsmGetValueMacro(UseStaticInterComm, H5FDdsmBoolean);

  H5FDdsmInt32           DupComm(MPI_Comm Source);

  virtual H5FDdsmInt32   Init();
  virtual H5FDdsmInt32   Send(H5FDdsmMsg *Msg);
  virtual H5FDdsmInt32   Receive(H5FDdsmMsg *Msg, H5FDdsmInt32 Channel=0);
  // Additional methods for one sided communications
  virtual H5FDdsmInt32   PutData(H5FDdsmMsg *DataMsg);
  virtual H5FDdsmInt32   GetData(H5FDdsmMsg *DataMsg);
  //
  virtual H5FDdsmInt32   Probe(H5FDdsmMsg *Msg);
  virtual H5FDdsmInt32   Barrier();

  virtual H5FDdsmInt32   OpenPort();
  virtual H5FDdsmInt32   ClosePort();
  virtual H5FDdsmInt32   RemoteCommAccept(void *storagePointer, H5FDdsmUInt64 storageSize);
  virtual H5FDdsmInt32   RemoteCommConnect();
  virtual H5FDdsmInt32   RemoteCommDisconnect();

  virtual H5FDdsmInt32   RemoteCommSync();
  virtual H5FDdsmInt32   RemoteCommChannelSynced(H5FDdsmInt32 who, H5FDdsmInt32 *syncId);
  virtual H5FDdsmInt32   RemoteCommRecvReady();
  virtual H5FDdsmInt32   RemoteCommSendReady();

  virtual H5FDdsmInt32   RemoteCommRecvInfo(H5FDdsmInfo *dsmInfo);
  virtual H5FDdsmInt32   RemoteCommSendInfo(H5FDdsmInfo *dsmInfo);

  virtual H5FDdsmInt32   RemoteCommSendXML(H5FDdsmString file, H5FDdsmInt32 dest);
  virtual H5FDdsmInt32   RemoteCommRecvXML(H5FDdsmString *file);

protected:
  MPI_Comm           Comm;
  H5FDdsmInt32       Id;
  H5FDdsmInt32       TotalSize;
  H5FDdsmInt32       InterSize;
  H5FDdsmInt32       CommType;
  H5FDdsmInt32       CommChannel;

  H5FDdsmBoolean     UseOneSidedComm;
  H5FDdsmBoolean     UseStaticInterComm;

  H5FDdsmInt32       SyncChannels;
};

#endif // __H5FDdsmComm_h
