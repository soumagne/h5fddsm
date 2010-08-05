/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmCommSocket.h

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
#ifndef XDMFDSMCOMMSOCKET_H
#define XDMFDSMCOMMSOCKET_H

#define H5FD_DSM_MAX_SOCKET 1024

#include "H5FDdsmComm.h"
#include "H5FDdsmSocket.h"

#include <mpi.h>

class H5FDdsm_EXPORT H5FDdsmCommSocket : public H5FDdsmComm {

public:
  H5FDdsmCommSocket();
  ~H5FDdsmCommSocket();

  // Set/Get the Internal MPI Communicator
  H5FDdsmSetValueMacro(Comm, MPI_Comm);
  H5FDdsmGetValueMacro(Comm, MPI_Comm);

  H5FDdsmGetStringMacro(DsmMasterHostName);
  void SetDsmMasterHostName(H5FDdsmConstString hostName);

  H5FDdsmGetValueMacro(DsmMasterPort, H5FDdsmInt32);
  H5FDdsmSetValueMacro(DsmMasterPort, H5FDdsmInt32);

  H5FDdsmGetValueMacro(TransferRate, H5FDdsmFloat64);

  H5FDdsmInt32   DupComm(MPI_Comm Source);

  H5FDdsmInt32   Init();
  H5FDdsmInt32   Send(H5FDdsmMsg *Msg);
  H5FDdsmInt32   Receive(H5FDdsmMsg *Msg);
  H5FDdsmInt32   Check(H5FDdsmMsg *Msg);
  H5FDdsmInt32   Barrier();

  H5FDdsmInt32   OpenPort();
  H5FDdsmInt32   ClosePort();
  H5FDdsmInt32   RemoteCommAccept();
  H5FDdsmInt32   RemoteCommConnect();
  H5FDdsmInt32   RemoteCommDisconnect();

  H5FDdsmInt32   RemoteCommRecvReady();
  H5FDdsmInt32   RemoteCommSendReady();

  H5FDdsmInt32   RemoteCommRecvInfo(H5FDdsmInt64 *length, H5FDdsmInt64 *totalLength,
      H5FDdsmInt32 *startServerId, H5FDdsmInt32 *endServerId);
  H5FDdsmInt32   RemoteCommSendInfo(H5FDdsmInt64 *length, H5FDdsmInt64 *totalLength,
      H5FDdsmInt32 *startServerId, H5FDdsmInt32 *endServerId);

  H5FDdsmInt32   RemoteCommSendXML(H5FDdsmString file, H5FDdsmInt32 dest);
  H5FDdsmInt32   RemoteCommRecvXML(H5FDdsmString *file);

  H5FDdsmInt32   RemoteCommSendSteeringCmd(H5FDdsmString cmd);
  H5FDdsmInt32   RemoteCommRecvSteeringCmd(H5FDdsmString *cmd);

  H5FDdsmInt32   HasStillData();

protected:
  H5FDdsmInt32   InterCommServerConnect();
  H5FDdsmInt32   InterCommClientConnect();

  MPI_Comm             Comm;
  H5FDdsmSocket       *InterComm[H5FD_DSM_MAX_SOCKET]; // Internode Socket Collection for data exchange
  H5FDdsmInt32         InterSize;
  H5FDdsmSocket       *DsmMasterSocket; // Used for initializing connection and send comm orders
  H5FDdsmByte          DsmMasterHostName[MPI_MAX_PORT_NAME];
  H5FDdsmInt32         DsmMasterPort;
  //
  H5FDdsmFloat64       TransferRate;
};

#endif /* XDMFDSMCOMMSOCKET_H */
