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
#ifndef __H5FDdsmCommSocket_h
#define __H5FDdsmCommSocket_h

#define H5FD_DSM_MAX_SOCKET 1024

#include "H5FDdsmComm.h"

class H5FDdsmSocket;

class H5VLdso_EXPORT H5FDdsmCommSocket : public H5FDdsmComm {

public:
  H5FDdsmCommSocket();
  virtual ~H5FDdsmCommSocket();

  H5FDdsmGetStringMacro(DsmMasterHostName);
  void SetDsmMasterHostName(H5FDdsmConstString hostName);

  H5FDdsmGetValueMacro(DsmMasterPort, H5FDdsmInt32);
  H5FDdsmSetValueMacro(DsmMasterPort, H5FDdsmInt32);

  H5FDdsmInt32   Init();
  H5FDdsmInt32   Send(H5FDdsmMsg *msg);
  H5FDdsmInt32   Receive(H5FDdsmMsg *msg);
  H5FDdsmInt32   Probe(H5FDdsmMsg *msg);

  H5FDdsmInt32   OpenPort();
  H5FDdsmInt32   ClosePort();
  H5FDdsmInt32   Accept();
  H5FDdsmInt32   Connect();
  H5FDdsmInt32   Disconnect();

protected:
  H5FDdsmSocket       *InterComm[H5FD_DSM_MAX_SOCKET]; // Internode Socket Collection for data exchange
  H5FDdsmSocket       *DsmMasterSocket; // Used for initializing connection and send comm orders
  H5FDdsmByte          DsmMasterHostName[MPI_MAX_PORT_NAME];
  H5FDdsmInt32         DsmMasterPort;

private:
  H5FDdsmInt32   InterCommServerConnect();
  H5FDdsmInt32   InterCommClientConnect();

  H5FDdsmInt32         InterCommSockets[H5FD_DSM_MAX_SOCKET];
};

#endif // __H5FDdsmCommSocket_h
