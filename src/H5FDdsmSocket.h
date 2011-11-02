/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmSocket.cxx

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

/*=========================================================================

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
// .NAME vtkSocket - BSD socket encapsulation.
// .SECTION Description
// This abstract class encapsulates a BSD socket. It provides an API for
// basic socket operations.

#ifndef __H5FDdsmSocket_h
#define __H5FDdsmSocket_h

#include "H5FDdsmObject.h"

class H5FDdsm_EXPORT H5FDdsmSocket : public H5FDdsmObject {

public:
  H5FDdsmSocket();
  ~H5FDdsmSocket();

  // ----- Status API ----
  // Description:
  // Check if the socket is alive.
  int GetConnected() { return (this->SocketDescriptor >=0); }

  H5FDdsmGetValueMacro(SocketDescriptor, int);
  H5FDdsmGetValueMacro(ClientSocketDescriptor, int);

  // Description:
  // Initialize/Clean Windows Socket Library
#ifdef _WIN32
  int WinSockInit();
  int WinSockCleanup();
  const char* WinSockPrintError(int error);
#endif

  // Description:
  // Close the socket.
  int Close();

  // Description:
  // Close the client socket if it exists.
  int CloseClient();

  // Description:
  // Binds socket to a particular port.
  // Returns 0 on success other -1 is returned.
  int Bind(int port, const char *hostName=NULL);

  // Description:
  // Accept a connection on a socket. Returns -1 on error. Otherwise
  // the descriptor of the accepted socket.
  int Accept();

  // Description:
  // Listen for connections on a socket. Returns 0 on success. -1 on error.
  int Listen();

  // Description:
  // Connect to a server socket. Returns 0 on success, -1 on error.
  int Connect(const char* hostname, int port);

  // Description:
  // Returns the port to which the socket is connected.
  // -1 on error.
  H5FDdsmGetValueMacro(Port, int);

  // Description:
  // Returns the hostname to which the socket is connected.
  // -1 on error.
  H5FDdsmGetValueMacro(HostName, const char*);

  // Description:
  // Returns local hostname found.
  const char* GetLocalHostName();

  // Description:
  // Selects set of sockets. Returns 0 on timeout, -1 on error.
  // 1 on success. Selected socket's index is returned through
  // selected_index
  static int SelectSockets(const int *sockets_to_select, int size,
    unsigned long msec, int *selected_index);

  // ------ Communication API --- // Should never be used
  // Description:
  // These methods send data over the socket.
  // Returns 0 on success, -1 on error.
  int Send(const void* data, int length);

  // Description:
  // Receive data from the socket.
  // This call blocks until some data is read from the socket.
  // When readFully is set, this call will block until all the
  // requested data is read from the socket.
  // -1 on error, else number of bytes read is returned.
  int Receive(void* data, int length, int readFully=1);

protected:
  int SocketDescriptor;
  int ClientSocketDescriptor;

  char *HostName;
  int   Port;

private:
  int IsServer;
};


#endif // __H5FDdsmSocket_h
