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
#include "H5FDdsmSocket.h"

#ifdef _WIN32
#include <windows.h>
#include <winsock.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <errno.h>
#endif

#include <cstdio>

#ifdef _WIN32
#define WSA_VERSION MAKEWORD(1,1)
#define H5FDdsmCloseSocketMacro(sock) (closesocket(sock))
#else
#define H5FDdsmCloseSocketMacro(sock) (close(sock))
#endif

#define QLEN 5

//-----------------------------------------------------------------------------
H5FDdsmSocket::H5FDdsmSocket()
{
  this->SocketDescriptor = -1;
  this->ClientSocketDescriptor = -1;
}

//-----------------------------------------------------------------------------
H5FDdsmSocket::~H5FDdsmSocket()
{
  if (this->ClientSocketDescriptor != -1) this->CloseClient();
  if (this->SocketDescriptor != -1) this->Close();
}

//-----------------------------------------------------------------------------
int
H5FDdsmSocket::WinSockInit()
{
#ifdef _WIN32
  WORD wVersionRequested;
  WSADATA wsaData;
  int err;

  // Use the MAKEWORD(lowbyte, highbyte) macro declared in Windef.h
  wVersionRequested = MAKEWORD(2, 2);
  err = WSAStartup(wVersionRequested, &wsaData);
  if (err != 0) {
    // Tell the user that we could not find a usable
    // Winsock DLL.
    printf("WSAStartup failed with error: %d\n", err);
    return -1;
  }

  // Confirm that the WinSock DLL supports 2.2.
  // Note that if the DLL supports versions greater
  // than 2.2 in addition to 2.2, it will still return
  // 2.2 in wVersion since that is the version we
  // requested.

  if (LOBYTE(wsaData.wVersion) != 2 || HIBYTE(wsaData.wVersion) != 2) {
    // Tell the user that we could not find a usable
    // WinSock DLL.
    printf("Could not find a usable version of Winsock.dll\n");
    WSACleanup();
    return -1;
  }
#endif
  return 0;
}

//-----------------------------------------------------------------------------
int
H5FDdsmSocket::WinSockCleanup()
{
  // Call WSACleanup when down using the Winsock dll
#ifdef _WIN32
  int err;
  err = WSACleanup();
  if (err != 0) {
    printf("WSACleanup failed with error: %d\n", err);
    return -1;
  }
#endif
  return 0;
}

//-----------------------------------------------------------------------------
int
H5FDdsmSocket::Create()
{
  if ((this->SocketDescriptor = (int) socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    return -1;
  }
  // Eliminate windows 0.2 second delay sending (buffering) data.
  int on = 1;
  if (setsockopt(this->SocketDescriptor, IPPROTO_TCP, TCP_NODELAY, (char*) &on,
      sizeof(on))) {
    return -1;
  }
  return 0;
}
//-----------------------------------------------------------------------------
int
H5FDdsmSocket::Close()
{
  if (this->SocketDescriptor < 0) return -1;
  if (H5FDdsmCloseSocketMacro(this->SocketDescriptor) < 0) return -1;
  this->SocketDescriptor = -1;
  return 0;
}

//-----------------------------------------------------------------------------
int
H5FDdsmSocket::CloseClient()
{
  if (this->ClientSocketDescriptor < 0) return -1;
  if (H5FDdsmCloseSocketMacro(this->ClientSocketDescriptor) < 0) return -1;
  this->ClientSocketDescriptor = -1;
  return 0;
}

//-----------------------------------------------------------------------------
int
H5FDdsmSocket::Bind(int port, const char *node)
{
  struct addrinfo hints;
  struct addrinfo *result, *rp;
  int s = 0, opt = 1;
  char service[64];

  sprintf(service, "%d", port);
  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE; /* For wildcard IP address */
  hints.ai_protocol = 0; /* Any protocol */
  hints.ai_canonname = NULL;
  hints.ai_addr = NULL;
  hints.ai_next = NULL;

  if (node != NULL && (strcmp("default", node) != 0)) {
    s = getaddrinfo(node, service, &hints, &result);
  } else {
    s = getaddrinfo(NULL, service, &hints, &result);
  }
  if (s != 0) {
    H5FDdsmError("getaddrinfo: " << gai_strerror(s));
    freeaddrinfo(result);
    return -1;
  }

  // Allow the socket to be bound to an address that is already in use
#ifdef _WIN32
  setsockopt(this->SocketDescriptor, SOL_SOCKET, SO_REUSEADDR, (char*) &opt, sizeof(int));
#else
  setsockopt(this->SocketDescriptor, SOL_SOCKET, SO_REUSEADDR, (void *) &opt,
      sizeof(int));
#endif

  for (rp = result; rp != NULL; rp = rp->ai_next) {
    if (bind(this->SocketDescriptor, rp->ai_addr, rp->ai_addrlen) == 0) break; /* Success */
  }

  if (rp == NULL) { /* No address succeeded */
    H5FDdsmError("Could not bind");
    freeaddrinfo(result);
    return -1;
  }
  freeaddrinfo(result);
  return 0;
}

//-----------------------------------------------------------------------------
int
H5FDdsmSocket::Select(unsigned long msec)
{
  int socketdescriptor = -1;

  if (this->ClientSocketDescriptor < 0) {
    socketdescriptor = this->SocketDescriptor;
  } else {
    socketdescriptor = this->ClientSocketDescriptor;
  }

  if (socketdescriptor < 0) {
    // invalid socket descriptor.
    return -1;
  }

  fd_set rset;
  struct timeval tval;
  struct timeval* tvalptr = 0;
  if (msec > 0) {
    tval.tv_sec = msec / 1000;
    tval.tv_usec = (msec % 1000) * 1000;
    tvalptr = &tval;
  }
  FD_ZERO(&rset);
  FD_SET(socketdescriptor, &rset);
  int res = select((int) socketdescriptor + 1, &rset, 0, 0, tvalptr);
  if (res == 0) {
    return 0;//for time limit expire
  }

  if (res < 0 || !(FD_ISSET(socketdescriptor, &rset))) {
    // Some error.
    return -1;
  }
  // The indicated socket has some activity on it.
  return 1;
}

//-----------------------------------------------------------------------------
int
H5FDdsmSocket::Accept()
{
  struct sockaddr_in client;
#ifdef _WIN32
  int client_len = sizeof(client);
#else
  socklen_t client_len = sizeof(client);
#endif

  if (this->SocketDescriptor < 0) {
    return -1;
  }
  this->ClientSocketDescriptor = (int) accept(this->SocketDescriptor,
      reinterpret_cast<sockaddr*> (&client), &client_len);
  if (this->ClientSocketDescriptor < 0) {
    return -1;
  }
  return 0;
}

//-----------------------------------------------------------------------------
int
H5FDdsmSocket::Listen()
{
  if (this->SocketDescriptor < 0) {
    return -1;
  }
  return listen(this->SocketDescriptor, QLEN);
}

//-----------------------------------------------------------------------------
int
H5FDdsmSocket::Connect(const char* node, int port)
{
  struct addrinfo hints;
  struct addrinfo *result, *rp;
  int s;
  char service[64];

  if (this->SocketDescriptor < 0) {
    return -1;
  }

  /* Obtain address(es) matching host/port */
  sprintf(service, "%d", port);
  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_INET;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = 0;
  hints.ai_protocol = 0; /* Any protocol */

  s = getaddrinfo(node, service, &hints, &result);
  if (s != 0) {
    H5FDdsmError("getaddrinfo: " << gai_strerror(s));
    freeaddrinfo(result);
    return -1;
  }

  for (rp = result; rp != NULL; rp = rp->ai_next) {
    if (connect(this->SocketDescriptor, rp->ai_addr, rp->ai_addrlen) != -1) break; /* Success */
  }

  if (rp == NULL) { /* No address succeeded */
    fprintf(stderr, "Could not connect\n");
    freeaddrinfo(result);
    return -1;
  }
  freeaddrinfo(result);
  return 0;
}

//-----------------------------------------------------------------------------
int
H5FDdsmSocket::GetPort()
{
  struct sockaddr_in sockinfo;
  memset(&sockinfo, 0, sizeof(sockinfo));
#ifdef _WIN32
  int sizebuf = sizeof(sockinfo);
#else
  socklen_t sizebuf = sizeof(sockinfo);
#endif
  if (getsockname(this->SocketDescriptor,
      reinterpret_cast<sockaddr*> (&sockinfo), &sizebuf) != 0) {
    return -1;
  }
  return ntohs(sockinfo.sin_port);
}

//-----------------------------------------------------------------------------
const char*
H5FDdsmSocket::GetHostName()
{
  struct sockaddr_in sockinfo;
  memset(&sockinfo, 0, sizeof(sockinfo));
#ifdef _WIN32
  int sizebuf = sizeof(sockinfo);
#else
  socklen_t sizebuf = sizeof(sockinfo);
#endif
  if (getsockname(this->SocketDescriptor,
      reinterpret_cast<sockaddr*> (&sockinfo), &sizebuf) != 0) {
    return NULL;
  }
  return (const char*) inet_ntoa(sockinfo.sin_addr);
}

//-----------------------------------------------------------------------------
const char*
H5FDdsmSocket::GetLocalHostName()
{
  static char localhost[256];
  gethostname(localhost, sizeof(localhost));
  return localhost;
}

//-----------------------------------------------------------------------------
int
H5FDdsmSocket::SelectSockets(const int *sockets_to_select, int size,
    unsigned long msec, int *selected_index)
{
  int i;
  int max_fd = -1;
  *selected_index = -1;
  if (size < 0) {
    return -1;
  }

  fd_set rset;
  struct timeval tval;
  struct timeval* tvalptr = 0;
  if (msec > 0) {
    tval.tv_sec = msec / 1000;
    tval.tv_usec = msec % 1000;
    tvalptr = &tval;
  }
  FD_ZERO(&rset);
  for (i = 0; i < size; i++) {
    FD_SET(sockets_to_select[i], &rset);
    max_fd = (sockets_to_select[i] > max_fd) ? sockets_to_select[i] : max_fd;
  }

  int res = select(max_fd + 1, &rset, 0, 0, tvalptr);
  if (res == 0) {
    return 0; //Timeout
  }
  if (res < 0) {
    // SelectSocket error.
    return -1;
  }

  //check which socket has some activity.
  for (i = 0; i < size; i++) {
    if (FD_ISSET(sockets_to_select[i], &rset)) {
      *selected_index = i;
      return 1;
    }
  }
  return -1;
}

//-----------------------------------------------------------------------------
int
H5FDdsmSocket::Send(const void* data, int length)
{
  if (!this->GetConnected()) {
    return -1;
  }
  if (length == 0) {
    // nothing to send.
    return 0;
  }
  const char* buffer = reinterpret_cast<const char*> (data);
  int total = 0;
  do {
    int flags, n;

#ifndef _WIN32
    // disabling, since not present on SUN.
    // flags = MSG_NOSIGNAL; //disable signal on Unix boxes.
#endif
    flags = 0;
    if (this->ClientSocketDescriptor < 0) {// Send from client to server
      n = send(this->SocketDescriptor, buffer + total, length - total, flags);
    } else {// Send from server to client
      n = send(this->ClientSocketDescriptor, buffer + total, length - total,
          flags);
    }
    if (n < 0) {
      H5FDdsmError("Socket Error: Send failed (size was " << length <<")");
      return -1;
    }
    total += n;
  } while (total < length);
  return 0;
}

//-----------------------------------------------------------------------------
int
H5FDdsmSocket::Receive(void* data, int length, int readFully/*=1*/)
{
  if (!this->GetConnected()) {
    return -1;
  }

  char* buffer = reinterpret_cast<char*> (data);
  int total = 0;
  do {
    int n;
#ifdef _WIN32
    int trys = 0;
#endif
    if (this->ClientSocketDescriptor < 0) {// Recv from Server to Client
      n = recv(this->SocketDescriptor, buffer + total, length - total, 0);
    } else {// Recv from Client to Server
      n = recv(this->ClientSocketDescriptor, buffer + total, length - total, 0);
    }
    if (n < 1) {
#ifdef _WIN32
      // On long messages, Windows recv sometimes fails with WSAENOBUFS, but
      // will work if you try again.
      int error = WSAGetLastError();
      if ((error == WSAENOBUFS) && (trys++ < 1000)) {
        Sleep(1);
        continue;
      }
#else
      // On unix, a recv may be interrupted by a signal.  In this case we should
      // retry.
      int errorNumber = errno;
      if (errorNumber == EINTR) continue;
#endif
      H5FDdsmError("Socket Error: Receive failed.");
      return -1;
    }
    total += n;
  } while (readFully && total < length);
  return total;
}
