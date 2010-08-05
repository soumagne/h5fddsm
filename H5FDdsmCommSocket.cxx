/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmCommSocket.cxx

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
#include "H5FDdsmCommSocket.h"
#include "H5FDdsmMsg.h"

#include <cstring>
#include <cstdlib>

// Sleep  in milliseconds
#ifdef _WIN32
  #include <windows.h> 
  #define sleep(a) ::Sleep(a)
#else
  void Sleep(int ms) {
    usleep(ms*1000); //convert to microseconds
    return;
  }
  #define sleep(a) Sleep(a)
#endif

#define DSM_COMM_SOCKET_PORT_INIT 22000
#define DSM_COMM_SOCKET_PORT_DATA 23000
//----------------------------------------------------------------------------
H5FDdsmCommSocket::H5FDdsmCommSocket()
{
  this->Comm = MPI_COMM_WORLD;
  this->CommType = H5FD_DSM_COMM_SOCKET;
  for (int i=0; i<H5FD_DSM_MAX_SOCKET; i++) this->InterComm[i] = NULL;
  this->DsmMasterSocket = NULL;
  this->CommChannel  = H5FD_DSM_COMM_CHANNEL_LOCAL;
  this->DsmMasterPort = 0;
}

//----------------------------------------------------------------------------
H5FDdsmCommSocket::~H5FDdsmCommSocket()
{
  for (int i=0; i<H5FD_DSM_MAX_SOCKET ;i++) {
    if (this->InterComm[i]) delete this->InterComm[i];
    this->InterComm[i] = NULL;
  }
  if (this->DsmMasterSocket) delete this->DsmMasterSocket;
  this->DsmMasterSocket = NULL;
}
//----------------------------------------------------------------------------
void
H5FDdsmCommSocket::SetDsmMasterHostName(const char *hostName) {
  strcpy(this->DsmMasterHostName, hostName);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::DupComm(MPI_Comm Source)
{
  MPI_Comm    NewComm;

  MPI_Comm_dup(Source, &NewComm);
  return(this->SetComm(NewComm));
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::Init()
{
  int size, rank;

  if (MPI_Comm_size(this->Comm, &size) != MPI_SUCCESS) return(H5FD_DSM_FAIL);
  if (MPI_Comm_rank(this->Comm, &rank) != MPI_SUCCESS) return(H5FD_DSM_FAIL);

  this->SetId(rank);
  this->SetTotalSize(size);

  if (this->Id == 0) {
    this->DsmMasterSocket = new H5FDdsmSocket();
    if (this->DsmMasterSocket->Create() == H5FD_DSM_FAIL) {
      H5FDdsmError("Create DsmMasterSocket failed");
      return(H5FD_DSM_FAIL);
    }
  }
  this->Barrier();

  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::Check(H5FDdsmMsg *Msg)
{
  int         nid, flag=0;
  MPI_Status  Status;

  if (H5FDdsmComm::Check(Msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  if (this->CommChannel == H5FD_DSM_COMM_CHANNEL_LOCAL) {
    MPI_Iprobe(MPI_ANY_SOURCE, Msg->Tag, this->Comm, &flag, &Status);
    if (flag) {
      nid = Status.MPI_SOURCE;
      Msg->SetSource(nid);
      return(H5FD_DSM_SUCCESS);
    }
  }
  return(H5FD_DSM_FAIL);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::Receive(H5FDdsmMsg *Msg)
{
  int            MessageLength;
  H5FDdsmInt32   status;
  H5FDdsmInt32   source = MPI_ANY_SOURCE;
  MPI_Status  SendRecvStatus;

  if (H5FDdsmComm::Receive(Msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  if (Msg->Source >= 0) source = Msg->Source;

  if (this->CommChannel == H5FD_DSM_COMM_CHANNEL_REMOTE) {
    H5FDdsmDebug("(" << this->Id << ") Receiving from remote DSM " << Msg->Length << " bytes from " << source << " Tag = " << H5FDdsmTagToString(Msg->Tag));
    if (source >= 0) {
      this->InterComm[source]->Receive(Msg->Data, Msg->Length);
    } else {
      // TODO when modifying then dynamically the socket array, should be careful not to change it
      // while doing a select on it
      int selectedIndex;
      int *socketsToSelect = new int[this->InterSize];
      for (int i=0; i<this->InterSize; i++) {
        socketsToSelect[i] = this->InterComm[i]->GetClientSocketDescriptor();
      }
      // if ANY_SOURCE use select on the whole list of sockets descriptors
      this->InterComm[0]->SelectSockets(socketsToSelect, this->InterSize, 0, &selectedIndex);
      this->InterComm[selectedIndex]->Receive(Msg->Data, Msg->Length);
      delete []socketsToSelect;
    }
  }
  else {
    H5FDdsmDebug("(" << this->Id << ") Receiving " << Msg->Length << " bytes from " << source << " Tag = " << H5FDdsmTagToString(Msg->Tag));
    status = MPI_Recv(Msg->Data, Msg->Length, MPI_UNSIGNED_CHAR, source, Msg->Tag, this->Comm, &SendRecvStatus);

    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Recv failed to receive " << Msg->Length << " Bytes from " << Msg->Source);
      H5FDdsmError("MPI Error Code = " << SendRecvStatus.MPI_ERROR);
      return(H5FD_DSM_FAIL);
    }
    status = MPI_Get_count(&SendRecvStatus, MPI_UNSIGNED_CHAR, &MessageLength);
    H5FDdsmDebug("(" << this->Id << ") Received " << MessageLength << " bytes from " << SendRecvStatus.MPI_SOURCE);

    Msg->SetSource(SendRecvStatus.MPI_SOURCE);
    Msg->SetLength(MessageLength);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("MPI_Get_count failed ");
      return(H5FD_DSM_FAIL);
    }
  }
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::Send(H5FDdsmMsg *Msg)
{
  H5FDdsmInt32   status;

  if (H5FDdsmComm::Send(Msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  H5FDdsmFloat64 startTime = MPI_Wtime();

  if (this->CommChannel == H5FD_DSM_COMM_CHANNEL_REMOTE) {
    H5FDdsmDebug("(" << this->Id << ") Sending to remote DSM " << Msg->Length << " bytes to " << Msg->Dest << " Tag = " << H5FDdsmTagToString(Msg->Tag));
    this->InterComm[Msg->Dest]->Send(Msg->Data, Msg->Length);
  }
  else {
    H5FDdsmDebug("(" << this->Id << ") Sending " << Msg->Length << " bytes to " << Msg->Dest << " Tag = " << H5FDdsmTagToString(Msg->Tag));
    status = MPI_Send(Msg->Data, Msg->Length, MPI_UNSIGNED_CHAR, Msg->Dest, Msg->Tag, this->Comm);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Send failed to send " << Msg->Length << " Bytes to " << Msg->Dest);
      return(H5FD_DSM_FAIL);
    }
  }

  H5FDdsmFloat64 endTime = MPI_Wtime();
  if ((Msg->Tag == H5FD_DSM_PUT_DATA_TAG) && (Msg->Length > 1024.f)) {
    //
    // This value is only valid when transferring big chunks of data when local
    // buffers are saturated. It does not measure the end-to-end tranfer rate.
    this->TransferRate = Msg->Length/((endTime - startTime)*1024*1024);
    //cout << "(" << this->Id << ") Sent " << Msg->Length << " bytes to "
    //     << Msg->Dest << " at " << this->TransferRate << " MB/s" << endl;
    //
  }
  H5FDdsmDebug("(" << this->Id << ") Sent " << Msg->Length << " bytes to " << Msg->Dest);

  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::Barrier()
{
  if (H5FDdsmComm::Barrier() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  MPI_Barrier(this->Comm);

  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::OpenPort()
{
  if (H5FDdsmComm::OpenPort() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  if (this->Id == 0) {
    int port = DSM_COMM_SOCKET_PORT_INIT;

    if (this->DsmMasterPort) port = this->DsmMasterPort;
    // Take this port as default for now and let Bind decide about hostname
    if (this->DsmMasterSocket->Bind(port, this->DsmMasterHostName) == H5FD_DSM_FAIL) {
      H5FDdsmError("Bind DsmMasterSocket failed");
      return(H5FD_DSM_FAIL);
    }
    this->DsmMasterPort = this->DsmMasterSocket->GetPort();
    strcpy(this->DsmMasterHostName, this->DsmMasterSocket->GetHostName());
    this->DsmMasterSocket->Listen();
  }
  this->Barrier();

  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::ClosePort()
{
  if (H5FDdsmComm::ClosePort() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  if (this->Id == 0) {
    // close MasterSocket
    this->DsmMasterSocket->Close();
    if (this->DsmMasterSocket->Create() == H5FD_DSM_FAIL) {
      H5FDdsmError("Create DsmMasterSocket failed");
      return(H5FD_DSM_FAIL);
    }
  }
  this->Barrier();

  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::RemoteCommAccept()
{
  if (H5FDdsmComm::RemoteCommAccept() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  // Needed if we want to insert a timeout
  // if (this->MasterSocket->Select(100) <= 0 ) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    if (this->DsmMasterSocket->Accept() == H5FD_DSM_FAIL) {
      H5FDdsmError("Accept socket failed");
      return(H5FD_DSM_FAIL);
    }
    this->DsmMasterSocket->Receive(&this->InterSize, sizeof(H5FDdsmInt32));
    this->DsmMasterSocket->Send(&this->TotalSize, sizeof(H5FDdsmInt32));
  }
  if (MPI_Bcast(&this->InterSize, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, this->Comm) != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of InterSize failed");
    return(H5FD_DSM_FAIL);
  }

  if (this->InterCommServerConnect() != H5FD_DSM_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " Error in InterCommServerConnect");
    return(H5FD_DSM_FAIL);
  }

  this->CommChannel = H5FD_DSM_COMM_CHANNEL_REMOTE;

  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::RemoteCommConnect()
{
  if (H5FDdsmComm::RemoteCommConnect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    int retry = 100;
    while (retry>0) {
      if (this->DsmMasterSocket->Connect(this->DsmMasterHostName, this->DsmMasterPort) == H5FD_DSM_FAIL) {
        retry -= 1;
        H5FDdsmError("Socket connection failed : " << "Retrying " << retry);
        sleep(1000);
      }
      else retry = -1;
    }
    if (retry!=-1) {
      H5FDdsmError("Socket connection failed");
      return(H5FD_DSM_FAIL);
    }
    this->DsmMasterSocket->Send(&this->TotalSize, sizeof(H5FDdsmInt32));
    this->DsmMasterSocket->Receive(&this->InterSize, sizeof(H5FDdsmInt32));
  }
  if (MPI_Bcast(&this->InterSize, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, this->Comm) != MPI_SUCCESS){
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of InterSize failed");
    return(H5FD_DSM_FAIL);
  }

  if (this->InterCommClientConnect() != H5FD_DSM_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " Error in InterCommClientConnect");
    return(H5FD_DSM_FAIL);
  }

  this->CommChannel = H5FD_DSM_COMM_CHANNEL_REMOTE;
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::RemoteCommDisconnect()
{
  if (H5FDdsmComm::RemoteCommDisconnect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  this->Barrier();
  for (int i=0; i<H5FD_DSM_MAX_SOCKET; i++) {
    if (this->InterComm[i]) delete this->InterComm[i];
    this->InterComm[i] = NULL;
  }
  this->CommChannel = H5FD_DSM_COMM_CHANNEL_LOCAL;
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::RemoteCommRecvReady()
{
  if (H5FDdsmComm::RemoteCommRecvReady() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  H5FDdsmByte ready[6];

  if (this->Id == 0) {
    this->InterComm[0]->Receive(ready, sizeof(ready));
    H5FDdsmDebug("Recv ready: " << ready);
  }
  this->Barrier();

  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::RemoteCommSendReady()
{
  if (H5FDdsmComm::RemoteCommSendReady() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  H5FDdsmByte ready[6] = "ready";

  if (this->Id == 0) {
    H5FDdsmDebug("Send ready: " << ready);
    this->InterComm[0]->Send(ready, sizeof(ready));
  }
  this->Barrier();

  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::RemoteCommRecvInfo(H5FDdsmInt64 *length, H5FDdsmInt64 *totalLength,
                                      H5FDdsmInt32 *startServerId, H5FDdsmInt32 *endServerId)
{
  if (H5FDdsmComm::RemoteCommRecvInfo(length, totalLength, startServerId, endServerId) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    this->InterComm[0]->Receive(length, sizeof(H5FDdsmInt64));
    H5FDdsmDebug("Recv DSM length: " << *length);
  }
  if (MPI_Bcast(length, sizeof(H5FDdsmInt64), MPI_UNSIGNED_CHAR, 0, this->Comm) != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of length failed");
    return(H5FD_DSM_FAIL);
  }

  if (this->Id == 0) {
    this->InterComm[0]->Receive(totalLength, sizeof(H5FDdsmInt64));
    H5FDdsmDebug("Recv DSM totalLength: " << *totalLength);
  }
  if (MPI_Bcast(totalLength, sizeof(H5FDdsmInt64), MPI_UNSIGNED_CHAR, 0, this->Comm) != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of totalLength failed");
    return(H5FD_DSM_FAIL);
  }

  if (this->Id == 0) {
    this->InterComm[0]->Receive(startServerId, sizeof(H5FDdsmInt32));
    H5FDdsmDebug("Recv DSM startServerId: " << *startServerId);
  }
  if (MPI_Bcast(startServerId, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, this->Comm) != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of startServerId failed");
    return(H5FD_DSM_FAIL);
  }

  if (this->Id == 0) {
    this->InterComm[0]->Receive(endServerId, sizeof(H5FDdsmInt32));
    H5FDdsmDebug("Recv DSM endServerId: " << *endServerId);
  }
  if (MPI_Bcast(endServerId, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, this->Comm) != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of endServerId failed");
    return(H5FD_DSM_FAIL);
  }
  H5FDdsmDebug("Recv DSM Info Completed");

  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::RemoteCommSendInfo(H5FDdsmInt64 *length, H5FDdsmInt64 *totalLength,
                                      H5FDdsmInt32 *startServerId, H5FDdsmInt32 *endServerId)
{
  if (H5FDdsmComm::RemoteCommSendInfo(length, totalLength, startServerId, endServerId) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    // Length
    H5FDdsmDebug("Send DSM length: " << *length);
    this->InterComm[0]->Send(length, sizeof(H5FDdsmInt64));

    // TotalLength
    H5FDdsmDebug("Send DSM totalLength: " << *totalLength);
    this->InterComm[0]->Send(totalLength, sizeof(H5FDdsmInt64));

    // StartServerId
    H5FDdsmDebug("Send DSM startServerId: " << *startServerId);
    this->InterComm[0]->Send(startServerId, sizeof(H5FDdsmInt32));

    // EndServerId
    H5FDdsmDebug("Send DSM endServerId: " << *endServerId);
    this->InterComm[0]->Send(endServerId, sizeof(H5FDdsmInt32));
    H5FDdsmDebug("Send DSM Info Completed");
  }
  this->Barrier();
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::RemoteCommSendXML(H5FDdsmString file, H5FDdsmInt32 dest)
{
  if (H5FDdsmComm::RemoteCommSendXML(file, dest) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (this->Id == 0) {
    H5FDdsmInt32 length = strlen(file) + 1;
    H5FDdsmDebug("Send XML to DSM Buffer object: Length=" << length << "\n" << file);

    if (this->CommChannel == H5FD_DSM_COMM_CHANNEL_REMOTE) {
      this->InterComm[dest]->Send(&length, sizeof(H5FDdsmInt32));
      this->InterComm[dest]->Send(file, sizeof(H5FDdsmChar)*length);
    }
    else {
      if (MPI_Send(file, length, MPI_CHAR, dest, H5FD_DSM_XML_TAG, this->Comm) != MPI_SUCCESS){
        H5FDdsmError("Id = " << this->Id << " MPI_Send of XML file failed");
        return(H5FD_DSM_FAIL);
      }
    }
  }
  H5FDdsmDebug("Send DSM XML Completed");
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::RemoteCommRecvXML(H5FDdsmString *file)
{
  H5FDdsmInt32 length; // string is null terminated on send
  //
  if (H5FDdsmComm::RemoteCommRecvXML(file) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (this->CommChannel == H5FD_DSM_COMM_CHANNEL_REMOTE) {
    this->InterComm[0]->Receive(&length, sizeof(H5FDdsmInt32));
    *file = new char[length];
    this->InterComm[0]->Receive(*file, sizeof(H5FDdsmChar)*length);
  }
  else {
    MPI_Status status;
    MPI_Probe(0, H5FD_DSM_XML_TAG, this->Comm, &status);
    MPI_Get_count(&status, MPI_CHAR, &length);
    *file = new char[length];
    if (MPI_Recv(*file, length, MPI_CHAR, 0, H5FD_DSM_XML_TAG, this->Comm, &status) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of XML file failed");
      return(H5FD_DSM_FAIL);
    }
  }
  H5FDdsmDebug("Recv DSM XML: " << *file);
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::RemoteCommSendSteeringCmd(H5FDdsmString cmd)
{
  if (H5FDdsmComm::RemoteCommSendSteeringCmd(cmd) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (this->Id == 0) {
    H5FDdsmInt32 length = strlen(cmd) + 1;
    this->InterComm[0]->Send(&length, sizeof(H5FDdsmInt32));
    this->InterComm[0]->Send(cmd, sizeof(H5FDdsmChar)*length);
  }
  this->Barrier();
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::RemoteCommRecvSteeringCmd(H5FDdsmString *cmd)
{
  H5FDdsmInt32 length; // string is null terminated on send
  //
  if (H5FDdsmComm::RemoteCommRecvSteeringCmd(cmd) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (this->Id == 0) {
    this->InterComm[0]->Receive(&length, sizeof(H5FDdsmInt32));
    *cmd = new char[length];
    this->InterComm[0]->Receive(*cmd, sizeof(H5FDdsmChar)*length);
  }
  if (MPI_Bcast(&length, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, this->Comm) != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of length failed");
    return(H5FD_DSM_FAIL);
  }
  if (this->Id != 0) *cmd = new char[length];
  if (MPI_Bcast(*cmd, sizeof(H5FDdsmChar)*length, MPI_UNSIGNED_CHAR, 0, this->Comm) != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Bcast of steering cmd failed");
      return(H5FD_DSM_FAIL);
    }
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::HasStillData()
{
  H5FDdsmInt32 ret = H5FD_DSM_TRUE;
  static H5FDdsmInt32 chanCleared = 0;

  chanCleared++;
  if (chanCleared == this->InterSize) {
    H5FDdsmDebug("Channels cleared: " << chanCleared << "/" << this->InterSize);
    chanCleared = 0;
    ret = H5FD_DSM_FALSE;
  }

  return(ret);
}
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::InterCommServerConnect()
{
  H5FDdsmString  interCommHostName       = NULL;
  H5FDdsmString  masterInterCommHostName = NULL;
  H5FDdsmInt32  *interCommPort           = NULL;
  H5FDdsmInt32  *masterInterCommPort     = NULL;

  interCommHostName = new char[this->InterSize*MPI_MAX_PORT_NAME];
  masterInterCommHostName = new char[(this->InterSize)*(this->TotalSize)*MPI_MAX_PORT_NAME];
  interCommPort = (H5FDdsmInt32*) malloc(sizeof(H5FDdsmInt32)*this->InterSize);
  masterInterCommPort = (H5FDdsmInt32*) malloc(sizeof(H5FDdsmInt32)*(this->InterSize)*(this->TotalSize));

  H5FDdsmDebug("(" << this->Id << ") Creating " << this->InterSize << " sockets for intercomm");
  int sock_offset = (this->Id*this->InterSize);
  for (int sock=0; sock<(this->InterSize); sock++) {
    int bindPort = DSM_COMM_SOCKET_PORT_DATA + sock_offset + sock;
    this->InterComm[sock] = new H5FDdsmSocket();
    this->InterComm[sock]->Create();
    while (1) {
      if (this->InterComm[sock]->Bind(bindPort, this->DsmMasterHostName) < 0) {
        bindPort++;
      } else {
        break;
      }
    }
    strcpy(&interCommHostName[sock*MPI_MAX_PORT_NAME], this->InterComm[sock]->GetHostName());
    interCommPort[sock] = this->InterComm[sock]->GetPort();
    this->InterComm[sock]->Listen();
  }

  // Gather socket info
  MPI_Gather(interCommHostName, this->InterSize*MPI_MAX_PORT_NAME, MPI_CHAR,
    masterInterCommHostName, this->InterSize*MPI_MAX_PORT_NAME, MPI_CHAR, 0, this->Comm);
  MPI_Gather(interCommPort, this->InterSize*sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR,
    masterInterCommPort, this->InterSize*sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, this->Comm);

  // Send it then to interComm 0
  if (this->Id == 0) {
    for (int i=0; i<((this->InterSize)*(this->TotalSize)); i++) {
      char tmpHost[MPI_MAX_PORT_NAME];
      H5FDdsmInt32 tmpPort = masterInterCommPort[i];
      strncpy(tmpHost, masterInterCommHostName+i*MPI_MAX_PORT_NAME, MPI_MAX_PORT_NAME);
      H5FDdsmDebug("Send info for intercomm socket on " << i << " to " << tmpHost << ":" << tmpPort);
    }
    // Send masterInterCommHostName
    this->DsmMasterSocket->Send(masterInterCommHostName,
      (this->InterSize)*(this->TotalSize)*MPI_MAX_PORT_NAME*sizeof(char));
    // Send masterInterCommPort
    this->DsmMasterSocket->Send(masterInterCommPort,
      (this->InterSize)*(this->TotalSize)*sizeof(H5FDdsmInt32));
  }
  //
  // Accept
  for (int sock=0; sock<(this->InterSize); sock++) {
    this->InterComm[sock]->Accept();
  }
  this->Barrier();
  //
  delete[] interCommHostName;
  delete[] masterInterCommHostName;
  free(interCommPort);
  free(masterInterCommPort);

  H5FDdsmDebug("Cleaned well interCommHostName/masterInterCommHostName");
  return H5FD_DSM_SUCCESS;
}
//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::InterCommClientConnect()
{
  H5FDdsmString  interCommHostName       = NULL;
  H5FDdsmString  masterInterCommHostName = NULL;
  H5FDdsmInt32  *interCommPort           = NULL;
  H5FDdsmInt32  *masterInterCommPort     = NULL;

  interCommHostName = new char[this->InterSize*MPI_MAX_PORT_NAME];
  masterInterCommHostName = new char[(this->InterSize)*(this->TotalSize)*MPI_MAX_PORT_NAME];
  interCommPort = (H5FDdsmInt32*) malloc(sizeof(H5FDdsmInt32)*this->InterSize);
  masterInterCommPort = (H5FDdsmInt32*) malloc(sizeof(H5FDdsmInt32)*(this->InterSize)*(this->TotalSize));

  if (this->Id == 0) {
    // Receive masterInterCommHostName
    this->DsmMasterSocket->Receive(masterInterCommHostName,
      (this->InterSize)*(this->TotalSize)*MPI_MAX_PORT_NAME*sizeof(char));
    // Receive masterInterCommPort
    this->DsmMasterSocket->Receive(masterInterCommPort,
      (this->InterSize)*(this->TotalSize)*sizeof(H5FDdsmInt32));

    for (int i=0; i<((this->InterSize)*(this->TotalSize)); i++) {
      if (i%(this->TotalSize) == 0) {
        strncpy(interCommHostName+(i/this->TotalSize)*MPI_MAX_PORT_NAME, 
          masterInterCommHostName+i*MPI_MAX_PORT_NAME, MPI_MAX_PORT_NAME);
        interCommPort[i/this->TotalSize] = masterInterCommPort[i];
      } else {
        char tmpHost[MPI_MAX_PORT_NAME];
        H5FDdsmInt32 tmpPort = masterInterCommPort[i];
        strncpy(tmpHost, masterInterCommHostName+i*MPI_MAX_PORT_NAME, MPI_MAX_PORT_NAME);
        H5FDdsmDebug("Receive info for intercomm socket on " << i << " to " << tmpHost << ":" << tmpPort);
        MPI_Send(tmpHost, MPI_MAX_PORT_NAME, MPI_CHAR, i%(this->TotalSize), 0, this->Comm);
        MPI_Send(&tmpPort, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, i%(this->TotalSize), 0, this->Comm);
      }
    }
  } else {
    for (int i=0; i<this->InterSize; i++) {
      MPI_Recv((interCommHostName+i*MPI_MAX_PORT_NAME), MPI_MAX_PORT_NAME, MPI_CHAR, 0, 0, this->Comm, MPI_STATUS_IGNORE);
      MPI_Recv((interCommPort+i), sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, 0, this->Comm, MPI_STATUS_IGNORE);
    }
  }
  this->Barrier();

  H5FDdsmDebug("(" << this->Id << ") Creating " << this->InterSize << " sockets for intercomm");
  for (int sock=0; sock<(this->InterSize); sock++) {
    char tmpHost[MPI_MAX_PORT_NAME];
    H5FDdsmInt32 tmpPort = interCommPort[sock];
    strncpy(tmpHost, interCommHostName+sock*MPI_MAX_PORT_NAME, MPI_MAX_PORT_NAME);
    this->InterComm[sock] = new H5FDdsmSocket();
    this->InterComm[sock]->Create();
    // Connect
    H5FDdsmDebug("Connecting intercomm socket " << sock << " on " << this->Id << " to " << tmpHost << ":" << tmpPort);
    this->InterComm[sock]->Connect(tmpHost, tmpPort);
  }
  this->Barrier();
  //
  delete[] interCommHostName;
  delete[] masterInterCommHostName;
  free(interCommPort);
  free(masterInterCommPort);

  H5FDdsmDebug("Cleaned well interCommHostName/masterInterCommHostName");

  return H5FD_DSM_SUCCESS;
}
//----------------------------------------------------------------------------
