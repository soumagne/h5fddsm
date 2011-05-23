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
#include "H5FDdsmSocket.h"
#include "H5FDdsmMsg.h"

#include <cstring>
#include <cstdlib>

#define DSM_COMM_SOCKET_PORT_INIT 22000
#define DSM_COMM_SOCKET_PORT_DATA 23000

//----------------------------------------------------------------------------
H5FDdsmCommSocket::H5FDdsmCommSocket()
{
  for (int i=0; i<H5FD_DSM_MAX_SOCKET; i++) this->InterComm[i] = NULL;
  this->DsmMasterSocket = NULL;
  this->CommChannel  = H5FD_DSM_INTRA_COMM;
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
H5FDdsmCommSocket::SetDsmMasterHostName(const char *hostName)
{
  strcpy(this->DsmMasterHostName, hostName);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::Init()
{
  if (H5FDdsmComm::Init() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    this->DsmMasterSocket = new H5FDdsmSocket();
  }

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::Send(H5FDdsmMsg *Msg)
{
  H5FDdsmInt32   status;

  if (H5FDdsmComm::Send(Msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->CommChannel == H5FD_DSM_INTER_COMM) {
    H5FDdsmDebug("(" << this->Id << ") Sending to remote DSM " << Msg->Length << " bytes to " << Msg->Dest << " Tag = " << H5FDdsmTagToString(Msg->Tag));
    this->InterComm[Msg->Dest]->Send(Msg->Data, Msg->Length);
  }
  else {
    H5FDdsmDebug("(" << this->Id << ") Sending " << Msg->Length << " bytes to " << Msg->Dest << " Tag = " << H5FDdsmTagToString(Msg->Tag));
    status = MPI_Send(Msg->Data, Msg->Length, MPI_UNSIGNED_CHAR, Msg->Dest, Msg->Tag, this->IntraComm);
    if (status != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Send failed to send " << Msg->Length << " Bytes to " << Msg->Dest);
      return(H5FD_DSM_FAIL);
    }
  }

  H5FDdsmDebug("(" << this->Id << ") Sent " << Msg->Length << " bytes to " << Msg->Dest);

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::Receive(H5FDdsmMsg *Msg, H5FDdsmInt32 Channel)
{
  int            MessageLength;
  H5FDdsmInt32   status;
  H5FDdsmInt32   source = MPI_ANY_SOURCE;
  H5FDdsmInt32   receiveChannel = Channel;
  MPI_Status  SendRecvStatus;

  if (H5FDdsmComm::Receive(Msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  if (Msg->Source >= 0) source = Msg->Source;

  if (!receiveChannel) receiveChannel = this->CommChannel;

  if (receiveChannel == H5FD_DSM_INTER_COMM) {
    H5FDdsmDebug("(" << this->Id << ") Receiving from remote DSM " << Msg->Length << " bytes from " << source << " Tag = " << H5FDdsmTagToString(Msg->Tag));
    if (source >= 0) {
      this->InterComm[source]->Receive(Msg->Data, Msg->Length);
    } else {
      // TODO when modifying then dynamically the socket array, should be careful not to change it
      // while doing a select on it
      int selectedIndex;
      // if ANY_SOURCE use select on the whole list of sockets descriptors
      this->InterComm[0]->SelectSockets(this->InterCommSockets, this->InterSize, 0, &selectedIndex);
      this->InterComm[selectedIndex]->Receive(Msg->Data, Msg->Length);
    }
  }
  else {
    H5FDdsmDebug("(" << this->Id << ") Receiving " << Msg->Length << " bytes from " << source << " Tag = " << H5FDdsmTagToString(Msg->Tag));
    status = MPI_Recv(Msg->Data, Msg->Length, MPI_UNSIGNED_CHAR, source, Msg->Tag, this->IntraComm, &SendRecvStatus);

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
H5FDdsmCommSocket::Probe(H5FDdsmMsg *Msg)
{
  int         nid, flag=0;
  MPI_Status  Status;

  if (H5FDdsmComm::Probe(Msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  if (this->CommChannel == H5FD_DSM_INTRA_COMM) {
    MPI_Iprobe(MPI_ANY_SOURCE, Msg->Tag, this->IntraComm, &flag, &Status);
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
  }

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::Accept(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize)
{
  if (H5FDdsmComm::Accept(storagePointer, storageSize) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  // TODO Needed if we want to insert a timeout
  // if (this->MasterSocket->Select(100) <= 0 ) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    if (this->DsmMasterSocket->Accept() == H5FD_DSM_FAIL) {
      H5FDdsmError("Accept socket failed");
      return(H5FD_DSM_FAIL);
    }
    this->DsmMasterSocket->Receive(&this->InterSize, sizeof(H5FDdsmInt32));
    this->DsmMasterSocket->Send(&this->IntraSize, sizeof(H5FDdsmInt32));
  }
  if (MPI_Bcast(&this->InterSize, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, this->IntraComm) != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of InterSize failed");
    return(H5FD_DSM_FAIL);
  }

  if (this->InterCommServerConnect() != H5FD_DSM_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " Error in InterCommServerConnect");
    return(H5FD_DSM_FAIL);
  }

  for (int i=0; i<this->InterSize; i++) {
    this->InterCommSockets[i] = this->InterComm[i]->GetClientSocketDescriptor();
  }

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::Connect()
{
  H5FDdsmInt32 isMasterConnected = H5FD_DSM_FAIL;

  if (H5FDdsmComm::Connect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    if (this->DsmMasterSocket->Connect(this->DsmMasterHostName, this->DsmMasterPort) == 0) {
      isMasterConnected = H5FD_DSM_SUCCESS;
    }
    if (isMasterConnected == H5FD_DSM_FAIL) {
      H5FDdsmDebug("Socket connection failed");
    } else {
      this->DsmMasterSocket->Send(&this->IntraSize, sizeof(H5FDdsmInt32));
      this->DsmMasterSocket->Receive(&this->InterSize, sizeof(H5FDdsmInt32));
    }
  }
  if (MPI_Bcast(&isMasterConnected, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, this->IntraComm) != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of isMasterConnected failed");
    return(H5FD_DSM_FAIL);;
  }

  if (isMasterConnected == H5FD_DSM_SUCCESS) {
    if (MPI_Bcast(&this->InterSize, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, this->IntraComm) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Bcast of InterSize failed");
      return(H5FD_DSM_FAIL);
    }

    if (this->InterCommClientConnect() != H5FD_DSM_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " Error in InterCommClientConnect");
      return(H5FD_DSM_FAIL);
    }

    this->CommChannel = H5FD_DSM_INTER_COMM;
    return(H5FD_DSM_SUCCESS);
  } else {
    return(H5FD_DSM_FAIL);
  }
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::Disconnect()
{
  if (H5FDdsmComm::Disconnect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  this->Barrier();
  if (this->InterComm[0]->GetClientSocketDescriptor() < 0) {
    H5FDdsmDebug("Client is now disconnecting");
  } else {
    H5FDdsmDebug("Server is now disconnecting");
  }
  for (int i=0; i<H5FD_DSM_MAX_SOCKET; i++) {
    if (this->InterComm[i]) delete this->InterComm[i];
    this->InterComm[i] = NULL;
  }
  this->CommChannel = H5FD_DSM_INTRA_COMM;
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::RecvReady()
{
  H5FDdsmByte ready[6];

  if (H5FDdsmComm::RecvReady() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    this->InterComm[0]->Receive(ready, sizeof(ready));
    H5FDdsmDebug("Recv ready: " << ready);
  }
  this->Barrier();

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::SendReady()
{
  H5FDdsmByte ready[6] = "ready";

  if (H5FDdsmComm::SendReady() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    H5FDdsmDebug("Send ready: " << ready);
    this->InterComm[0]->Send(ready, sizeof(ready));
  }
  this->Barrier();

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::RecvInfo(H5FDdsmInfo *dsmInfo)
{
  if (H5FDdsmComm::RecvInfo(dsmInfo) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    H5FDdsmDebug("Receiving DSM Info...");
    this->InterComm[0]->Receive(dsmInfo, sizeof(H5FDdsmInfo));
    H5FDdsmDebug("Recv DSM Info Completed");
  }
  if (MPI_Bcast(dsmInfo, sizeof(H5FDdsmInfo), MPI_UNSIGNED_CHAR, 0, this->IntraComm) != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of Info failed");
    return(H5FD_DSM_FAIL);
  }

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::SendInfo(H5FDdsmInfo *dsmInfo)
{
  if (H5FDdsmComm::SendInfo(dsmInfo) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    H5FDdsmDebug("Sending DSM Info...");
    this->InterComm[0]->Send(dsmInfo, sizeof(H5FDdsmInfo));
    H5FDdsmDebug("Send DSM Info Completed");
  }
  this->Barrier();
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::SendXML(H5FDdsmString file, H5FDdsmInt32 dest)
{
  if (H5FDdsmComm::SendXML(file, dest) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (this->Id == 0) {
    H5FDdsmInt32 length = (H5FDdsmInt32)strlen(file) + 1;
    H5FDdsmDebug("Send XML to DSM Buffer object: Length=" << length << "\n" << file);

    if (this->CommChannel == H5FD_DSM_INTER_COMM) {
      this->InterComm[dest]->Send(&length, sizeof(H5FDdsmInt32));
      this->InterComm[dest]->Send(file, sizeof(H5FDdsmByte)*length);
    }
    else {
      if (MPI_Send(file, length, MPI_CHAR, dest, H5FD_DSM_XML_TAG, this->IntraComm) != MPI_SUCCESS){
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
H5FDdsmCommSocket::RecvXML(H5FDdsmString *file)
{
  H5FDdsmInt32 length; // string is null terminated on send
  //
  if (H5FDdsmComm::RecvXML(file) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (this->CommChannel == H5FD_DSM_INTER_COMM) {
    this->InterComm[0]->Receive(&length, sizeof(H5FDdsmInt32));
    *file = new char[length];
    this->InterComm[0]->Receive(*file, sizeof(H5FDdsmByte)*length);
  }
  else {
    MPI_Status status;
    MPI_Probe(0, H5FD_DSM_XML_TAG, this->IntraComm, &status);
    MPI_Get_count(&status, MPI_CHAR, &length);
    *file = new char[length];
    if (MPI_Recv(*file, length, MPI_CHAR, 0, H5FD_DSM_XML_TAG, this->IntraComm, &status) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of XML file failed");
      return(H5FD_DSM_FAIL);
    }
  }
  H5FDdsmDebug("Recv DSM XML: " << *file);
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommSocket::InterCommServerConnect()
{
  H5FDdsmString  interCommHostName       = NULL;
  H5FDdsmString  masterInterCommHostName = NULL;
  H5FDdsmInt32  *interCommPort           = NULL;
  H5FDdsmInt32  *masterInterCommPort     = NULL;

  interCommHostName = (H5FDdsmString) calloc(this->InterSize*MPI_MAX_PORT_NAME, sizeof(H5FDdsmByte));
  masterInterCommHostName = (H5FDdsmString) calloc((this->InterSize)*(this->IntraSize)*MPI_MAX_PORT_NAME, sizeof(H5FDdsmByte));
  interCommPort = (H5FDdsmInt32*) malloc(sizeof(H5FDdsmInt32)*this->InterSize);
  masterInterCommPort = (H5FDdsmInt32*) malloc(sizeof(H5FDdsmInt32)*(this->InterSize)*(this->IntraSize));

  H5FDdsmDebug("(" << this->Id << ") Creating " << this->InterSize << " sockets for intercomm");
  int sock_offset = (this->Id*this->InterSize);
  for (int sock=0; sock<(this->InterSize); sock++) {
    int bindPort = DSM_COMM_SOCKET_PORT_DATA + sock_offset + sock;
    this->InterComm[sock] = new H5FDdsmSocket();
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
      masterInterCommHostName, this->InterSize*MPI_MAX_PORT_NAME, MPI_CHAR, 0, this->IntraComm);
  MPI_Gather(interCommPort, this->InterSize*sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR,
      masterInterCommPort, this->InterSize*sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, this->IntraComm);

  // Send it then to interComm 0
  if (this->Id == 0) {
    for (int i=0; i<((this->InterSize)*(this->IntraSize)); i++) {
      char tmpHost[MPI_MAX_PORT_NAME];
      H5FDdsmInt32 tmpPort = masterInterCommPort[i];
      strncpy(tmpHost, masterInterCommHostName+i*MPI_MAX_PORT_NAME, MPI_MAX_PORT_NAME);
      H5FDdsmDebug("Send info for intercomm socket on " << i << " to " << tmpHost << ":" << tmpPort);
    }
    // Send masterInterCommHostName
    this->DsmMasterSocket->Send(masterInterCommHostName,
        (this->InterSize)*(this->IntraSize)*MPI_MAX_PORT_NAME*sizeof(char));
    // Send masterInterCommPort
    this->DsmMasterSocket->Send(masterInterCommPort,
        (this->InterSize)*(this->IntraSize)*sizeof(H5FDdsmInt32));
  }
  //
  // Accept
  for (int sock=0; sock<(this->InterSize); sock++) {
    this->InterComm[sock]->Accept();
  }
  this->Barrier();
  //
  free(interCommHostName);
  free(masterInterCommHostName);
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

  interCommHostName = (H5FDdsmString) calloc(this->InterSize*MPI_MAX_PORT_NAME, sizeof(H5FDdsmByte));
  masterInterCommHostName = (H5FDdsmString) calloc((this->InterSize)*(this->IntraSize)*MPI_MAX_PORT_NAME, sizeof(H5FDdsmByte));
  interCommPort = (H5FDdsmInt32*) malloc(sizeof(H5FDdsmInt32)*this->InterSize);
  masterInterCommPort = (H5FDdsmInt32*) malloc(sizeof(H5FDdsmInt32)*(this->InterSize)*(this->IntraSize));

  if (this->Id == 0) {
    // Receive masterInterCommHostName
    this->DsmMasterSocket->Receive(masterInterCommHostName,
        (this->InterSize)*(this->IntraSize)*MPI_MAX_PORT_NAME*sizeof(char));
    // Receive masterInterCommPort
    this->DsmMasterSocket->Receive(masterInterCommPort,
        (this->InterSize)*(this->IntraSize)*sizeof(H5FDdsmInt32));

    for (int i=0; i<((this->InterSize)*(this->IntraSize)); i++) {
      if (i%(this->IntraSize) == 0) {
        strncpy(interCommHostName+(i/this->IntraSize)*MPI_MAX_PORT_NAME,
            masterInterCommHostName+i*MPI_MAX_PORT_NAME, MPI_MAX_PORT_NAME);
        interCommPort[i/this->IntraSize] = masterInterCommPort[i];
      } else {
        char tmpHost[MPI_MAX_PORT_NAME];
        H5FDdsmInt32 tmpPort = masterInterCommPort[i];
        strncpy(tmpHost, masterInterCommHostName+i*MPI_MAX_PORT_NAME, MPI_MAX_PORT_NAME);
        H5FDdsmDebug("Receive info for intercomm socket on " << i << " to " << tmpHost << ":" << tmpPort);
        MPI_Send(tmpHost, MPI_MAX_PORT_NAME, MPI_CHAR, i%(this->IntraSize), 0, this->IntraComm);
        MPI_Send(&tmpPort, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, i%(this->IntraSize), 0, this->IntraComm);
      }
    }
  } else {
    for (int i=0; i<this->InterSize; i++) {
      MPI_Recv((interCommHostName+i*MPI_MAX_PORT_NAME), MPI_MAX_PORT_NAME, MPI_CHAR, 0, 0, this->IntraComm, MPI_STATUS_IGNORE);
      MPI_Recv((interCommPort+i), sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, 0, this->IntraComm, MPI_STATUS_IGNORE);
    }
  }
  this->Barrier();

  H5FDdsmDebug("(" << this->Id << ") Creating " << this->InterSize << " sockets for intercomm");
  for (int sock=0; sock<(this->InterSize); sock++) {
    char tmpHost[MPI_MAX_PORT_NAME];
    H5FDdsmInt32 tmpPort = interCommPort[sock];
    strncpy(tmpHost, interCommHostName+sock*MPI_MAX_PORT_NAME, MPI_MAX_PORT_NAME);
    this->InterComm[sock] = new H5FDdsmSocket();
    // Connect
    H5FDdsmDebug("Connecting intercomm socket " << sock << " on " << this->Id << " to " << tmpHost << ":" << tmpPort);
    this->InterComm[sock]->Connect(tmpHost, tmpPort);
  }
  this->Barrier();
  //
  free(interCommHostName);
  free(masterInterCommHostName);
  free(interCommPort);
  free(masterInterCommPort);

  H5FDdsmDebug("Cleaned well interCommHostName/masterInterCommHostName");

  return H5FD_DSM_SUCCESS;
}
