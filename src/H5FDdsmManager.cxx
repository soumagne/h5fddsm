/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmManager.cxx

  Copyright (C) CSCS - Swiss National Supercomputing Centre.
  You may use modify and and distribute this code freely providing 
  1) This copyright notice appears on all copies of source code 
  2) An acknowledgment appears with any substantial usage of the code
  3) If this code is contributed to any other open source project, it 
  must not be reformatted such that the indentation, bracketing or 
  overall style is modified significantly. 

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=========================================================================*/
#include "H5FDdsmManager.h"
//
#include <vector>
//
#include "mpi.h"
//
#include "H5FDdsmCommSocket.h"
#include "H5FDdsmCommMpi.h"
#include "H5FDdsmMsg.h"
//
#include "H5FDdsmDump.h"
//
#ifdef _WIN32
#include <io.h>
  #define access _access
  #define atoll _atoi64 
#endif

//----------------------------------------------------------------------------
H5FDdsmManager::H5FDdsmManager() 
{
  this->Communicator            = MPI_COMM_NULL;
  this->UpdatePiece             = 0;
  this->UpdateNumPieces         = 0;
  this->LocalBufferSizeMBytes   = 128;
#ifdef HAVE_PTHREADS
  this->ServiceThread           = 0;
#elif HAVE_BOOST_THREADS
  this->ServiceThread           = NULL;
#endif

  //
  this->Communicator            = NULL;
  this->DSMBuffer               = NULL;
  this->DSMComm                 = NULL;
  this->DsmCommType             = H5FD_DSM_COMM_MPI;
  this->DsmIsServer             = 1;
  this->ServerHostName          = NULL;
  this->ServerPort              = 0;
  this->DsmConfigFilePath       = NULL;
  this->DsmUpdateReady          = 0;
  this->SteeringCommand         = NULL;
  this->XMLStringSend           = NULL;
}
//----------------------------------------------------------------------------
H5FDdsmManager::~H5FDdsmManager()
{ 
  this->DestroyDSM();

  this->SetDsmConfigFilePath(NULL);
  this->SetSteeringCommand(NULL);
  this->SetXMLStringSend(NULL);
}
//----------------------------------------------------------------------------
void H5FDdsmManager::SetCommunicator(MPI_Comm comm)
{
  if (comm!=this->Communicator) {
    this->Communicator = comm;
    if (this->Communicator!=MPI_COMM_NULL) {
      MPI_Comm_size(this->Communicator, &this->UpdateNumPieces);
      MPI_Comm_rank(this->Communicator, &this->UpdatePiece);
    }
  }
}
//----------------------------------------------------------------------------
int H5FDdsmManager::GetAcceptedConnection()
{
  int ret = 0;
  if (this->DSMBuffer) {
    if (this->DSMBuffer->GetIsConnected()) ret = 1;
  }
  return ret;
}
//----------------------------------------------------------------------------
int H5FDdsmManager::GetDsmUpdateReady()
{
  int ret = 0;
  if (this->DSMBuffer) {
    if (this->DSMBuffer->GetIsUpdateReady()) ret = 1;
  }
  return ret;
}
//----------------------------------------------------------------------------
void H5FDdsmManager::ClearDsmUpdateReady()
{
  if (this->DSMBuffer) {
    this->DSMBuffer->SetIsUpdateReady(0);
  }
}
//----------------------------------------------------------------------------
bool H5FDdsmManager::DestroyDSM()
{
  if (this->DSMBuffer && this->DSMBuffer->GetIsServer() && this->UpdatePiece == 0) {
    this->DSMBuffer->SendDone();
  }
/* @TODO, client doesn't need to call SendDone, but can if it wants to 
  if (this->DSMBuffer && this->DSMBuffer->GetIsConnected() && this->UpdatePiece == 0) {
    // @TODO watch out that all processes have empty message queues
    this->DSMBuffer->SendDone();
  }
*/
#ifdef HAVE_PTHREADS
  if (this->ServiceThread) {
    pthread_join(this->ServiceThread, NULL);
    this->ServiceThread = 0;
  }
#elif HAVE_BOOST_THREADS
  if (this->ServiceThread) {
    this->ServiceThread->join();
    delete this->ServiceThread;
    this->ServiceThread = NULL;
  }
#endif

  if (this->DSMComm) {
    delete this->DSMComm;
    this->DSMComm = NULL;
  }
  if (this->DSMBuffer) {
    delete this->DSMBuffer;
    this->DSMBuffer = NULL;
    H5FDdsmDebug(<<"DSM destroyed on " << this->UpdatePiece);
  }

  this->SetServerHostName(NULL);

  return true;
}
//----------------------------------------------------------------------------
H5FDdsmBuffer *H5FDdsmManager::GetDSMHandle()
{
  return DSMBuffer;
}
//----------------------------------------------------------------------------
#ifdef HAVE_PTHREADS
// nothing at the moment
#elif HAVE_BOOST_THREADS
class DSMServiceThread 
{
public:
  DSMServiceThread(H5FDdsmBuffer *dsmObject)
  {
    this->DSMObject = dsmObject;
  }
  void operator()() {
    this->DSMObject->ServiceThread();
  }
  //
  H5FDdsmBuffer *DSMObject;
};
#endif
//----------------------------------------------------------------------------
bool H5FDdsmManager::CreateDSM()
{
  if (this->DSMBuffer) {
    return true;
  }

  MPI_Comm_size(this->Communicator, &this->UpdateNumPieces);
  MPI_Comm_rank(this->Communicator, &this->UpdatePiece);

  //
  // Create Xdmf DSM communicator
  //
  if ((this->GetDsmCommType() == H5FD_DSM_COMM_MPI) ||
      (this->GetDsmCommType() == H5FD_DSM_COMM_MPI_RMA)) {
    this->DSMComm = new H5FDdsmCommMpi();
    if (this->GetDsmCommType() == H5FD_DSM_COMM_MPI) H5FDdsmDebug(<< "Using MPI Intercomm...");
    if (this->GetDsmCommType() == H5FD_DSM_COMM_MPI_RMA) {
      this->DSMComm->SetCommType(H5FD_DSM_COMM_MPI_RMA);
      H5FDdsmDebug(<< "Using MPI RMA Intercomm...");
    }
    dynamic_cast<H5FDdsmCommMpi*> (this->DSMComm)->DupComm(this->Communicator);
  }
  else if (this->GetDsmCommType() == H5FD_DSM_COMM_SOCKET) {
    this->DSMComm = new H5FDdsmCommSocket();
    H5FDdsmDebug(<< "Using Socket Intercomm...");
    dynamic_cast<H5FDdsmCommSocket*> (this->DSMComm)->DupComm(this->Communicator);
  }
  // this->DSMComm->DebugOn();
  this->DSMComm->Init();
  //
  // Create the DSM buffer
  //
  this->DSMBuffer = new H5FDdsmBuffer();
  // this->DSMBuffer->DebugOn();
  this->DSMBuffer->SetServiceThreadUseCopy(0);

  if (this->DsmIsServer) {
    // Uniform Dsm : every node has a buffer the same size. (Addresses are sequential)
    this->DSMBuffer->ConfigureUniform(this->DSMComm, ((long) this->GetLocalBufferSizeMBytes())*1024*1024, -1, -1);
    if (this->UpdatePiece == 0) {
      H5FDdsmDebug(<< "Length set: " << this->DSMBuffer->GetLength() <<
         ", totalLength set: " << this->DSMBuffer->GetTotalLength() <<
         ", startServerId set: " << this->DSMBuffer->GetStartServerId() <<
         ", endServerId set: " << this->DSMBuffer->GetEndServerId());
    }
    //
    // setup service thread
    //
    H5FDdsmDebug(<< "Creating service thread...");
#ifdef HAVE_PTHREADS
    // Start another thread to handle DSM requests from other nodes
    pthread_create(&this->ServiceThread, NULL, &H5FDdsmBufferServiceThread, (void *) this->DSMBuffer);
#elif HAVE_BOOST_THREADS
    DSMServiceThread MyDSMServiceThread(this->DSMBuffer);
    this->ServiceThread = new boost::thread(MyDSMServiceThread);
#endif

    // Wait for DSM to be ready
    while (!this->DSMBuffer->GetThreadDsmReady()) {
      // Spin until service initialized
    }
    this->DSMBuffer->SetIsServer(true);
    H5FDdsmDebug(<<"DSM Service Ready on " << this->UpdatePiece);
  } 
  else {
    this->DSMBuffer->SetIsServer(false);
    this->DSMBuffer->SetComm(this->DSMComm);
  }

  return true;
}
//----------------------------------------------------------------------------
void H5FDdsmManager::ClearDSM()
{
  this->DSMBuffer->ClearStorage();
  if (this->UpdatePiece == 0) H5FDdsmDebug(<< "DSM cleared");
}
//----------------------------------------------------------------------------
void H5FDdsmManager::RequestRemoteChannel()
{
  this->DSMBuffer->RequestRemoteChannel();
}
//----------------------------------------------------------------------------
void H5FDdsmManager::ConnectDSM()
{
  if (this->UpdatePiece == 0) H5FDdsmDebug(<< "Connect DSM");

  if ((this->GetDsmCommType() == H5FD_DSM_COMM_MPI) ||
      (this->GetDsmCommType() == H5FD_DSM_COMM_MPI_RMA)) {
    if (this->GetServerHostName() != NULL) {
      dynamic_cast<H5FDdsmCommMpi*> (this->DSMBuffer->GetComm())->SetDsmMasterHostName(this->GetServerHostName());
      if (this->UpdatePiece == 0) {
        H5FDdsmDebug(<< "Initializing connection to "
            << dynamic_cast<H5FDdsmCommMpi*> (this->DSMBuffer->GetComm())->GetDsmMasterHostName());
      }
    }
  }
  else if (this->GetDsmCommType() == H5FD_DSM_COMM_SOCKET) {
    if ((this->GetServerHostName() != NULL) && (this->GetServerPort() != 0)) {
      dynamic_cast<H5FDdsmCommSocket*> (this->DSMBuffer->GetComm())->SetDsmMasterHostName(this->GetServerHostName());
      dynamic_cast<H5FDdsmCommSocket*> (this->DSMBuffer->GetComm())->SetDsmMasterPort(this->GetServerPort());
      if (this->UpdatePiece == 0) {
        H5FDdsmDebug(<< "Initializing connection to "
            << dynamic_cast<H5FDdsmCommSocket*> (this->DSMBuffer->GetComm())->GetDsmMasterHostName()
            << ":"
            << dynamic_cast<H5FDdsmCommSocket*> (this->DSMBuffer->GetComm())->GetDsmMasterPort());
      }
    }
  }
  else {
    if (this->UpdatePiece == 0) H5FDdsmError(<< "NULL port");
  }


  if (!this->DSMBuffer->GetIsConnected()) {
#ifdef H5FD_DSM_DEBUG
    this->DSMBuffer->DebugOn();
    this->DSMBuffer->GetComm()->DebugOn();
    if (this->GetDsmCommType() == H5FD_DSM_COMM_SOCKET) {
      H5FDdsmConstString hostName = dynamic_cast<H5FDdsmCommSocket*> (this->DSMBuffer->GetComm())->GetDsmMasterHostName();
      H5FDdsmInt32 port = dynamic_cast<H5FDdsmCommSocket*> (this->DSMBuffer->GetComm())->GetDsmMasterPort();
      H5FDdsmDebug(<<"DSM driver connecting on: " << hostName << ":" << port);
    }
    else   if ((this->GetDsmCommType() == H5FD_DSM_COMM_MPI) ||
        (this->GetDsmCommType() == H5FD_DSM_COMM_MPI_RMA)) {
      H5FDdsmConstString hostName = dynamic_cast<H5FDdsmCommMpi*> (this->DSMBuffer->GetComm())->GetDsmMasterHostName();
      H5FDdsmDebug(<<"DSM driver connecting on: " << hostName);
    }
#endif
    if (this->DSMBuffer->GetComm()->RemoteCommConnect() == H5FD_DSM_SUCCESS) {
      H5FDdsmDebug(<< "Connected!");
      this->DSMBuffer->SetIsConnected(true);

      // Receive DSM info
      H5FDdsmInt64 length;
      H5FDdsmInt64 totalLength;
      H5FDdsmInt32 startServerId, endServerId;

      this->DSMBuffer->GetComm()->RemoteCommRecvInfo(&length, &totalLength, &startServerId, &endServerId);

      this->DSMBuffer->SetLength(length, 0);
      H5FDdsmDebug(<<"Length received: " << this->DSMBuffer->GetLength());

      this->DSMBuffer->SetTotalLength(totalLength);
      H5FDdsmDebug(<<"totalLength received: " << this->DSMBuffer->GetTotalLength());

      this->DSMBuffer->SetStartServerId(startServerId);
      H5FDdsmDebug(<<"startServerId received: " << this->DSMBuffer->GetStartServerId());

      this->DSMBuffer->SetEndServerId(endServerId);
      H5FDdsmDebug(<<"endServerId received: " << this->DSMBuffer->GetEndServerId());
    }
    else {
      H5FDdsmError(<< "DSMBuffer Comm_connect error");
    }
  }
}
//----------------------------------------------------------------------------
void H5FDdsmManager::DisconnectDSM()
{
  if (this->UpdatePiece == 0) H5FDdsmDebug(<< "Disconnect DSM");
  this->DSMBuffer->GetComm()->RemoteCommRecvReady();
  this->DSMBuffer->RequestDisconnection(); // Go back to normal channel
}
//----------------------------------------------------------------------------
void H5FDdsmManager::PublishDSM()
{
  if (this->UpdatePiece == 0) H5FDdsmDebug(<< "Opening port...");
  if (this->GetDsmCommType() == H5FD_DSM_COMM_SOCKET) {
    dynamic_cast<H5FDdsmCommSocket*>
    (this->DSMBuffer->GetComm())->SetDsmMasterHostName(this->GetServerHostName());
    dynamic_cast<H5FDdsmCommSocket*>
    (this->DSMBuffer->GetComm())->SetDsmMasterPort(this->GetServerPort());
  }
  this->DSMBuffer->GetComm()->OpenPort();

  //
  // Only write config file if process 0
  //
  if (this->UpdatePiece == 0) {
    H5FDdsmIniFile dsmConfigFile;
    std::string fullDsmConfigFilePath;
    const char *dsmEnvPath = getenv("H5FD_DSM_CONFIG_PATH");
    if (!dsmEnvPath) dsmEnvPath = getenv("HOME");
    if (dsmEnvPath) {
      this->SetDsmConfigFilePath(dsmEnvPath);
    }
    if (this->GetDsmConfigFilePath()) {
      fullDsmConfigFilePath = std::string(this->GetDsmConfigFilePath()) +
          std::string("/.dsm_client_config");
      dsmConfigFile.Create(fullDsmConfigFilePath);
      dsmConfigFile.AddSection("Comm", fullDsmConfigFilePath);

      std::cout << "Written " << fullDsmConfigFilePath.c_str() << std::endl;

    }
    if ((this->GetDsmCommType() == H5FD_DSM_COMM_MPI) ||
        (this->GetDsmCommType() == H5FD_DSM_COMM_MPI_RMA)) {
      this->SetServerHostName(dynamic_cast<H5FDdsmCommMpi*>
      (this->DSMBuffer->GetComm())->GetDsmMasterHostName());
      H5FDdsmDebug(<< "Server PortName: " << this->GetServerHostName());
      if (this->GetDsmConfigFilePath()) {
        if (this->GetDsmCommType() == H5FD_DSM_COMM_MPI) {
          dsmConfigFile.SetValue("DSM_COMM_SYSTEM", "mpi", "Comm", fullDsmConfigFilePath);
        }
        if (this->GetDsmCommType() == H5FD_DSM_COMM_MPI_RMA) {
          dsmConfigFile.SetValue("DSM_COMM_SYSTEM", "mpi_rma", "Comm", fullDsmConfigFilePath);
        }
        dsmConfigFile.SetValue("DSM_BASE_HOST", this->GetServerHostName(), "Comm", fullDsmConfigFilePath);
      }
    } else if (this->GetDsmCommType() == H5FD_DSM_COMM_SOCKET) {
      this->SetServerHostName(dynamic_cast<H5FDdsmCommSocket*>
      (this->DSMBuffer->GetComm())->GetDsmMasterHostName());
      this->SetServerPort(dynamic_cast<H5FDdsmCommSocket*>
      (this->DSMBuffer->GetComm())->GetDsmMasterPort());
      H5FDdsmDebug(<< "Server HostName: " << this->GetServerHostName()
          << ", Server port: " << this->GetServerPort());
      if (this->GetDsmConfigFilePath()) {
        char serverPort[32];
        sprintf(serverPort, "%d", this->GetServerPort());
        dsmConfigFile.SetValue("DSM_COMM_SYSTEM", "socket", "Comm", fullDsmConfigFilePath);
        dsmConfigFile.SetValue("DSM_BASE_HOST", this->GetServerHostName(), "Comm", fullDsmConfigFilePath);
        dsmConfigFile.SetValue("DSM_BASE_PORT", serverPort, "Comm", fullDsmConfigFilePath);
      }
    }
  }
  //
  this->DSMBuffer->RequestRemoteChannel();
}
//----------------------------------------------------------------------------

void H5FDdsmManager::UnpublishDSM()
{
  if (this->UpdatePiece == 0) H5FDdsmDebug(<< "Closing port...");
  this->DSMBuffer->GetComm()->ClosePort();

  if (this->UpdatePiece == 0) {
    if (this->GetServerHostName() != NULL) {
      this->SetServerHostName(NULL);
      this->SetServerPort(0);
    }
  }
  if (this->UpdatePiece == 0) H5FDdsmDebug(<< "Port closed");
}
//----------------------------------------------------------------------------
void H5FDdsmManager::H5Dump()
{  
  if (this->DSMBuffer) {
    H5FDdsmDump *myDsmDump = new H5FDdsmDump();
    myDsmDump->SetDsmBuffer(this->DSMBuffer);
    myDsmDump->SetFileName("DSM.h5");
    myDsmDump->Dump();
    if (this->UpdatePiece == 0) H5FDdsmDebug(<< "Dump done");
    delete myDsmDump;
  }
}
//----------------------------------------------------------------------------
void H5FDdsmManager::H5DumpLight()
{  
  if (this->DSMBuffer) {
    H5FDdsmDump *myDsmDump = new H5FDdsmDump();
    myDsmDump->SetDsmBuffer(this->DSMBuffer);
    myDsmDump->SetFileName("DSM.h5");
    myDsmDump->DumpLight();
    if (this->UpdatePiece == 0) H5FDdsmDebug(<< "Dump light done");
    delete myDsmDump;
  }
}
//----------------------------------------------------------------------------
void H5FDdsmManager::H5DumpXML()
{
  if (this->DSMBuffer) {
    std::ostringstream dumpStream;
    H5FDdsmDump *myDsmDump = new H5FDdsmDump();
    myDsmDump->SetDsmBuffer(this->DSMBuffer);
    myDsmDump->SetFileName("DSM.h5");
    myDsmDump->DumpXML(dumpStream);
    if (this->UpdatePiece == 0) H5FDdsmDebug(<< "Dump XML done");
    if (this->UpdatePiece == 0) H5FDdsmDebug(<< dumpStream.str().c_str());
    delete myDsmDump;
  }
}
//----------------------------------------------------------------------------
void H5FDdsmManager::SendDSMXML()
{
  this->DSMBuffer->RequestXMLExchange();
  if (this->XMLStringSend != NULL) {
    int commServerSize = this->DSMBuffer->GetEndServerId() - this->DSMBuffer->GetStartServerId() + 1;
    for (int i=0; i<commServerSize; i++) {
      this->DSMBuffer->GetComm()->RemoteCommSendXML(this->XMLStringSend, i);
    }
  }
  this->DSMBuffer->GetComm()->Barrier();
}
//----------------------------------------------------------------------------
const char *H5FDdsmManager::GetXMLStringReceive()
{
  if (this->DSMBuffer) return this->DSMBuffer->GetXMLDescription();
  return NULL;
}
//----------------------------------------------------------------------------
void H5FDdsmManager::ClearXMLStringReceive()
{
  this->DSMBuffer->SetXMLDescription(NULL);
}
//----------------------------------------------------------------------------
bool FileExists(const char *fname) 
{
  if( access( fname, 0 ) != -1 ) {
      return true;
  } else {
      return false;
  }
}
//----------------------------------------------------------------------------
bool H5FDdsmManager::ReadDSMConfigFile()
{
  H5FDdsmIniFile config;
  std::string configPath;
  std::string mode = "client";
  const char *dsmEnvPath = getenv("H5FD_DSM_CONFIG_PATH");
  const char *dsmServerEnvPath = getenv("H5FD_DSM_SERVER_CONFIG_PATH");

  if (dsmServerEnvPath) {
    mode = "server";
    configPath = std::string(dsmServerEnvPath) + std::string("/.dsm_server_config");
  } else {
    if (!dsmEnvPath) dsmEnvPath = getenv("HOME");
    configPath = std::string(dsmEnvPath) + std::string("/.dsm_client_config");
  }
  if (FileExists(configPath.c_str())) {
    if (this->UpdatePiece == 0) {
      std::cout << "Reading from " << configPath.c_str() << std::endl;
    }
    std::string size = config.GetValue("DSM_INIT_SIZE",   "General", configPath);

    std::string comm = config.GetValue("DSM_COMM_SYSTEM", "Comm", configPath);
    std::string host = config.GetValue("DSM_BASE_HOST",   "Comm", configPath);
    std::string port = config.GetValue("DSM_BASE_PORT",   "Comm", configPath);

    // General settings
    if (mode == "server" && atoll(size.c_str())) {
      this->SetLocalBufferSizeMBytes(atoll(size.c_str()));
      this->SetDsmIsServer(true);
    } else {
      this->SetDsmIsServer(false);
    }

    // Comm settings
    if (comm == "socket") {
      this->SetDsmCommType(H5FD_DSM_COMM_SOCKET);
      this->SetServerPort(atoi(port.c_str()));
    } else if (comm == "mpi") {
      this->SetDsmCommType(H5FD_DSM_COMM_MPI);
    } else if (comm == "mpi_rma") {
      this->SetDsmCommType(H5FD_DSM_COMM_MPI_RMA);
    }
    this->SetServerHostName(host.c_str());
    return true;
  }
  return false;
}
//----------------------------------------------------------------------------
void H5FDdsmManager::SetSteeringCommand(char *cmd)
{
  H5FDdsmDebug(<< "cmd: " << cmd);
  if (!strcmp(cmd, "none")) { return; }
  if (this->SteeringCommand) { delete [] this->SteeringCommand; this->SteeringCommand = NULL; }
  if (cmd) {
    this->SteeringCommand = new char[strlen(cmd) + 1];
    strcpy(this->SteeringCommand, cmd);
    // Send command
    this->DSMBuffer->GetSteerer()->SetCurrentCommand(cmd);
  }
}
//----------------------------------------------------------------------------
