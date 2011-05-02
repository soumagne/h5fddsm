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
#include <string>
//
#include "mpi.h"
//
#include "H5FDdsmIniFile.h"
#ifdef H5FD_DSM_HAVE_STEERING
#include "H5FDdsmSteerer.h"
#endif
#include "H5FDdsmDump.h"
//
#ifdef _WIN32
#include <io.h>
  #define access _access
  #define atoll _atoi64 
#endif

struct H5FDdsmManagerInternals
{
#ifdef H5FD_DSM_HAVE_STEERING
  struct SteeringEntryInt
  {
    SteeringEntryInt(std::string text, int nelements, int *values) : Text(text),
        NumberOfElements(nelements), Values(values) {}
    std::string Text;
    int  NumberOfElements;
    int *Values;
  };
  struct SteeringEntryDouble
  {
    SteeringEntryDouble(std::string text, int nelements, double *values) : Text(text),
        NumberOfElements(nelements), Values(values) {}
    std::string Text;
    int     NumberOfElements;
    double *Values;
  };

  typedef std::vector<SteeringEntryInt>    SteeringEntriesInt;
  typedef std::vector<SteeringEntryDouble> SteeringEntriesDouble;
  typedef std::vector<std::string>         SteeringEntriesString;

  SteeringEntriesInt    SteeringValuesInt;
  SteeringEntriesDouble SteeringValuesDouble;
  SteeringEntriesString RequestedDisabledObjects;
#endif
};
//----------------------------------------------------------------------------
H5FDdsmManager::H5FDdsmManager() 
{
  this->Communicator            = MPI_COMM_NULL;
  this->UpdatePiece             = 0;
  this->UpdateNumPieces         = 0;
  this->LocalBufferSizeMBytes   = 128;
  //
  this->DSMBuffer               = NULL;
  this->DSMComm                 = NULL;
  this->DsmType                 = H5FD_DSM_TYPE_UNIFORM;
  this->DsmBlockLength          = H5FD_DSM_DEFAULT_BLOCK_LENGTH;
  this->DsmCommType             = H5FD_DSM_COMM_MPI;
  this->DsmUseStaticInterComm   = 0;
  this->DsmIsServer             = 1;
  this->ServerHostName          = NULL;
  this->ServerPort              = 0;
  this->DsmConfigFilePath       = NULL;
  this->XMLStringSend           = NULL;
  this->ManagerInternals        = new H5FDdsmManagerInternals;
}
//----------------------------------------------------------------------------
H5FDdsmManager::~H5FDdsmManager()
{ 
  this->DestroyDSM();

  this->SetDsmConfigFilePath(NULL);
  this->SetXMLStringSend(NULL);
  delete this->ManagerInternals;
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
H5FDdsmInt32 H5FDdsmManager::GetDsmIsConnected()
{
  H5FDdsmInt32 ret = H5FD_DSM_FALSE;
  if (this->DSMBuffer) {
    if (this->DSMBuffer->GetIsConnected()) ret = H5FD_DSM_TRUE;
  }
  return(ret);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::WaitForConnected()
{
  H5FDdsmInt32 ret = H5FD_DSM_FALSE;
  if (this->DSMBuffer) {
    ret = this->DSMBuffer->WaitForConnected();
  }
  return(ret);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::GetDsmUpdateReady()
{
  H5FDdsmInt32 ret = H5FD_DSM_FALSE;
  if (this->DSMBuffer) {
    if (this->DSMBuffer->GetIsUpdateReady()) ret = H5FD_DSM_TRUE;
  }
  return(ret);
}
//----------------------------------------------------------------------------
void H5FDdsmManager::ClearDsmUpdateReady()
{
  if (this->DSMBuffer) {
    this->DSMBuffer->SetIsUpdateReady(H5FD_DSM_FALSE);
  }
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::WaitForUpdateReady()
{
  H5FDdsmInt32 ret = H5FD_DSM_FAIL;
  if (this->DSMBuffer) {
    ret = this->DSMBuffer->WaitForUpdateReady();
  }
  return(ret);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::GetDsmIsDataModified()
{
  H5FDdsmInt32 ret = H5FD_DSM_FALSE;
  if (this->DSMBuffer) {
    if (this->DSMBuffer->GetIsDataModified()) ret = H5FD_DSM_TRUE;
  }
  return(ret);
}
//----------------------------------------------------------------------------
void H5FDdsmManager::ClearDsmIsDataModified()
{
  if (this->DSMBuffer) {
    this->DSMBuffer->SetIsDataModified(0);
  }
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::GetDsmUpdateLevel()
{
  H5FDdsmInt32 ret = 0;
  if (this->DSMBuffer) {
    ret = this->DSMBuffer->GetUpdateLevel();
  }
  return(ret);
}
//----------------------------------------------------------------------------
void H5FDdsmManager::ClearDsmUpdateLevel()
{
  if (this->DSMBuffer) {
    this->DSMBuffer->SetUpdateLevel(H5FD_DSM_UPDATE_LEVEL_MAX);
  }
}
//----------------------------------------------------------------------------
void H5FDdsmManager::UpdateFinalize()
{
  // When UpdateFinalize, the server lock is released
  this->DSMBuffer->SetReleaseLockOnClose(true);
  if (this->DSMBuffer->GetIsConnected()) {
    this->DSMBuffer->RequestLockRelease();
  }
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::CreateDSM()
{
  if (this->DSMBuffer) return(H5FD_DSM_SUCCESS);

  MPI_Comm_size(this->Communicator, &this->UpdateNumPieces);
  MPI_Comm_rank(this->Communicator, &this->UpdatePiece);
  //
  // Create DSM communicator
  //
  switch (this->GetDsmCommType()) {
  case H5FD_DSM_COMM_MPI:
  case H5FD_DSM_COMM_MPI_RMA:
    this->DSMComm = new H5FDdsmCommMpi();
    this->DSMComm->SetCommType(this->GetDsmCommType());
    H5FDdsmDebug("Using MPI Intercomm...");
    if (this->GetDsmCommType() == H5FD_DSM_COMM_MPI_RMA) {
      this->DSMComm->SetUseOneSidedComm(true);
      H5FDdsmDebug("Using RMA Intercomm...");
    }
    break;
  case H5FD_DSM_COMM_SOCKET:
    this->DSMComm = new H5FDdsmCommSocket();
    H5FDdsmDebug("Using Socket Intercomm...");
    break;
#ifdef H5FD_DSM_HAVE_DMAPP
  case H5FD_DSM_COMM_DMAPP:
    this->DSMComm = new H5FDdsmCommDmapp();
    this->DSMComm->SetUseOneSidedComm(true);
    H5FDdsmDebug("Using DMAPP Intercomm...");
    break;
#endif
  default:
    H5FDdsmError("DSM communication type not supported");
    return(H5FD_DSM_FAIL);
    break;
  }
  this->DSMComm->SetUseStaticInterComm(this->GetDsmUseStaticInterComm());
  this->DSMComm->DupComm(this->Communicator);
  this->DSMComm->Init();
  //
  // Create the DSM buffer
  //
  this->DSMBuffer = new H5FDdsmBuffer();
  //
  if (this->DsmIsServer) {
    // Uniform Dsm : every node has a buffer the same size. (Addresses are sequential)
    H5FDdsmUInt64 length = (H5FDdsmUInt64)(this->GetLocalBufferSizeMBytes())*1024LU*1024LU;
    switch (this->DsmType) {
    case H5FD_DSM_TYPE_UNIFORM:
    case H5FD_DSM_TYPE_UNIFORM_RANGE:
      this->DSMBuffer->ConfigureUniform(this->DSMComm, length, -1, -1);
      break;
    case H5FD_DSM_TYPE_BLOCK_CYCLIC:
      this->DSMBuffer->ConfigureUniform(this->DSMComm, length, -1, -1, this->DsmBlockLength);
      break;
    default:
      H5FDdsmError("DSM configuration type not supported");
      return(H5FD_DSM_FAIL);
      break;
    }
    if (this->UpdatePiece == 0) {
      H5FDdsmDebug("Length set: " << this->DSMBuffer->GetLength() <<
         ", totalLength set: " << this->DSMBuffer->GetTotalLength() <<
         ", startServerId set: " << this->DSMBuffer->GetStartServerId() <<
         ", endServerId set: " << this->DSMBuffer->GetEndServerId());
    }
    //
    // setup service thread
    //
    this->DSMBuffer->StartService();
    H5FDdsmDebug("DSM Service Ready on " << this->UpdatePiece);
  }
  else {
    this->DSMBuffer->SetDsmType(this->GetDsmType());
    this->DSMBuffer->SetComm(this->DSMComm);
  }
  this->DSMBuffer->SetIsServer(this->DsmIsServer);
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::DestroyDSM()
{
  // Watch out that all processes have empty message queues
  // Should be already done during the disconnection
  if (this->DSMBuffer) {
    delete this->DSMBuffer;
    this->DSMBuffer = NULL;
    H5FDdsmDebug("DSM destroyed on " << this->UpdatePiece);
  }
  if (this->DSMComm) {
    delete this->DSMComm;
    this->DSMComm = NULL;
  }
  this->SetServerHostName(NULL);
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmBuffer *H5FDdsmManager::GetDSMHandle()
{
  return(this->DSMBuffer);
}
//----------------------------------------------------------------------------
void H5FDdsmManager::ClearDSM()
{
  this->DSMBuffer->ClearStorage();
  if (this->UpdatePiece == 0) H5FDdsmDebug("DSM cleared");
}
//----------------------------------------------------------------------------
void H5FDdsmManager::ConnectDSM(H5FDdsmBoolean persist)
{
  H5FDdsmInt32 status;

  if (this->UpdatePiece == 0) H5FDdsmDebug("Connect DSM");

  if (!this->DSMBuffer->GetIsConnected()) {

    if ((this->GetDsmCommType() == H5FD_DSM_COMM_MPI) ||
        (this->GetDsmCommType() == H5FD_DSM_COMM_MPI_RMA)) {
      if (this->GetServerHostName() != NULL) {
        dynamic_cast<H5FDdsmCommMpi*> (this->DSMBuffer->GetComm())->SetDsmMasterHostName(this->GetServerHostName());
        if (this->UpdatePiece == 0) {
          H5FDdsmDebug("Initializing connection to "
              << dynamic_cast<H5FDdsmCommMpi*> (this->DSMBuffer->GetComm())->GetDsmMasterHostName());
        }
      }
    }
    else if (this->GetDsmCommType() == H5FD_DSM_COMM_SOCKET) {
      if ((this->GetServerHostName() != NULL) && (this->GetServerPort() != 0)) {
        dynamic_cast<H5FDdsmCommSocket*> (this->DSMBuffer->GetComm())->SetDsmMasterHostName(this->GetServerHostName());
        dynamic_cast<H5FDdsmCommSocket*> (this->DSMBuffer->GetComm())->SetDsmMasterPort(this->GetServerPort());
        if (this->UpdatePiece == 0) {
          H5FDdsmDebug("Initializing connection to "
              << dynamic_cast<H5FDdsmCommSocket*> (this->DSMBuffer->GetComm())->GetDsmMasterHostName()
              << ":"
              << dynamic_cast<H5FDdsmCommSocket*> (this->DSMBuffer->GetComm())->GetDsmMasterPort());
        }
      }
    }
    else {
      if (this->UpdatePiece == 0) H5FDdsmError("NULL port");
    }
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

    do {
      status = this->DSMBuffer->GetComm()->RemoteCommConnect();
      if (status == H5FD_DSM_SUCCESS) {
        H5FDdsmDebug("Connected!");
        this->DSMBuffer->SetIsConnected(true);
        this->DSMBuffer->ReceiveInfo();
      }
      else {
#ifdef _WIN32
        Sleep(1000);
#else
        sleep(1);
#endif
        H5FDdsmDebug("DSMBuffer Comm_connect returned FAIL");
      }
    } while (persist && (status != H5FD_DSM_SUCCESS));
  }
}
//----------------------------------------------------------------------------
void H5FDdsmManager::ConnectInterCommDSM()
{
  H5FDdsmInt32 status = H5FD_DSM_FAIL;
  if (this->DsmIsServer) {
    this->DSMBuffer->RequestAccept();
  } else {
    status = this->DSMBuffer->GetComm()->RemoteCommConnect();
    if (status == H5FD_DSM_SUCCESS) {
      H5FDdsmDebug("Connected!");
      this->DSMBuffer->SetIsConnected(true);
      this->DSMBuffer->ReceiveInfo();
    }
  }
}
//----------------------------------------------------------------------------
void H5FDdsmManager::DisconnectDSM()
{
  if (this->UpdatePiece == 0) H5FDdsmDebug("Disconnect DSM");
  this->DSMBuffer->RequestDisconnect();
}
//----------------------------------------------------------------------------
void H5FDdsmManager::PublishDSM()
{
  if (this->UpdatePiece == 0) H5FDdsmDebug("Opening port...");
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
    H5FDdsmConstString dsmEnvPath = getenv("H5FD_DSM_CONFIG_PATH");
    if (!dsmEnvPath) dsmEnvPath = getenv("HOME");
    if (dsmEnvPath) {
      this->SetDsmConfigFilePath(dsmEnvPath);
    }
    if (this->GetDsmConfigFilePath()) {
      fullDsmConfigFilePath = std::string(this->GetDsmConfigFilePath()) +
          std::string("/.dsm_client_config");
      dsmConfigFile.Create(fullDsmConfigFilePath);
      dsmConfigFile.AddSection("Comm", fullDsmConfigFilePath);

      H5FDdsmDebug("Written " << fullDsmConfigFilePath.c_str());

    }
    if ((this->GetDsmCommType() == H5FD_DSM_COMM_MPI) || (this->GetDsmCommType() == H5FD_DSM_COMM_MPI_RMA)) {
      this->SetServerHostName(dynamic_cast<H5FDdsmCommMpi*> (this->DSMBuffer->GetComm())->GetDsmMasterHostName());
      H5FDdsmDebug("Server PortName: " << this->GetServerHostName());
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
      this->SetServerHostName(dynamic_cast<H5FDdsmCommSocket*> (this->DSMBuffer->GetComm())->GetDsmMasterHostName());
      this->SetServerPort(dynamic_cast<H5FDdsmCommSocket*> (this->DSMBuffer->GetComm())->GetDsmMasterPort());
      H5FDdsmDebug("Server HostName: " << this->GetServerHostName() << ", Server port: " << this->GetServerPort());
      if (this->GetDsmConfigFilePath()) {
        char serverPort[32];
        sprintf(serverPort, "%d", this->GetServerPort());
        dsmConfigFile.SetValue("DSM_COMM_SYSTEM", "socket", "Comm", fullDsmConfigFilePath);
        dsmConfigFile.SetValue("DSM_BASE_HOST", this->GetServerHostName(), "Comm", fullDsmConfigFilePath);
        dsmConfigFile.SetValue("DSM_BASE_PORT", serverPort, "Comm", fullDsmConfigFilePath);
      }
    }
    dsmConfigFile.SetValue("DSM_STATIC_INTERCOMM", "false", "Comm", fullDsmConfigFilePath);
  }
  //
  this->DSMBuffer->RequestAccept();
}
//----------------------------------------------------------------------------
void H5FDdsmManager::UnpublishDSM()
{
  if (this->UpdatePiece == 0) H5FDdsmDebug("Closing port...");
  this->DSMBuffer->GetComm()->ClosePort();

  if (this->UpdatePiece == 0) {
    if (this->GetServerHostName() != NULL) {
      this->SetServerHostName(NULL);
      this->SetServerPort(0);
    }
  }
  if (this->UpdatePiece == 0) H5FDdsmDebug("Port closed");
}
//----------------------------------------------------------------------------
void H5FDdsmManager::H5Dump()
{  
  if (this->DSMBuffer) {
    H5FDdsmDump *myDsmDump = new H5FDdsmDump();
    myDsmDump->SetDsmBuffer(this->DSMBuffer);
    myDsmDump->SetFileName("DSM.h5");
    myDsmDump->Dump();
    if (this->UpdatePiece == 0) H5FDdsmDebug("Dump done");
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
    if (this->UpdatePiece == 0) H5FDdsmDebug("Dump light done");
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
    if (this->UpdatePiece == 0) H5FDdsmDebug("Dump XML done");
    if (this->UpdatePiece == 0) H5FDdsmDebug(dumpStream.str().c_str());
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
H5FDdsmConstString H5FDdsmManager::GetXMLStringReceive()
{
  if (this->DSMBuffer) return this->DSMBuffer->GetXMLDescription();
  return(NULL);
}
//----------------------------------------------------------------------------
void H5FDdsmManager::ClearXMLStringReceive()
{
  this->DSMBuffer->SetXMLDescription(NULL);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 FileExists(H5FDdsmConstString fname)
{
  if(access(fname, 0) != -1) {
      return(H5FD_DSM_TRUE);
  } else {
      return(H5FD_DSM_FALSE);
  }
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::ReadDSMConfigFile()
{
  H5FDdsmIniFile config;
  std::string configPath;
  std::string mode = "client";
  H5FDdsmConstString dsmEnvPath = getenv("H5FD_DSM_CONFIG_PATH");
  H5FDdsmConstString dsmServerEnvPath = getenv("H5FD_DSM_SERVER_CONFIG_PATH");

  if (dsmServerEnvPath) {
    mode = "server";
    configPath = std::string(dsmServerEnvPath) + std::string("/.dsm_server_config");
  } else {
    if (!dsmEnvPath) dsmEnvPath = getenv("HOME");
    configPath = std::string(dsmEnvPath) + std::string("/.dsm_client_config");
  }
  if (FileExists(configPath.c_str())) {
    if (this->UpdatePiece == 0) {
      H5FDdsmDebug("Reading from " << configPath.c_str());
    }
    std::string size = config.GetValue("DSM_INIT_SIZE",   "General", configPath);

    std::string comm = config.GetValue("DSM_COMM_SYSTEM", "Comm", configPath);
    std::string static_intercomm = config.GetValue("DSM_STATIC_INTERCOMM", "Comm", configPath);
    std::string host = config.GetValue("DSM_BASE_HOST",   "Comm", configPath);
    std::string port = config.GetValue("DSM_BASE_PORT",   "Comm", configPath);

    // General settings
    if (mode == "server" && atoi(size.c_str())) {
      this->SetLocalBufferSizeMBytes(atoi(size.c_str()));
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
    } else if (comm == "dmapp") {
      this->SetDsmCommType(H5FD_DSM_COMM_DMAPP);
    }
    if (static_intercomm == "true") this->SetDsmUseStaticInterComm(1);
    this->SetServerHostName(host.c_str());
    return(H5FD_DSM_SUCCESS);
  }
  return(H5FD_DSM_FAIL);
}
//----------------------------------------------------------------------------
#ifdef H5FD_DSM_HAVE_STEERING
void H5FDdsmManager::WriteSteeredData()
{
  if (this->ManagerInternals->SteeringValuesInt.size() ||
      this->ManagerInternals->SteeringValuesDouble.size()) {
    this->DSMBuffer->GetSteerer()->BeginInteractionsCache(H5F_ACC_RDWR);
    while (!this->ManagerInternals->SteeringValuesInt.empty()) {
      H5FDdsmManagerInternals::SteeringEntryInt entryInt =
          this->ManagerInternals->SteeringValuesInt.back();
      this->DSMBuffer->GetSteerer()->WriteInteractions(entryInt.Text.c_str(),
          entryInt.NumberOfElements, entryInt.Values);
      this->ManagerInternals->SteeringValuesInt.pop_back();
    }
    while (!this->ManagerInternals->SteeringValuesDouble.empty()) {
      H5FDdsmManagerInternals::SteeringEntryDouble entryDouble =
          this->ManagerInternals->SteeringValuesDouble.back();
      this->DSMBuffer->GetSteerer()->WriteInteractions(entryDouble.Text.c_str(),
          entryDouble.NumberOfElements, entryDouble.Values);
      this->ManagerInternals->SteeringValuesDouble.pop_back();
    }
    this->DSMBuffer->GetSteerer()->EndInteractionsCache();
  }
}
//----------------------------------------------------------------------------
void H5FDdsmManager::UpdateSteeredObjects()
{
  this->WriteSteeredData();

  while (!this->ManagerInternals->RequestedDisabledObjects.empty()) {
    this->DSMBuffer->GetSteerer()->SetDisabledObject(
        this->ManagerInternals->RequestedDisabledObjects.back().c_str());
    this->ManagerInternals->RequestedDisabledObjects.pop_back();
  }

  this->DSMBuffer->GetSteerer()->UpdateSteeringCommands();
  this->DSMBuffer->GetSteerer()->UpdateDisabledObjects();
}
//----------------------------------------------------------------------------
void H5FDdsmManager::SetSteeringCommand(H5FDdsmConstString cmd)
{
  H5FDdsmDebug("cmd: " << cmd);
  if (cmd) {
    if (!strcmp(cmd, "none")) { return; }
    // Send command
    this->DSMBuffer->GetSteerer()->SetCurrentCommand(cmd);
  }
}
//----------------------------------------------------------------------------
void H5FDdsmManager::SetSteeringValues(const char *name, int numberOfElements, int *values)
{
  if (numberOfElements) {
    H5FDdsmInt32 entryExists = H5FD_DSM_FALSE;
    // Check if the entry already exists
    H5FDdsmManagerInternals::SteeringEntriesInt::iterator iter =
        this->ManagerInternals->SteeringValuesInt.begin();
    for (; iter < this->ManagerInternals->SteeringValuesInt.end(); iter++) {
      if (iter->Text == std::string(name)) {
        entryExists = true;
        break;
      }
    }
    if (entryExists) {
      for (int i = 0; i < numberOfElements; i++) {
        iter->Values[i] = values[i];
      }
    } else {
      int *entryValues = new int[numberOfElements];
      for (int i=0; i<numberOfElements; i++) {
        entryValues[i] = values[i];
      }
      this->ManagerInternals->SteeringValuesInt.push_back(
          H5FDdsmManagerInternals::SteeringEntryInt(name, numberOfElements, entryValues));
    }
  }
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::GetSteeringValues(const char *name, int numberOfElements, int *values)
{
  H5FDdsmInt32 ret = H5FD_DSM_FALSE;
  if (numberOfElements) {
    this->DSMBuffer->GetSteerer()->BeginInteractionsCache(H5F_ACC_RDONLY);
    ret = (H5FD_DSM_SUCCESS==this->DSMBuffer->GetSteerer()->ReadInteractions(name, numberOfElements, values));
    this->DSMBuffer->GetSteerer()->EndInteractionsCache();
  }
  return(ret);
}
//----------------------------------------------------------------------------
void H5FDdsmManager::SetSteeringValues(const char *name, int numberOfElements, double *values)
{
  if (numberOfElements) {
    H5FDdsmBoolean entryExists = false;
    // Check if the entry already exists
    H5FDdsmManagerInternals::SteeringEntriesDouble::iterator iter =
        this->ManagerInternals->SteeringValuesDouble.begin();
    for (; iter != this->ManagerInternals->SteeringValuesDouble.end(); iter++) {
      if (iter->Text == std::string(name)) {
        entryExists = true;
        break;
      }
    }
    if (entryExists) {
      for (int i = 0; i < numberOfElements; i++) {
        iter->Values[i] = values[i];
      }
    } else {
      double *entryValues = new double[numberOfElements];
      for (int i = 0; i < numberOfElements; i++) {
        entryValues[i] = values[i];
      }
      this->ManagerInternals->SteeringValuesDouble.push_back(
          H5FDdsmManagerInternals::SteeringEntryDouble(name, numberOfElements, entryValues));
    }
  }
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::GetSteeringValues(const char *name, int numberOfElements, double *values)
{
  H5FDdsmInt32 ret = H5FD_DSM_FALSE;
  if (numberOfElements) {
    this->DSMBuffer->GetSteerer()->BeginInteractionsCache(H5F_ACC_RDONLY);
    ret = (H5FD_DSM_SUCCESS==this->DSMBuffer->GetSteerer()->ReadInteractions(name, numberOfElements, values));
    this->DSMBuffer->GetSteerer()->EndInteractionsCache();
  }
  return(ret);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::GetInteractionsGroupPresent()
{
  H5FDdsmInt32 ret = H5FD_DSM_FALSE;
  if ((this->DSMBuffer != NULL) && (this->DSMBuffer->GetSteerer() != NULL)) {
    ret = (H5FD_DSM_SUCCESS==this->DSMBuffer->GetSteerer()->BeginInteractionsCache(H5F_ACC_RDONLY));
    this->DSMBuffer->GetSteerer()->EndInteractionsCache();
  }
  return(ret);
}
//----------------------------------------------------------------------------
void H5FDdsmManager::SetDisabledObject(H5FDdsmString objectName)
{
  this->ManagerInternals->RequestedDisabledObjects.push_back(objectName);
}
#endif
