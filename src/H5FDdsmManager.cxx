/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmManager.cxx

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
#include "H5FDdsmManager.h"
//
#include "H5FDdsmCommSocket.h"
#include "H5FDdsmCommMpi.h"
#include "H5FDdsmCommMpiRma.h"
//
#ifdef __CRAYXT_COMPUTE_LINUX_TARGET
  #ifdef H5FD_DSM_HAVE_DMAPP
    #include "H5FDdsmCommDmapp.h"
  #endif
  #ifdef H5FD_DSM_HAVE_UGNI
    #include "H5FDdsmCommUGni.h"
  #endif
#endif
//
#include "H5FDdsmIniFile.h"
//
#ifdef H5FD_DSM_HAVE_STEERING
  #include "H5FDdsmSteerer.h"
#endif
//
#include "H5FDdsmDump.h"
//
#ifdef _WIN32
#include <io.h>
  #define access _access
  #define atoll _atoi64 
#endif
//
#include <vector>
#include <string>

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

MPI_Comm H5FDdsmManager::MpiComm = MPI_COMM_NULL;
//----------------------------------------------------------------------------
H5FDdsmManager::H5FDdsmManager() 
{
  this->MpiComm                 = MPI_COMM_NULL;
  this->UpdatePiece             = 0;
  this->UpdateNumPieces         = 0;
  this->LocalBufferSizeMBytes   = 128;
  //
  this->DsmBuffer               = NULL;
  this->DsmComm                 = NULL;
  this->DsmType                 = H5FD_DSM_TYPE_UNIFORM;
  this->BlockLength             = H5FD_DSM_DEFAULT_BLOCK_LENGTH;
  this->InterCommType           = H5FD_DSM_COMM_MPI;
  this->UseStaticInterComm      = H5FD_DSM_FALSE;
  this->IsServer                = H5FD_DSM_TRUE;
  this->ServerHostName          = NULL;
  this->ServerPort              = 0;
  this->XMLStringSend           = NULL;
  this->ManagerInternals        = new H5FDdsmManagerInternals;
}

//----------------------------------------------------------------------------
H5FDdsmManager::~H5FDdsmManager()
{ 
  this->Destroy();

  this->SetXMLStringSend(NULL);
  delete this->ManagerInternals;
}

//----------------------------------------------------------------------------
void H5FDdsmManager::SetMpiComm(MPI_Comm comm)
{
  if (comm != this->MpiComm) {
    this->MpiComm = comm;
    if (this->MpiComm != MPI_COMM_NULL) {
      MPI_Comm_size(this->MpiComm, &this->UpdateNumPieces);
      MPI_Comm_rank(this->MpiComm, &this->UpdatePiece);
    }
  }
}

//----------------------------------------------------------------------------
MPI_Comm H5FDdsmManager::GetGlobalMPICommunicator()
{
    return H5FDdsmManager::MpiComm;
}
//----------------------------------------------------------------------------
H5FDdsmBoolean H5FDdsmManager::GetIsConnected()
{
  H5FDdsmBoolean ret = H5FD_DSM_FALSE;
  if (this->DsmBuffer) {
    if (this->DsmBuffer->GetIsConnected()) ret = H5FD_DSM_TRUE;
  }
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::WaitForConnection()
{
  H5FDdsmInt32 ret = H5FD_DSM_FAIL;
  if (this->DsmBuffer) {
    ret = this->DsmBuffer->WaitForConnection();
  }
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmBoolean H5FDdsmManager::GetIsNotified()
{
  H5FDdsmBoolean ret = H5FD_DSM_FALSE;
  if (this->DsmBuffer) {
    if (this->DsmBuffer->GetIsNotified()) ret = H5FD_DSM_TRUE;
  }
  return(ret);
}

//----------------------------------------------------------------------------
void H5FDdsmManager::ClearIsNotified()
{
  if (this->DsmBuffer) {
    this->DsmBuffer->SetIsNotified(H5FD_DSM_FALSE);
  }
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::WaitForNotification()
{
  H5FDdsmInt32 ret = H5FD_DSM_FAIL;
  if (this->DsmBuffer) {
    ret = this->DsmBuffer->WaitForNotification();
  }
  return(ret);
}

//----------------------------------------------------------------------------
void H5FDdsmManager::NotificationFinalize()
{
  // When UpdateFinalize, the server lock is released
  this->DsmBuffer->SetReleaseLockOnClose(H5FD_DSM_TRUE);
  if (this->DsmBuffer->GetIsConnected()) {
    this->DsmBuffer->RequestLockRelease();
  }
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::GetNotification()
{
  H5FDdsmInt32 ret = 0;
  if (this->DsmBuffer) {
    ret = this->DsmBuffer->GetNotification();
  }
  return(ret);
}

//----------------------------------------------------------------------------
void H5FDdsmManager::ClearNotification()
{
  if (this->DsmBuffer) {
    this->DsmBuffer->SetNotification(0);
  }
}

//----------------------------------------------------------------------------
H5FDdsmBoolean H5FDdsmManager::GetIsDataModified()
{
  H5FDdsmBoolean ret = H5FD_DSM_FALSE;
  if (this->DsmBuffer) {
    if (this->DsmBuffer->GetIsDataModified()) ret = H5FD_DSM_TRUE;
  }
  return(ret);
}

//----------------------------------------------------------------------------
void H5FDdsmManager::ClearIsDataModified()
{
  if (this->DsmBuffer) {
    this->DsmBuffer->SetIsDataModified(H5FD_DSM_FALSE);
  }
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::Create()
{
  if (this->DsmBuffer) return(H5FD_DSM_SUCCESS);

  MPI_Comm_size(this->MpiComm, &this->UpdateNumPieces);
  MPI_Comm_rank(this->MpiComm, &this->UpdatePiece);
  //
  // Create DSM communicator
  //
  switch (this->GetInterCommType()) {
  case H5FD_DSM_COMM_MPI:
    this->DsmComm = new H5FDdsmCommMpi();
    H5FDdsmDebug("Using MPI Intercomm...");
    break;
  case H5FD_DSM_COMM_MPI_RMA:
    this->DsmComm = new H5FDdsmCommMpiRma();
    H5FDdsmDebug("Using MPI RMA Intercomm...");
    break;
  case H5FD_DSM_COMM_SOCKET:
    this->DsmComm = new H5FDdsmCommSocket();
    H5FDdsmDebug("Using Socket Intercomm...");
    break;
#ifdef __CRAYXT_COMPUTE_LINUX_TARGET
#ifdef H5FD_DSM_HAVE_DMAPP
  case H5FD_DSM_COMM_DMAPP:
    this->DsmComm = new H5FDdsmCommDmapp();
    H5FDdsmDebug("Using DMAPP Intercomm...");
    break;
#endif
#ifdef H5FD_DSM_HAVE_UGNI
  case H5FD_DSM_COMM_UGNI:
    this->DsmComm = new H5FDdsmCommUGni();
    H5FDdsmDebug("Using UGNI Intercomm...");
    break;
#endif
#endif
  default:
    H5FDdsmError("DSM communication type not supported");
    return(H5FD_DSM_FAIL);
    break;
  }
  this->DsmComm->SetUseStaticInterComm(this->GetUseStaticInterComm());
  this->DsmComm->DupComm(this->MpiComm);
  this->DsmComm->Init();
  //
  // Create the DSM buffer
  //
  this->DsmBuffer = new H5FDdsmBufferService();
  //
  if (this->IsServer) {
    // Uniform Dsm : every node has a buffer the same size. (Addresses are sequential)
    H5FDdsmUInt64 length = (H5FDdsmUInt64) (this->GetLocalBufferSizeMBytes())*1024LU*1024LU;
    switch (this->DsmType) {
    case H5FD_DSM_TYPE_UNIFORM:
    case H5FD_DSM_TYPE_UNIFORM_RANGE:
      this->DsmBuffer->ConfigureUniform(this->DsmComm, length, -1, -1);
      break;
    case H5FD_DSM_TYPE_BLOCK_CYCLIC:
      this->DsmBuffer->ConfigureUniform(this->DsmComm, length, -1, -1, this->BlockLength, H5FD_DSM_FALSE);
      break;
    case H5FD_DSM_TYPE_BLOCK_RANDOM:
      this->DsmBuffer->ConfigureUniform(this->DsmComm, length, -1, -1, this->BlockLength, H5FD_DSM_TRUE);
      break;
    case H5FD_DSM_TYPE_DYNAMIC_MASK:
      this->DsmBuffer->ConfigureUniform(this->DsmComm, length, -1, -1);
      this->DsmBuffer->SetDsmType(H5FD_DSM_TYPE_DYNAMIC_MASK);
      break;
    default:
      H5FDdsmError("DSM configuration type not supported");
      return(H5FD_DSM_FAIL);
      break;
    }
    if (this->UpdatePiece == 0) {
      H5FDdsmDebug("Length set: " << this->DsmBuffer->GetLength() <<
         ", totalLength set: " << this->DsmBuffer->GetTotalLength() <<
         ", startServerId set: " << this->DsmBuffer->GetStartServerId() <<
         ", endServerId set: " << this->DsmBuffer->GetEndServerId());
    }
    //
    // setup service thread
    //
    this->DsmBuffer->StartService();
    H5FDdsmDebug("DSM Service Ready on " << this->UpdatePiece);
  }
  else {
    this->DsmBuffer->SetDsmType(this->GetDsmType());
    this->DsmBuffer->SetComm(this->DsmComm);
  }
  this->DsmBuffer->SetIsServer(this->IsServer);
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::Destroy()
{
  // Watch out that all processes have empty message queues
  // Should be already done during the disconnection
  if (this->DsmBuffer) {
    delete this->DsmBuffer;
    this->DsmBuffer = NULL;
    H5FDdsmDebug("DSM destroyed on " << this->UpdatePiece);
  }
  if (this->DsmComm) {
    delete this->DsmComm;
    this->DsmComm = NULL;
  }
  this->SetServerHostName(NULL);
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::ClearStorage()
{
  H5FDdsmInt32 ret = this->DsmBuffer->ClearStorage();
  if (this->UpdatePiece == 0) H5FDdsmDebug("DSM cleared");
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::Connect(H5FDdsmBoolean persist)
{
  H5FDdsmInt32 status = H5FD_DSM_FAIL;

  if (this->UpdatePiece == 0) H5FDdsmDebug("Connect DSM");

  if (!this->DsmBuffer->GetIsConnected()) {

    if (!this->UseStaticInterComm) {
      if ((this->GetInterCommType() == H5FD_DSM_COMM_MPI) ||
          (this->GetInterCommType() == H5FD_DSM_COMM_MPI_RMA)) {
        if (this->GetServerHostName() != NULL) {
          dynamic_cast<H5FDdsmCommMpi*> (this->DsmBuffer->GetComm())->SetDsmMasterHostName(this->GetServerHostName());
          if (this->UpdatePiece == 0) {
            H5FDdsmDebug("Initializing connection to "
                << dynamic_cast<H5FDdsmCommMpi*> (this->DsmBuffer->GetComm())->GetDsmMasterHostName());
          }
        }
      }
      else if (this->GetInterCommType() == H5FD_DSM_COMM_SOCKET) {
        if ((this->GetServerHostName() != NULL) && (this->GetServerPort() != 0)) {
          dynamic_cast<H5FDdsmCommSocket*> (this->DsmBuffer->GetComm())->SetDsmMasterHostName(this->GetServerHostName());
          dynamic_cast<H5FDdsmCommSocket*> (this->DsmBuffer->GetComm())->SetDsmMasterPort(this->GetServerPort());
          if (this->UpdatePiece == 0) {
            H5FDdsmDebug("Initializing connection to "
                << dynamic_cast<H5FDdsmCommSocket*> (this->DsmBuffer->GetComm())->GetDsmMasterHostName()
                << ":"
                << dynamic_cast<H5FDdsmCommSocket*> (this->DsmBuffer->GetComm())->GetDsmMasterPort());
          }
        }
      }
      else {
        if (this->UpdatePiece == 0) H5FDdsmError("NULL port");
      }
#ifdef H5FD_DSM_DEBUG
      this->DsmBuffer->DebugOn();
      this->DsmBuffer->GetComm()->DebugOn();
      if (this->GetInterCommType() == H5FD_DSM_COMM_SOCKET) {
        H5FDdsmConstString hostName = dynamic_cast<H5FDdsmCommSocket*> (this->DsmBuffer->GetComm())->GetDsmMasterHostName();
        H5FDdsmInt32 port = dynamic_cast<H5FDdsmCommSocket*> (this->DsmBuffer->GetComm())->GetDsmMasterPort();
        H5FDdsmDebug(<<"DSM driver connecting on: " << hostName << ":" << port);
      }
      else   if ((this->GetInterCommType() == H5FD_DSM_COMM_MPI) ||
          (this->GetInterCommType() == H5FD_DSM_COMM_MPI_RMA)) {
        H5FDdsmConstString hostName = dynamic_cast<H5FDdsmCommMpi*> (this->DsmBuffer->GetComm())->GetDsmMasterHostName();
        H5FDdsmDebug(<<"DSM driver connecting on: " << hostName);
      }
#endif
    }

    do {
      status = this->DsmBuffer->GetComm()->Connect();
      if (status == H5FD_DSM_SUCCESS) {
        H5FDdsmDebug("Connected!");
        this->DsmBuffer->SetIsConnected(H5FD_DSM_TRUE);
        this->DsmBuffer->ReceiveInfo();
      }
      else {
#ifdef _WIN32
        Sleep(1000);
#else
        sleep(1);
#endif
        H5FDdsmDebug("Buffer Comm_connect returned FAIL");
      }
    } while (persist && (status != H5FD_DSM_SUCCESS));
  }
  return(status);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::Disconnect()
{
  if (this->UpdatePiece == 0) H5FDdsmDebug("Disconnect DSM");
  this->DsmBuffer->RequestDisconnect();
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::Publish()
{
  H5FDdsmConstString dsmEnvPath = getenv("H5FD_DSM_CONFIG_PATH");
#ifdef _WIN32
  if (!dsmEnvPath) dsmEnvPath = getenv("USERPROFILE");
#else
  if (!dsmEnvPath) dsmEnvPath = getenv("HOME");
#endif
  //
  if (!dsmEnvPath) {
    H5FDdsmError("Could not find DSM config path");
    return(H5FD_DSM_FAIL);
  } else {
    if (!this->UseStaticInterComm) {
      if (this->UpdatePiece == 0) H5FDdsmDebug("Opening port...");
      if (this->GetInterCommType() == H5FD_DSM_COMM_SOCKET) {
        dynamic_cast<H5FDdsmCommSocket*> (this->DsmBuffer->GetComm())->SetDsmMasterHostName(this->GetServerHostName());
        dynamic_cast<H5FDdsmCommSocket*> (this->DsmBuffer->GetComm())->SetDsmMasterPort(this->GetServerPort());
      }
      this->DsmBuffer->GetComm()->OpenPort();
    }
    // Only write config file if process 0
    if (this->UpdatePiece == 0) {
      H5FDdsmIniFile configFile;
      std::string configFilePath = std::string(dsmEnvPath);
#ifdef _WIN32
      configFilePath += std::string("\\.dsm_client_config");
#else
      configFilePath += std::string("/.dsm_client_config");
#endif
      configFile.Create(configFilePath);
      configFile.AddSection("Comm", configFilePath);
      H5FDdsmDebug("Written " << configFilePath.c_str());

      if ((this->GetInterCommType() == H5FD_DSM_COMM_MPI) || (this->GetInterCommType() == H5FD_DSM_COMM_MPI_RMA)) {
        this->SetServerHostName(dynamic_cast<H5FDdsmCommMpi*> (this->DsmBuffer->GetComm())->GetDsmMasterHostName());
        H5FDdsmDebug("Server PortName: " << this->GetServerHostName());
        if (this->GetInterCommType() == H5FD_DSM_COMM_MPI) {
          configFile.SetValue("DSM_COMM_SYSTEM", "mpi", "Comm", configFilePath);
        }
        if (this->GetInterCommType() == H5FD_DSM_COMM_MPI_RMA) {
          configFile.SetValue("DSM_COMM_SYSTEM", "mpi_rma", "Comm", configFilePath);
        }
        configFile.SetValue("DSM_BASE_HOST", this->GetServerHostName(), "Comm", configFilePath);
      }
      else if (this->GetInterCommType() == H5FD_DSM_COMM_SOCKET) {
        this->SetServerHostName(dynamic_cast<H5FDdsmCommSocket*> (this->DsmBuffer->GetComm())->GetDsmMasterHostName());
        this->SetServerPort(dynamic_cast<H5FDdsmCommSocket*> (this->DsmBuffer->GetComm())->GetDsmMasterPort());
        H5FDdsmDebug("Server HostName: " << this->GetServerHostName() << ", Server port: " << this->GetServerPort());
        char serverPort[32];
        sprintf(serverPort, "%d", this->GetServerPort());
        configFile.SetValue("DSM_COMM_SYSTEM", "socket", "Comm", configFilePath);
        configFile.SetValue("DSM_BASE_HOST", this->GetServerHostName(), "Comm", configFilePath);
        configFile.SetValue("DSM_BASE_PORT", serverPort, "Comm", configFilePath);
      }
      if (!this->UseStaticInterComm) {
        configFile.SetValue("DSM_STATIC_INTERCOMM", "false", "Comm", configFilePath);
      } else {
        configFile.SetValue("DSM_STATIC_INTERCOMM", "true", "Comm", configFilePath);
      }
    }
    this->DsmBuffer->RequestAccept();
  }
  //
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::Unpublish()
{
  if (this->UpdatePiece == 0) H5FDdsmDebug("Closing port...");
  this->DsmBuffer->GetComm()->ClosePort();

  if (this->UpdatePiece == 0) {
    if (this->GetServerHostName() != NULL) {
      this->SetServerHostName(NULL);
      this->SetServerPort(0);
    }
  }
  if (this->UpdatePiece == 0) H5FDdsmDebug("Port closed");
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
void H5FDdsmManager::H5Dump()
{  
  if (this->DsmBuffer) {
    H5FDdsmDump *myDsmDump = new H5FDdsmDump();
    myDsmDump->SetDsmBuffer(this->DsmBuffer);
    myDsmDump->SetFileName("DSM.h5");
    myDsmDump->Dump();
    if (this->UpdatePiece == 0) H5FDdsmDebug("Dump done");
    delete myDsmDump;
  }
}

//----------------------------------------------------------------------------
void H5FDdsmManager::H5DumpLight()
{  
  if (this->DsmBuffer) {
    H5FDdsmDump *myDsmDump = new H5FDdsmDump();
    myDsmDump->SetDsmBuffer(this->DsmBuffer);
    myDsmDump->SetFileName("DSM.h5");
    myDsmDump->DumpLight();
    if (this->UpdatePiece == 0) H5FDdsmDebug("Dump light done");
    delete myDsmDump;
  }
}

//----------------------------------------------------------------------------
void H5FDdsmManager::H5DumpXML()
{
  if (this->DsmBuffer) {
    std::ostringstream dumpStream;
    H5FDdsmDump *myDsmDump = new H5FDdsmDump();
    myDsmDump->SetDsmBuffer(this->DsmBuffer);
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
  this->DsmBuffer->RequestXMLExchange();
  if (this->XMLStringSend != NULL) {
    int commServerSize = this->DsmBuffer->GetEndServerId() - this->DsmBuffer->GetStartServerId() + 1;
    for (int i=0; i<commServerSize; i++) {
      this->DsmBuffer->GetComm()->SendXML(this->XMLStringSend, i);
    }
  }
  this->DsmBuffer->GetComm()->Barrier();
}

//----------------------------------------------------------------------------
H5FDdsmConstString H5FDdsmManager::GetXMLStringReceive()
{
  if (this->DsmBuffer) return this->DsmBuffer->GetXMLDescription();
  return(NULL);
}

//----------------------------------------------------------------------------
void H5FDdsmManager::ClearXMLStringReceive()
{
  this->DsmBuffer->SetXMLDescription(NULL);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 FileExists(H5FDdsmConstString fname)
{
  if (access(fname, 0) != -1) {
      return(H5FD_DSM_TRUE);
  } else {
      return(H5FD_DSM_FALSE);
  }
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::ReadConfigFile()
{
  H5FDdsmIniFile configFile;
  std::string configFilePath;
  std::string mode = "client";
  H5FDdsmConstString dsmEnvPath = getenv("H5FD_DSM_CONFIG_PATH");
  H5FDdsmConstString dsmServerEnvPath = getenv("H5FD_DSM_SERVER_CONFIG_PATH");

  if (dsmServerEnvPath) {
    mode = "server";
    configFilePath = std::string(dsmServerEnvPath) + std::string("/.dsm_server_config");
  } else {
#ifdef _WIN32
    if (!dsmEnvPath) dsmEnvPath = getenv("USERPROFILE");
    configFilePath = std::string(dsmEnvPath) + std::string("\\.dsm_client_config");
#else
    if (!dsmEnvPath) dsmEnvPath = getenv("HOME");
    configFilePath = std::string(dsmEnvPath) + std::string("/.dsm_client_config");
#endif
    if (!dsmEnvPath) {
      H5FDdsmError("Could not find DSM config path");
      return(H5FD_DSM_FAIL);
    }
  }
  if (FileExists(configFilePath.c_str())) {
    if (this->UpdatePiece == 0) H5FDdsmDebug("Reading from " << configFilePath.c_str());
    std::string size = configFile.GetValue("DSM_INIT_SIZE",   "General", configFilePath);

    std::string comm = configFile.GetValue("DSM_COMM_SYSTEM", "Comm", configFilePath);
    std::string static_intercomm = configFile.GetValue("DSM_STATIC_INTERCOMM", "Comm", configFilePath);
    std::string host = configFile.GetValue("DSM_BASE_HOST",   "Comm", configFilePath);
    std::string port = configFile.GetValue("DSM_BASE_PORT",   "Comm", configFilePath);

    // General settings
    if (mode == "server" && atoi(size.c_str())) {
      this->SetLocalBufferSizeMBytes(atoi(size.c_str()));
      this->SetIsServer(H5FD_DSM_TRUE);
    } else {
      this->SetIsServer(H5FD_DSM_FALSE);
    }

    // Comm settings
    if (comm == "socket") {
      this->SetInterCommType(H5FD_DSM_COMM_SOCKET);
      this->SetServerPort(atoi(port.c_str()));
    } else if (comm == "mpi") {
      this->SetInterCommType(H5FD_DSM_COMM_MPI);
    } else if (comm == "mpi_rma") {
      this->SetInterCommType(H5FD_DSM_COMM_MPI_RMA);
    } else if (comm == "dmapp") {
      this->SetInterCommType(H5FD_DSM_COMM_DMAPP);
    }
    if (static_intercomm == "true") this->SetUseStaticInterComm(H5FD_DSM_TRUE);
    this->SetServerHostName(host.c_str());
    return(H5FD_DSM_SUCCESS);
  }
  return(H5FD_DSM_FAIL);
}

//----------------------------------------------------------------------------
#ifdef H5FD_DSM_HAVE_STEERING
H5FDdsmInt32 H5FDdsmManager::WriteSteeredData()
{
  if (this->ManagerInternals->SteeringValuesInt.size() ||
      this->ManagerInternals->SteeringValuesDouble.size()) {
    this->DsmBuffer->GetSteerer()->BeginInteractionsCache(H5F_ACC_RDWR);
    while (!this->ManagerInternals->SteeringValuesInt.empty()) {
      H5FDdsmManagerInternals::SteeringEntryInt entryInt =
          this->ManagerInternals->SteeringValuesInt.back();
      this->DsmBuffer->GetSteerer()->WriteInteractions(entryInt.Text.c_str(),
          entryInt.NumberOfElements, entryInt.Values);
      delete []entryInt.Values;
      this->ManagerInternals->SteeringValuesInt.pop_back();
    }
    while (!this->ManagerInternals->SteeringValuesDouble.empty()) {
      H5FDdsmManagerInternals::SteeringEntryDouble entryDouble =
          this->ManagerInternals->SteeringValuesDouble.back();
      this->DsmBuffer->GetSteerer()->WriteInteractions(entryDouble.Text.c_str(),
          entryDouble.NumberOfElements, entryDouble.Values);
      delete []entryDouble.Values;
      this->ManagerInternals->SteeringValuesDouble.pop_back();
    }
    this->DsmBuffer->GetSteerer()->EndInteractionsCache();
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::UpdateSteeredObjects()
{
  this->WriteSteeredData();

  while (!this->ManagerInternals->RequestedDisabledObjects.empty()) {
    this->DsmBuffer->GetSteerer()->SetDisabledObject(
        this->ManagerInternals->RequestedDisabledObjects.back().c_str());
    this->ManagerInternals->RequestedDisabledObjects.pop_back();
  }

  this->DsmBuffer->GetSteerer()->UpdateSteeringCommands();
  this->DsmBuffer->GetSteerer()->UpdateDisabledObjects();
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
void H5FDdsmManager::SetSteeringCommand(H5FDdsmConstString cmd)
{
  H5FDdsmDebug("cmd: " << cmd);
  if (cmd) {
    if (!strcmp(cmd, "none")) { return; }
    // Send command
    this->DsmBuffer->GetSteerer()->SetCurrentCommand(cmd);
  }
}

//----------------------------------------------------------------------------
void H5FDdsmManager::SetSteeringValues(const char *name, int numberOfElements, int *values)
{
  if (numberOfElements) {
    H5FDdsmBoolean entryExists = H5FD_DSM_FALSE;
    // Check if the entry already exists
    H5FDdsmManagerInternals::SteeringEntriesInt::iterator iter =
        this->ManagerInternals->SteeringValuesInt.begin();
    for (; iter < this->ManagerInternals->SteeringValuesInt.end(); iter++) {
      if (iter->Text == std::string(name)) {
        entryExists = H5FD_DSM_TRUE;
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
    this->DsmBuffer->GetSteerer()->BeginInteractionsCache(H5F_ACC_RDONLY);
    ret = (H5FD_DSM_SUCCESS==this->DsmBuffer->GetSteerer()->ReadInteractions(name, numberOfElements, values));
    this->DsmBuffer->GetSteerer()->EndInteractionsCache();
  }
  return(ret);
}

//----------------------------------------------------------------------------
void H5FDdsmManager::SetSteeringValues(const char *name, int numberOfElements, double *values)
{
  if (numberOfElements) {
    H5FDdsmBoolean entryExists = H5FD_DSM_FALSE;
    // Check if the entry already exists
    H5FDdsmManagerInternals::SteeringEntriesDouble::iterator iter =
        this->ManagerInternals->SteeringValuesDouble.begin();
    for (; iter != this->ManagerInternals->SteeringValuesDouble.end(); iter++) {
      if (iter->Text == std::string(name)) {
        entryExists = H5FD_DSM_TRUE;
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
    this->DsmBuffer->GetSteerer()->BeginInteractionsCache(H5F_ACC_RDONLY);
    ret = (H5FD_DSM_SUCCESS==this->DsmBuffer->GetSteerer()->ReadInteractions(name, numberOfElements, values));
    this->DsmBuffer->GetSteerer()->EndInteractionsCache();
  }
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmManager::GetInteractionsGroupPresent()
{
  H5FDdsmInt32 ret = H5FD_DSM_FALSE;
  if ((this->DsmBuffer != NULL) && (this->DsmBuffer->GetSteerer() != NULL)) {
    ret = (H5FD_DSM_SUCCESS==this->DsmBuffer->GetSteerer()->BeginInteractionsCache(H5F_ACC_RDONLY));
    this->DsmBuffer->GetSteerer()->EndInteractionsCache();
  }
  return(ret);
}

//----------------------------------------------------------------------------
void H5FDdsmManager::SetDisabledObject(H5FDdsmConstString objectName)
{
  this->ManagerInternals->RequestedDisabledObjects.push_back(objectName);
}
#endif
