/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmSteerer.cxx

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

#include "H5FDdsmSteerer.h"
#include "H5FDdsmBuffer.h"
#include "H5FDdsmComm.h"
#include "H5FDdsmCommMpi.h"
#include "H5FDdsmCommSocket.h"
//
#include "H5FDdsmDump.h"
//
#include <string>
//----------------------------------------------------------------------------
H5FDdsmSteerer::H5FDdsmSteerer(H5FDdsmBuffer *buffer)
{
  this->WriteToDSM = 1;
  this->CurrentCommand = NULL;
  this->SetCurrentCommand("none");
  this->DsmBuffer = buffer;
  this->SteerableObjects = NULL;
}
//----------------------------------------------------------------------------
H5FDdsmSteerer::~H5FDdsmSteerer()
{
  this->SetCurrentCommand(NULL);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::SetCurrentCommand(H5FDdsmConstString cmd)
{
  H5FDdsmDebug("SetCurrentCommand to: " << cmd);
  if (this->CurrentCommand == cmd) { return(H5FD_DSM_SUCCESS); }
  if (this->CurrentCommand && cmd && !strcmp(this->CurrentCommand, cmd)) { return(H5FD_DSM_SUCCESS); }
  if (this->CurrentCommand) { delete [] this->CurrentCommand; this->CurrentCommand = NULL; }
  if (cmd) {
    this->CurrentCommand = new char[strlen(cmd) + 1];
    strcpy(this->CurrentCommand, cmd);
    if (strcmp(this->CurrentCommand, "play") == 0) {
      H5FDdsmDebug("Sending ready...");
      this->DsmBuffer->GetComm()->RemoteCommSendReady();
      H5FDdsmDebug("Ready sent");
    }
  }
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::UpdateSteeringCommands()
{
  H5FDdsmInt64 addr;
  H5FDdsmMetaData metadata;

  H5FDdsmDebug("Sending steering command " << this->CurrentCommand);
  strcpy(metadata.steering_cmd, (H5FDdsmConstString)this->CurrentCommand);
  addr = this->DsmBuffer->GetTotalLength() - sizeof(metadata) + sizeof(metadata.entry);
  // Only one of the processes writing to the DSM needs to write file metadata
  // but we must be careful that all the processes keep the metadata synchronized
  if (this->DsmBuffer->GetComm()->GetId() == 0) {
    if (this->DsmBuffer->Put(addr, sizeof(metadata.steering_cmd), metadata.steering_cmd) != H5FD_DSM_SUCCESS) return H5FD_DSM_FAIL;
  }
  this->DsmBuffer->GetComm()->Barrier();
  this->SetCurrentCommand("none");
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::GetSteeringCommands()
{
  H5FDdsmInt64 addr;
  H5FDdsmMetaData metadata;
  MPI_Comm comm = MPI_COMM_NULL;

  addr = this->DsmBuffer->GetTotalLength() - sizeof(metadata) + sizeof(metadata.entry);

  if (this->DsmBuffer->GetComm()->GetId() == 0) {
    if (this->DsmBuffer->Get(addr, sizeof(metadata.steering_cmd), metadata.steering_cmd) != H5FD_DSM_SUCCESS) {
      H5FDdsmDebug("DsmGetSteeringCmd failed");
      return H5FD_DSM_FAIL;
    }
  }
  if (this->DsmBuffer->GetComm()->GetCommType() == H5FD_DSM_COMM_SOCKET) {
    comm = dynamic_cast <H5FDdsmCommSocket*> (this->DsmBuffer->GetComm())->GetComm();
  }
  else if ((this->DsmBuffer->GetComm()->GetCommType() == H5FD_DSM_COMM_MPI) ||
      (this->DsmBuffer->GetComm()->GetCommType() == H5FD_DSM_COMM_MPI_RMA)) {
    comm = dynamic_cast <H5FDdsmCommMpi*> (this->DsmBuffer->GetComm())->GetComm();
  }
  MPI_Bcast(metadata.steering_cmd, sizeof(metadata.steering_cmd), MPI_UNSIGNED_CHAR, 0, comm);
  H5FDdsmDebug("Received steering command: " << metadata.steering_cmd);
  if (this->CheckCommand((H5FDdsmConstString) metadata.steering_cmd) == H5FD_DSM_SUCCESS) {
    // Steering command successfully treated, clear it
    this->UpdateSteeringCommands();
  }
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::IsSteerable(H5FDdsmConstString hdfPath)
{
  std::cout << "IsSteerable: " << hdfPath << std::endl;

  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::GetBoolean(H5FDdsmConstString name, void *data)
{
  H5FDdsmDump *myDsmDump = new H5FDdsmDump();
  myDsmDump->SetDsmBuffer(this->DsmBuffer);
  myDsmDump->SetFileName("DSM.h5");
  myDsmDump->DumpLight();
  delete myDsmDump;
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::GetScalar(H5FDdsmConstString name, void *data)
{
  H5FDdsmDump *myDsmDump = new H5FDdsmDump();
  myDsmDump->SetDsmBuffer(this->DsmBuffer);
  myDsmDump->SetFileName("DSM.h5");
  myDsmDump->DumpLight();
  delete myDsmDump;
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::GetVector(H5FDdsmConstString name, void *data)
{
  H5FDdsmDump *myDsmDump = new H5FDdsmDump();
  myDsmDump->SetDsmBuffer(this->DsmBuffer);
  myDsmDump->SetFileName("DSM.h5");
  myDsmDump->DumpLight();
  delete myDsmDump;
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::WriteInteractions(H5FDdsmConstString name, H5FDdsmInteractionType type, void *data)
{

  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::CheckCommand(H5FDdsmConstString command)
{
  std::string stringCommand = command;

  if (stringCommand == "none") {
    return H5FD_DSM_FAIL;
  }
  else if (stringCommand == "pause") {
    H5FDdsmDebug("Receiving ready...");
    this->DsmBuffer->GetComm()->RemoteCommRecvReady();
    H5FDdsmDebug("Ready received");
  }
  else if (stringCommand == "play") {
    // nothing special
  }
  else if (stringCommand == "restart") {

  }
  else if (stringCommand == "notSend") {

  }
  else if (stringCommand == "disk") {
    H5FDdsmDebug("Setting WriteToDSM to 0");
    this->WriteToDSM = 0;
  }
  else if (stringCommand == "dsm") {
    H5FDdsmDebug("Setting WriteToDSM to 1");
    this->WriteToDSM = 1;
  }
  return(H5FD_DSM_SUCCESS);
}
