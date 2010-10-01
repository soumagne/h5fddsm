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
#include "H5FDdsmComm.h"
#include <string>
//----------------------------------------------------------------------------
H5FDdsmSteerer::H5FDdsmSteerer()
{
  this->Pause = 0;
  this->WriteToDSM = 1;
  this->CurrentCommand = NULL;
  this->Comm = NULL;
  // this->DebugOn();
}
//----------------------------------------------------------------------------
H5FDdsmSteerer::~H5FDdsmSteerer()
{
  this->SetCurrentCommand(NULL);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::SetCurrentCommand(H5FDdsmConstString cmd)
{
  std::string oldCommand;
  H5FDdsmDebug("SetCurrentCommand to: " << cmd);
  if (this->CurrentCommand == cmd) { return(H5FD_DSM_SUCCESS); }
  if (this->CurrentCommand != NULL) { oldCommand = this->CurrentCommand; }
  if (this->CurrentCommand && cmd && !strcmp(this->CurrentCommand, cmd)) { return(H5FD_DSM_SUCCESS); }
  if (this->CurrentCommand) { delete [] this->CurrentCommand; this->CurrentCommand = NULL; }
  if (cmd) {
    this->CurrentCommand = new char[strlen(cmd) + 1];
    strcpy(this->CurrentCommand, cmd);
    if ((oldCommand == "pause") && strcmp(this->CurrentCommand, "play") == 0) {
      this->SendSteeringCommands();
    }
  }
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
void H5FDdsmSteerer::SendSteeringCommands()
{
  if (this->CurrentCommand == NULL) this->SetCurrentCommand("none");
  // Send the command
  H5FDdsmDebug("Sending steering command " << this->CurrentCommand);
  this->Comm->RemoteCommSendSteeringCmd(this->CurrentCommand);
  if (strcmp(this->CurrentCommand, "pause")) this->SetCurrentCommand("none");
}
//----------------------------------------------------------------------------
void H5FDdsmSteerer::ReceiveSteeringCommands()
{
  // Receive commands
  H5FDdsmString cmd;
  this->Comm->RemoteCommRecvSteeringCmd(&cmd);
  std::string stringCommand = cmd;
  delete []cmd;
  H5FDdsmDebug("Received steering command: " << stringCommand);
  this->CheckCommand(stringCommand.c_str());
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::IsSteerable(const char *name) {

  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::CheckCommand(const char *command)
{
  std::string stringCommand = command;

  if (stringCommand == "none") {
    return H5FD_DSM_FAIL;
  }
  else if (stringCommand == "pause") {
    this->Pause = 1;
    while (this->Pause) {
      this->ReceiveSteeringCommands();
    }
  }
  else if (stringCommand == "play") {
    // nothing special
    this->Pause = 0;
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
