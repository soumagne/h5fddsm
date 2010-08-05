/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmStorage.h

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

//----------------------------------------------------------------------------
void H5FD_dsm_begin_loop(const char *name)
{
  volatile int pause = 0;

  while (pause == 1) {
    // spin
    // look for new data coming in and new update of the value
  }
}
//----------------------------------------------------------------------------
void H5FD_dsm_end_loop(const char *name)
{
  // finish to build - close the HTM loop section
  // for later
}
//----------------------------------------------------------------------------
H5FDdsmSteerer::H5FDdsmSteerer()
{
  this->CurrentCommand = NULL;
  this->Comm = NULL;
}
//----------------------------------------------------------------------------
H5FDdsmSteerer::~H5FDdsmSteerer() {
  this->SetCurrentCommand(NULL);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::SetCurrentCommand(H5FDdsmConstString cmd)
{
  std::string oldCommand;
  if (this->CurrentCommand != NULL) oldCommand = this->CurrentCommand;
  if (this->CurrentCommand == cmd) { return(H5FD_DSM_SUCCESS); }
  if (this->CurrentCommand && cmd && strcmp(this->CurrentCommand, cmd) == 0 ) { return(H5FD_DSM_SUCCESS); }
  if ( this->CurrentCommand ) { delete [] this->CurrentCommand; this->CurrentCommand = 0; }
  if (cmd) {
    this->CurrentCommand = new char[strlen(cmd) + 1]; strcpy(this->CurrentCommand, cmd);
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
  std::cerr << "Sending steering command " << this->CurrentCommand << std::endl;
  this->Comm->RemoteCommSendSteeringCmd(this->CurrentCommand);
}
//----------------------------------------------------------------------------
void H5FDdsmSteerer::ReceiveSteeringCommands()
{
  // Receive commands
  H5FDdsmString cmd;
  this->Comm->RemoteCommRecvSteeringCmd(&cmd);
  std::string stringCommand = cmd;
  delete []cmd;
  std::cerr << "Received steering command: " << stringCommand << std::endl;
  //  if (this->Pause) {
  if (stringCommand == "pause") {
    this->ReceiveSteeringCommands();
  }
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::CheckCommand(const char *command)
{
  std::string stringCommand = command;
  H5FDdsmBoolean isSteerable;

  if (stringCommand == "pause") {
    isSteerable = true;
  }
  else {
    isSteerable = false;
  }

  return(H5FD_DSM_SUCCESS);
}
