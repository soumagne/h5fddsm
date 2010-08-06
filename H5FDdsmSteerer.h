/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmSteerer.h

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

#ifndef H5FDDSMSTEERER_H
#define H5FDDSMSTEERER_H

void H5FD_dsm_begin_loop(const char *name);
void H5FD_dsm_end_loop(const char *name);

#include "H5FDdsmObject.h"
#include "H5FDdsmComm.h"

class H5FDdsm_EXPORT H5FDdsmSteerer : public H5FDdsmObject {

public:
  H5FDdsmSteerer();
  ~H5FDdsmSteerer();

  // Set/Get the current command
  H5FDdsmInt32 SetCurrentCommand(H5FDdsmConstString cmd);
  H5FDdsmGetStringMacro(CurrentCommand);

  // Set/Get WriteToDSM value
  // Allows the H5FDdsm driver to switch to the MPIO driver
  H5FDdsmSetValueMacro(WriteToDSM, H5FDdsmInt32);
  H5FDdsmGetValueMacro(WriteToDSM, H5FDdsmInt32);

  H5FDdsmSetValueMacro(Comm, H5FDdsmComm*);
  void SendSteeringCommands();
  void ReceiveSteeringCommands();

protected:
  H5FDdsmInt32 CheckCommand(const char *command);

  H5FDdsmComm  *Comm;
  H5FDdsmString CurrentCommand;
  volatile H5FDdsmInt32  Pause;
  H5FDdsmInt32  WriteToDSM;
};

#endif /* H5FDDSMSTEERER_H */
