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

#ifndef __H5FDdsmSteerer_h
#define __H5FDdsmSteerer_h

#include <mpi.h>

#include "H5FDdsmObject.h"

class H5FDdsmBuffer;

class H5FDdsm_EXPORT H5FDdsmSteerer : public H5FDdsmObject {

public:
  H5FDdsmSteerer(H5FDdsmBuffer *);
  ~H5FDdsmSteerer();

  // Set/Get the current command
  H5FDdsmInt32 SetCurrentCommand(H5FDdsmConstString cmd);
  H5FDdsmGetStringMacro(CurrentCommand);

  // Set/Get WriteToDSM value
  // Allows the H5FDdsm driver to switch to the MPIO driver
  H5FDdsmSetValueMacro(WriteToDSM, H5FDdsmInt32);
  H5FDdsmGetValueMacro(WriteToDSM, H5FDdsmInt32);

  H5FDdsmInt32 UpdateSteeringCommands();
  H5FDdsmInt32 GetSteeringCommands();

  H5FDdsmInt32 IsSteerable(H5FDdsmConstString parentName, H5FDdsmConstString name);

protected:
  H5FDdsmInt32 CheckCommand(H5FDdsmConstString command);

  H5FDdsmBuffer  *DsmBuffer;
  H5FDdsmString CurrentCommand;
  H5FDdsmInt32  WriteToDSM;
  std::string   *SteerableObjects;
};

#endif // __H5FDdsmSteerer_h
