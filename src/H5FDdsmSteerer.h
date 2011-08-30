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
#include <hdf5.h>

#include "H5FDdsmObject.h"

class H5FDdsmBuffer;

struct H5FDdsmSteererInternals;

class H5FDdsm_EXPORT H5FDdsmSteerer : public H5FDdsmObject {

public:
  H5FDdsmSteerer();
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

  H5FDdsmInt32 UpdateDisabledObjects();
  H5FDdsmInt32 GetDisabledObjects();
  H5FDdsmInt32 CheckCommand(H5FDdsmConstString command);

  // Use H5F_ACC_RDONLY    for queries
  // Use H5F_ACC_RDWR    for read/write
  H5FDdsmInt32   BeginInteractionsCache(unsigned int mode);
  H5FDdsmInt32   EndInteractionsCache();
  H5FDdsmBoolean InteractionsCacheActive();
  void           BeginHideHDF5Errors();
  void           EndHideHDF5Errors();

  H5FDdsmInt32 IsObjectEnabled(H5FDdsmConstString name);
  H5FDdsmInt32 IsObjectPresent(H5FDdsmConstString name, int &present);
  H5FDdsmInt32 GetScalar(H5FDdsmConstString name, hid_t memType, void *data);
  H5FDdsmInt32 SetScalar(H5FDdsmConstString name, hid_t memType, void *data);
  H5FDdsmInt32 GetVector(H5FDdsmConstString name, hid_t memType, hsize_t numberOfElements, void *data);
  H5FDdsmInt32 SetVector(H5FDdsmConstString name, hid_t memType, hsize_t numberOfElements, void *data);
  H5FDdsmInt32 GetHandle(H5FDdsmConstString name, hid_t *handle);
  H5FDdsmInt32 FreeHandle(hid_t handle);

  H5FDdsmInt32 DsmDump();

protected:
  friend class H5FDdsmManager;

  H5FDdsmInt32 WriteInteractions(H5FDdsmConstString name, hsize_t numberOfElements, int *data);
  H5FDdsmInt32 WriteInteractions(H5FDdsmConstString name, hsize_t numberOfElements, double *data);

  H5FDdsmInt32 ReadInteractions(H5FDdsmConstString name, hsize_t numberOfElements, int *data);
  H5FDdsmInt32 ReadInteractions(H5FDdsmConstString name, hsize_t numberOfElements, double *data);

  void SetDisabledObject(H5FDdsmConstString objectName);

  H5FDdsmBuffer           *DsmBuffer;
  H5FDdsmString            CurrentCommand;
  H5FDdsmInt32             WriteToDSM;
  H5FDdsmSteererInternals *SteererInternals;
  //
  hid_t        Cache_fapl;
  hid_t        Cache_fileId;
  hid_t        Cache_interactionGroupId;
  H5E_auto2_t  Cache_errfunc;
  void        *Cache_errdata;
};

#endif // __H5FDdsmSteerer_h
