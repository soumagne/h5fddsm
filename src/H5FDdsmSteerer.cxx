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
#include "H5FDdsmManager.h"
//
#include <hdf5.h>
#include <H5FDdsm.h>
#include <string>
#include <map>

//----------------------------------------------------------------------------
// Declare extra debug info 
#undef H5FDdsmDebugLevel
#ifdef H5FDdsm_DEBUG_GLOBAL
#define H5FDdsmDebugLevel(level, x) \
{ if (this->DebugLevel >= level) { \
  std::cout << "H5FD_DSM Debug Level " << level << ": " << (this->DsmManager->GetIsServer() ? "Server " : "Client ") << (this->DsmManager->DsmComm ? this->DsmManager->DsmComm->GetId() : -1) << " : " << x << std::endl; \
  } \
}
#else
#define H5FDdsmDebugLevel(level, x) \
{ if (this->Debug && this->DebugLevel >= level) { \
  std::cout << "H5FD_DSM Debug Level " << level << ": " << (this->DsmManager->GetIsServer() ? "Server " : "Client ") << (this->DsmManager->DsmComm ? this->DsmManager->DsmComm->GetId() : -1) << " : " << x << std::endl; \
  } \
}
#endif
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

struct H5FDdsmSteererInternals {
  typedef std::map<std::string, H5FDdsmBoolean> SteeringEntriesString;
  SteeringEntriesString DisabledObjects;
  //
};

//----------------------------------------------------------------------------
H5FDdsmSteerer::H5FDdsmSteerer()
{
}

//----------------------------------------------------------------------------
H5FDdsmSteerer::H5FDdsmSteerer(H5FDdsmManager *manager)
{
  this->WriteToDSM                = 1;
  this->CurrentCommand            = NULL;
  this->DsmManager                = manager;
  this->SteererInternals          = new H5FDdsmSteererInternals;
  this->Cache_interactionGroupId  = H5I_BADID;
  this->SetCurrentCommand("none");
}

//----------------------------------------------------------------------------
H5FDdsmSteerer::~H5FDdsmSteerer()
{
  this->SetCurrentCommand(NULL);
  delete this->SteererInternals;
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::SetCurrentCommand(H5FDdsmConstString cmd)
{
  H5FDdsmDebugLevel(1,"SetCurrentCommand to : " << (cmd ? cmd : "NULL"));
  if (this->CurrentCommand == cmd) { return(H5FD_DSM_SUCCESS); }
  if (this->CurrentCommand && cmd && !strcmp(this->CurrentCommand, cmd)) { return(H5FD_DSM_SUCCESS); }
  if (this->CurrentCommand) { delete [] this->CurrentCommand; this->CurrentCommand = NULL; }
  if (cmd) {
    this->CurrentCommand = new char[strlen(cmd) + 1];
    strcpy(this->CurrentCommand, cmd);
    if (strcmp(this->CurrentCommand, "play") == 0) {
      H5FDdsmDebugLevel(1,"Sending ready...");
      if (this->DsmManager->GetUpdatePiece() == 0) {
//        this->DsmManager->GetDsmBuffer()->SendAcknowledgment(0, H5FD_DSM_INTER_COMM, -1, "SetCurrentCommand");
      }
      H5FDdsmDebugLevel(1,"Ready sent");
    }
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::UpdateSteeringCommands()
{
  H5FDdsmAddr addr;
  H5FDdsmMetaData metadata;

  H5FDdsmDebugLevel(1,"Sending steering command " << this->CurrentCommand);
  memset(&metadata, 0, sizeof(metadata));
  strcpy(metadata.steering_cmd, (H5FDdsmConstString)this->CurrentCommand);
  addr = (H5FDdsmAddr) (this->DsmManager->GetDsmBuffer()->GetTotalLength() -
      sizeof(metadata) + sizeof(metadata.entry));
  // Only one of the processes writing to the DSM needs to write file metadata
  // but we must be careful that all the processes keep the metadata synchronized
  if (this->DsmManager->GetUpdatePiece() == 0) {
    if (this->DsmManager->GetDsmBuffer()->Put(addr, sizeof(metadata.steering_cmd),
        metadata.steering_cmd) != H5FD_DSM_SUCCESS) return H5FD_DSM_FAIL;
  }
  this->DsmManager->GetDsmBuffer()->GetComm()->Barrier();
  this->SetCurrentCommand("none");
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::GetSteeringCommands()
{
  H5FDdsmAddr addr;
  H5FDdsmMetaData metadata;

  memset(&metadata, 0, sizeof(metadata));
  addr = (H5FDdsmAddr) (this->DsmManager->GetDsmBuffer()->GetTotalLength() -
      sizeof(metadata) + sizeof(metadata.entry));

  if (this->DsmManager->GetUpdatePiece() == 0) {
    if (this->DsmManager->GetDsmBuffer()->Get(addr, sizeof(metadata.steering_cmd),
        metadata.steering_cmd) != H5FD_DSM_SUCCESS) {
      H5FDdsmDebugLevel(1,"DsmGetSteeringCmd failed");
      return H5FD_DSM_FAIL;
    }
  }
  
  MPI_Bcast(metadata.steering_cmd, sizeof(metadata.steering_cmd),
      MPI_UNSIGNED_CHAR, 0, this->DsmManager->GetMpiComm());
  H5FDdsmDebugLevel(1,"Received steering command: " << metadata.steering_cmd);
  if (this->CheckCommand((H5FDdsmConstString) metadata.steering_cmd) == H5FD_DSM_SUCCESS) {
    // Steering command successfully treated, clear it
    this->UpdateSteeringCommands();
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
void H5FDdsmSteerer::SetDisabledObject(H5FDdsmConstString objectName)
{
  this->SteererInternals->DisabledObjects[objectName] =
      (this->SteererInternals->DisabledObjects[objectName]) ? H5FD_DSM_FALSE : H5FD_DSM_TRUE;
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::UpdateDisabledObjects()
{
  H5FDdsmAddr addr;
  H5FDdsmMetaData metadata;
  static H5FDdsmInt32 prev_index = 0;
  H5FDdsmInt32 index = 0;

  memset(&metadata, 0, sizeof(metadata));
  for (H5FDdsmSteererInternals::SteeringEntriesString::iterator iter =
      this->SteererInternals->DisabledObjects.begin();
      iter != this->SteererInternals->DisabledObjects.end(); iter++) {
    if (iter->second) {
      strcpy(&metadata.disabled_objects.object_names[index*64], iter->first.c_str());
      index++;
    }
  }
  metadata.disabled_objects.number_of_objects = index;

  if (index != prev_index) {
    addr = (H5FDdsmAddr) (this->DsmManager->GetDsmBuffer()->GetTotalLength() -
        sizeof(metadata) + sizeof(metadata.entry) + sizeof(metadata.steering_cmd));
    // Only one of the processes writing to the DSM needs to write file metadata
    // but we must be careful that all the processes keep the metadata synchronized
    if (this->DsmManager->GetUpdatePiece() == 0) {
      if (this->DsmManager->GetDsmBuffer()->Put(addr, sizeof(metadata.disabled_objects.number_of_objects),
          &metadata.disabled_objects.number_of_objects) != H5FD_DSM_SUCCESS) {
        return H5FD_DSM_FAIL;
      }
      if (this->DsmManager->GetDsmBuffer()->Put(addr +
          sizeof(metadata.disabled_objects.number_of_objects),
          sizeof(metadata.disabled_objects.object_names),
          metadata.disabled_objects.object_names) != H5FD_DSM_SUCCESS) {
        return H5FD_DSM_FAIL;
      }
    }
    this->DsmManager->GetDsmBuffer()->GetComm()->Barrier();
  }
  prev_index = index;
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::GetDisabledObjects()
{
  H5FDdsmAddr addr;
  H5FDdsmMetaData metadata;

  memset(&metadata, 0, sizeof(metadata));
  addr = (H5FDdsmAddr) (this->DsmManager->GetDsmBuffer()->GetTotalLength() -
      sizeof(metadata) + sizeof(metadata.entry) + sizeof(metadata.steering_cmd));

  if (this->DsmManager->GetUpdatePiece() == 0) {
    if (this->DsmManager->GetDsmBuffer()->Get(addr,
        sizeof(metadata.disabled_objects.number_of_objects),
        &metadata.disabled_objects.number_of_objects) != H5FD_DSM_SUCCESS) {
      return H5FD_DSM_FAIL;
    }
    if (this->DsmManager->GetDsmBuffer()->Get(addr +
        sizeof(metadata.disabled_objects.number_of_objects),
        sizeof(metadata.disabled_objects.object_names),
        metadata.disabled_objects.object_names) != H5FD_DSM_SUCCESS) {
      return H5FD_DSM_FAIL;
    }
  }

  MPI_Bcast(&metadata.disabled_objects.number_of_objects,
      sizeof(metadata.disabled_objects.number_of_objects),
      MPI_UNSIGNED_CHAR, 0, this->DsmManager->GetMpiComm());
  MPI_Bcast(metadata.disabled_objects.object_names,
      sizeof(metadata.disabled_objects.object_names),
      MPI_UNSIGNED_CHAR, 0, this->DsmManager->GetMpiComm());

  H5FDdsmDebugLevel(1,"Received nb of Disabled objects: " << metadata.disabled_objects.number_of_objects);

  this->SteererInternals->DisabledObjects.clear();
  for (int i = 0; i < metadata.disabled_objects.number_of_objects; i++) {
    char tmp[64];
    strcpy(tmp, &metadata.disabled_objects.object_names[i*64]);
    this->SetDisabledObject(tmp);
    H5FDdsmDebugLevel(1,"Received disabled object: " << tmp);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmBoolean H5FDdsmSteerer::IsObjectEnabled(H5FDdsmConstString name)
{
  H5FDdsmInt32 ret = H5FD_DSM_FALSE;
  // if the object is found and set to true, it has been disabled in the GUI
  if (!this->SteererInternals->DisabledObjects[name]) {
    ret = H5FD_DSM_TRUE;
  }
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmBoolean H5FDdsmSteerer::InteractionsCacheActive()
{
  return (this->Cache_interactionGroupId != H5I_BADID);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::BeginInteractionsCache(H5FDdsmUInt32 mode)
{
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  // DSM handles caching of file and fapl handles for us.
  // Don't change the serial/parallel mode 
  if (this->DsmManager->IsOpenDSM()) {
    this->DsmManager->OpenDSM(mode, this->DsmManager->GetIsDriverSerial());
  }
  else {
    this->DsmManager->OpenDSM(mode);
  }
  //
  if (!this->InteractionsCacheActive()) {
    hid_t fileId = this->DsmManager->GetCachedFileHandle();
    if (mode == H5F_ACC_RDONLY) {
      this->BeginHideHDF5Errors();
      this->Cache_interactionGroupId = H5Gopen(fileId, "Interactions", H5P_DEFAULT);
      this->EndHideHDF5Errors();
    }
    else if (mode == H5F_ACC_RDWR) {
      // if it does not already exist, create it
      this->BeginHideHDF5Errors();
      int exists = H5Lexists(fileId, "Interactions", H5P_DEFAULT);
      this->EndHideHDF5Errors();
      if (exists<=0) {
        this->Cache_interactionGroupId = H5Gcreate(fileId, "Interactions", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
      }
      else {
        this->Cache_interactionGroupId = H5Gopen(fileId, "Interactions", H5P_DEFAULT);
      }
    }
  }
  if (this->Cache_interactionGroupId < 0) {
    ret = H5FD_DSM_FAIL;
  }
  else {
    if (!this->Cache_Stack.empty() && this->Cache_Stack.top()!=mode) {
//      std::cout << "info : requested Interactions cache mode open " << this->DsmManager->OpenModeString(mode) 
//        << " but already open in mode " << this->DsmManager->OpenModeString(this->Cache_Stack.top()) << std::endl;
      if ((mode==H5F_ACC_RDWR) && (this->Cache_Stack.top()==H5F_ACC_RDWR)) {
        H5FDdsmError("Bad DSM open : switching from readonly to read/write mode not allowed");
        return H5FD_DSM_FAIL;
      }
    }
    this->Cache_Stack.push(mode);
  }
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::EndInteractionsCache()
{
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  //
  if (this->InteractionsCacheActive()) {        
    this->Cache_Stack.pop();
    if (this->Cache_Stack.empty()) {
      if (H5Gclose(this->Cache_interactionGroupId)<0) {
        ret = H5FD_DSM_FAIL;
      }
      this->Cache_interactionGroupId = H5I_BADID;
    }
  }
  this->DsmManager->CloseDSM();
  //
  return (ret);
}

//----------------------------------------------------------------------------
void H5FDdsmSteerer::BeginHideHDF5Errors()
{
  // Prevent HDF5 to print out handled errors, first save old error handler
  H5Eget_auto(H5E_DEFAULT, &this->Cache_errfunc, &this->Cache_errdata);
  // Turn off error handling
  H5Eset_auto(H5E_DEFAULT, NULL, NULL);
}

//----------------------------------------------------------------------------
void H5FDdsmSteerer::EndHideHDF5Errors()
{
  // Restore previous error handler
  H5Eset_auto(H5E_DEFAULT, this->Cache_errfunc, this->Cache_errdata);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::IsObjectPresent(H5FDdsmConstString name, H5FDdsmBoolean &present)
{
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  if (this->BeginInteractionsCache(H5F_ACC_RDONLY) != H5FD_DSM_SUCCESS) return H5FD_DSM_FAIL;

  this->BeginHideHDF5Errors();

  // Try attribute first
  hid_t attributeId = H5Aopen(this->Cache_interactionGroupId, name, H5P_DEFAULT);
  if (attributeId < 0) {
    // Try dataset if attribute failed
    hid_t datasetId = H5Dopen(this->Cache_interactionGroupId, name, H5P_DEFAULT);
    if (datasetId < 0) {
      present = H5FD_DSM_FALSE;
      // TODO should not return fail here
      // ret = H5FD_DSM_FAIL;
    } else {
      present = H5FD_DSM_TRUE;
      H5Dclose(datasetId);
    }
  } else {
    present = H5FD_DSM_TRUE;
    H5Aclose(attributeId);
  }
  this->EndHideHDF5Errors();
  // Clean up
  if (this->EndInteractionsCache() != H5FD_DSM_SUCCESS) ret = H5FD_DSM_FAIL;
  return(ret);
}

//----------------------------------------------------------------------------
/*
H5FDdsmInt32 H5FDdsmSteerer::GetScalar(H5FDdsmConstString name, H5FDdsmInt32 memType, void *data)
{
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  H5FDdsmBoolean usecache = this->InteractionsCacheActive();
  if (!usecache) {
    if (this->BeginInteractionsCache(H5F_ACC_RDONLY) != H5FD_DSM_SUCCESS) return H5FD_DSM_FAIL;
  }
  this->BeginHideHDF5Errors();

  hid_t attributeId = H5Aopen(this->Cache_interactionGroupId, name, H5P_DEFAULT);
  if (attributeId < 0) {
    ret = H5FD_DSM_FAIL;
  } else {
    if (H5Aread(attributeId, memType, data) < 0) {
      H5Eprint(H5E_DEFAULT, stderr);
      ret = H5FD_DSM_FAIL;
    }
    H5Aclose(attributeId);
  }
  // Clean up
  if (!usecache) {
    if (this->EndInteractionsCache() != H5FD_DSM_SUCCESS) ret = H5FD_DSM_FAIL;
  }
  this->EndHideHDF5Errors();
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::SetScalar(H5FDdsmConstString name, H5FDdsmInt32 memType, void *data)
{
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  H5FDdsmBoolean usecache = this->InteractionsCacheActive();
  if (!usecache) {
    if (this->BeginInteractionsCache(H5F_ACC_RDWR) != H5FD_DSM_SUCCESS) return H5FD_DSM_FAIL;
  }
  // we don't hide errors when writing, we need to know if something has failed

  hid_t memspaceId = H5Screate(H5S_SCALAR);
  hid_t attributeId;
  if (H5Aexists(this->Cache_interactionGroupId, name) > 0) {
    attributeId = H5Aopen(this->Cache_interactionGroupId, name, H5P_DEFAULT);
  }
  else {
    attributeId = H5Acreate(this->Cache_interactionGroupId, name, memType,
        memspaceId, H5P_DEFAULT, H5P_DEFAULT);
  }
  if (attributeId < 0) {
    ret = H5FD_DSM_FAIL;
  } else {
    // Must be collective
    if (H5Awrite(attributeId, memType, data) < 0) {
      ret = H5FD_DSM_FAIL;
    }
    H5Aclose(attributeId);
  }
  H5Sclose(memspaceId);
  // Clean up
  if (!usecache) {
    if (this->EndInteractionsCache() != H5FD_DSM_SUCCESS) ret = H5FD_DSM_FAIL;
  }
  return(ret);
}
*/
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::GetVector(H5FDdsmConstString name, H5FDdsmInt32 memType, hsize_t numberOfElements, void *data)
{
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  if (this->BeginInteractionsCache(H5F_ACC_RDONLY) != H5FD_DSM_SUCCESS) return H5FD_DSM_FAIL;

  this->BeginHideHDF5Errors();

  hsize_t arraySize = numberOfElements;
  hid_t  memspaceId = H5Screate_simple(1, &arraySize, NULL);
  hid_t   datasetId = H5Dopen(this->Cache_interactionGroupId, name, H5P_DEFAULT);
  if (datasetId < 0) {
    ret = H5FD_DSM_FAIL;
  } else {
    if (H5Dread(datasetId, memType, memspaceId, H5S_ALL, H5P_DEFAULT, data) < 0) {
      H5Eprint(H5E_DEFAULT, stderr);
      ret = H5FD_DSM_FAIL;
    }
    H5Dclose(datasetId);
  }
  H5Sclose(memspaceId);
  this->EndHideHDF5Errors();
  // Clean up
  if (this->EndInteractionsCache() != H5FD_DSM_SUCCESS) ret = H5FD_DSM_FAIL;
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::SetVector(H5FDdsmConstString name, H5FDdsmInt32 memType, hsize_t numberOfElements, void *data)
{
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  if (this->BeginInteractionsCache(H5F_ACC_RDWR) != H5FD_DSM_SUCCESS) return H5FD_DSM_FAIL;

  // we don't hide errors when writing, we need to know if something has failed

  hsize_t arraySize = numberOfElements;
  hid_t memspaceId = H5Screate_simple(1, &arraySize, NULL);
  hid_t datasetId = -1;
  this->BeginHideHDF5Errors();
  int exists = H5Lexists(this->Cache_interactionGroupId, name, H5P_DEFAULT);
  this->EndHideHDF5Errors();
  if (exists<=0) {
    datasetId = H5Dcreate(this->Cache_interactionGroupId, name, memType, memspaceId,
      H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  }
  else {
    datasetId = H5Dopen(this->Cache_interactionGroupId, name, H5P_DEFAULT);
  }
  if (datasetId < 0) {
    ret = H5FD_DSM_FAIL;
  } else {
    if ((this->DsmManager->GetUpdatePiece() == 0) && H5Dwrite(datasetId, memType,
        H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0) {
      ret = H5FD_DSM_FAIL;
    }
    H5Dclose(datasetId);
  }
  H5Sclose(memspaceId);
  // Clean up
  if (this->EndInteractionsCache() != H5FD_DSM_SUCCESS) ret = H5FD_DSM_FAIL;
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::GetHandle(H5FDdsmConstString name, hid_t *handle)
{
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  H5FDdsmBoolean usecache = this->InteractionsCacheActive();
  if (!usecache) {
    H5FDdsmError("This function cannot be used without first calling H5FD_dsm_steering_begin_query()");
    return H5FD_DSM_FAIL;
  }

  *handle = H5Dopen(this->Cache_interactionGroupId, name, H5P_DEFAULT);
  if (*handle < 0) {
    ret = H5FD_DSM_FAIL;
  } 
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::FreeHandle(hid_t handle)
{
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  H5FDdsmBoolean usecache = this->InteractionsCacheActive();
  if (!usecache) {
    H5FDdsmError("This function cannot be used without first calling H5FD_dsm_steering_begin_query()");
    return H5FD_DSM_FAIL;
  }

  if (H5Dclose(handle) < 0) {
    ret = H5FD_DSM_FAIL;
  } 
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::WriteInteractions(H5FDdsmConstString name, hsize_t numberOfElements, int *data)
{
  H5FDdsmInt32 ret = H5FD_DSM_FAIL;

  if (numberOfElements) {
    ret = this->SetVector(name, H5T_NATIVE_INT, numberOfElements, data);
  }
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::WriteInteractions(H5FDdsmConstString name, hsize_t numberOfElements, double *data)
{
  H5FDdsmInt32 ret = H5FD_DSM_FAIL;

  if (numberOfElements) {
    ret = this->SetVector(name, H5T_NATIVE_DOUBLE, numberOfElements, data);
  }
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::ReadInteractions(H5FDdsmConstString name, hsize_t numberOfElements, int *data)
{
  H5FDdsmInt32 ret = H5FD_DSM_FAIL;

  if (numberOfElements) {
    ret = this->GetVector(name, H5T_NATIVE_INT, numberOfElements, data);
  }
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::ReadInteractions(H5FDdsmConstString name, hsize_t numberOfElements, double *data)
{
  H5FDdsmInt32 ret = H5FD_DSM_FAIL;

  if (numberOfElements) {
    ret = this->GetVector(name, H5T_NATIVE_DOUBLE, numberOfElements, data);
  }
  return(ret);
}

//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::CheckCommand(H5FDdsmConstString command)
{
  H5FDdsmInt32 ret = H5FD_DSM_FAIL;
  std::string stringCommand = command;
  
  if (stringCommand == "pause") {
    this->DsmManager->GetDsmBuffer()->RequestLockAcquire();
    this->DsmManager->GetDsmBuffer()->SetUnlockStatus(H5FD_DSM_NOTIFY_WAIT);
    this->DsmManager->GetDsmBuffer()->RequestLockRelease();

    H5FDdsmDebug("Receiving ready...");
    if (this->DsmManager->GetUpdatePiece() == 0) {
      H5FDdsmInt32 unused;
//      this->DsmManager->GetDsmBuffer()->ReceiveAcknowledgment(0, H5FD_DSM_INTER_COMM, unused, "Ready");
    }
    this->DsmManager->GetDsmBuffer()->GetComm()->Barrier();
    H5FDdsmDebug("Ready received");

    ret = H5FD_DSM_SUCCESS;
  }
  else if (stringCommand == "play") {
    // nothing special
    ret = H5FD_DSM_SUCCESS;
  }
  else if (stringCommand == "disk") {
    H5FDdsmDebugLevel(1,"Setting WriteToDSM to 0");
    this->WriteToDSM = 0;
    ret = H5FD_DSM_SUCCESS;
  }
  else if (stringCommand == "dsm") {
    H5FDdsmDebugLevel(1,"Setting WriteToDSM to 1");
    this->WriteToDSM = 1;
    ret = H5FD_DSM_SUCCESS;
  }
  return(ret);
}
