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
#include <hdf5.h>
#include <H5FDdsm.h>
#include <string>
#include <map>

struct H5FDdsmSteererInternals {
  typedef std::map<std::string, bool> SteeringEntriesString;
  SteeringEntriesString               DisabledObjects;
  //
};
//----------------------------------------------------------------------------
H5FDdsmSteerer::H5FDdsmSteerer()
{
}
H5FDdsmSteerer::H5FDdsmSteerer(H5FDdsmBuffer *buffer)
{
  this->WriteToDSM         = 1;
  this->CurrentCommand     = NULL;
  this->SetCurrentCommand("none");
  this->DsmBuffer          = buffer;
  this->FileId             = H5I_BADID;
  this->InteractionGroupId = H5I_BADID;
  this->SteererInternals   = new H5FDdsmSteererInternals;
  this->QueryCache_fapl               = H5I_BADID;
  this->QueryCache_fileId             = H5I_BADID;
  this->QueryCache_interactionGroupId = H5I_BADID;
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
void H5FDdsmSteerer::SetDisabledObject(H5FDdsmConstString objectName)
{
  this->SteererInternals->DisabledObjects[objectName] =
      (this->SteererInternals->DisabledObjects[objectName]) ? false : true;
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::UpdateDisabledObjects()
{
  H5FDdsmInt64 addr;
  H5FDdsmMetaData metadata;
  int index = 0;

  for (H5FDdsmSteererInternals::SteeringEntriesString::iterator iter =
      this->SteererInternals->DisabledObjects.begin();
      iter != this->SteererInternals->DisabledObjects.end(); iter++) {
    if (iter->second) {
      strcpy(&metadata.disabled_objects.object_names[index*64], iter->first.c_str());
      index++;
    }
  }
  metadata.disabled_objects.number_of_objects = index;

  addr = this->DsmBuffer->GetTotalLength() - sizeof(metadata) + sizeof(metadata.entry)
      + sizeof(metadata.steering_cmd);
  // Only one of the processes writing to the DSM needs to write file metadata
  // but we must be careful that all the processes keep the metadata synchronized
  if (this->DsmBuffer->GetComm()->GetId() == 0) {
    if (this->DsmBuffer->Put(addr, sizeof(metadata.disabled_objects.number_of_objects),
        &metadata.disabled_objects.number_of_objects) != H5FD_DSM_SUCCESS) {
      return H5FD_DSM_FAIL;
    }
    if (this->DsmBuffer->Put(addr+sizeof(metadata.disabled_objects.number_of_objects),
        sizeof(metadata.disabled_objects.object_names),
        metadata.disabled_objects.object_names) != H5FD_DSM_SUCCESS) {
      return H5FD_DSM_FAIL;
    }
  }
  this->DsmBuffer->GetComm()->Barrier();
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::GetDisabledObjects()
{
  H5FDdsmInt64 addr;
  H5FDdsmMetaData metadata;
  MPI_Comm comm = MPI_COMM_NULL;

  addr = this->DsmBuffer->GetTotalLength() - sizeof(metadata) + sizeof(metadata.entry)
      + sizeof(metadata.steering_cmd);

  if (this->DsmBuffer->GetComm()->GetId() == 0) {
    if (this->DsmBuffer->Get(addr, sizeof(metadata.disabled_objects.number_of_objects),
        &metadata.disabled_objects.number_of_objects) != H5FD_DSM_SUCCESS) {
      return H5FD_DSM_FAIL;
    }
    if (this->DsmBuffer->Get(addr+sizeof(metadata.disabled_objects.number_of_objects),
        sizeof(metadata.disabled_objects.object_names),
        metadata.disabled_objects.object_names) != H5FD_DSM_SUCCESS) {
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
  MPI_Bcast(&metadata.disabled_objects.number_of_objects, sizeof(metadata.disabled_objects.number_of_objects), MPI_UNSIGNED_CHAR, 0, comm);
  MPI_Bcast(metadata.disabled_objects.object_names, sizeof(metadata.disabled_objects.object_names), MPI_UNSIGNED_CHAR, 0, comm);

  H5FDdsmDebug("Received nb of Disabled objects: " << metadata.disabled_objects.number_of_objects);

  this->SteererInternals->DisabledObjects.clear();
  for (int i = 0; i < metadata.disabled_objects.number_of_objects; i++) {
    char tmp[64];
    strcpy(tmp, &metadata.disabled_objects.object_names[i*64]);
    this->SetDisabledObject(tmp);
    H5FDdsmDebug("Received disabled object: " << tmp);
  }
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::IsObjectEnabled(H5FDdsmConstString name)
{
  H5FDdsmInt32 ret = H5FD_DSM_FAIL;
  // if the object is found and set to true, it has been disabled in the GUI
  if (!this->SteererInternals->DisabledObjects[name]) {
    ret = H5FD_DSM_SUCCESS;
  }
  return(ret);
}
//----------------------------------------------------------------------------
bool H5FDdsmSteerer::QueryActive()
{
  return (this->QueryCache_fapl!=H5I_BADID);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::BeginQuery()
{
  if (this->QueryActive()) return H5FD_DSM_SUCCESS;
  //
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  this->BeginHideHDF5Errors();
  //
  this->QueryCache_fapl = H5Pcreate(H5P_FILE_ACCESS);
  H5Pset_fapl_dsm(this->QueryCache_fapl, MPI_COMM_WORLD, this->DsmBuffer);
  this->QueryCache_fileId = H5Fopen("dsm", H5F_ACC_RDONLY, this->QueryCache_fapl);
  if (this->QueryCache_fileId < 0) {
    ret = H5FD_DSM_FAIL;
  } else {
    this->QueryCache_interactionGroupId = H5Gopen(this->QueryCache_fileId, "Interactions", H5P_DEFAULT);
    if (this->QueryCache_interactionGroupId < 0) {
      ret = H5FD_DSM_FAIL;
    } 
  }
  this->EndHideHDF5Errors();

  return(ret);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::EndQuery()
{
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  //
  if (!this->QueryActive()) return ret;
  //
  if (H5Pclose(this->QueryCache_fapl)<0) ret = H5FD_DSM_FAIL;
  if (H5Gclose(this->QueryCache_interactionGroupId)<0) ret = H5FD_DSM_FAIL;
  if (H5Fclose(this->QueryCache_fileId)<0) ret = H5FD_DSM_FAIL;
  //
  this->QueryCache_fapl               = H5I_BADID;
  this->QueryCache_fileId             = H5I_BADID;
  this->QueryCache_interactionGroupId = H5I_BADID;
  return (ret);
}
//----------------------------------------------------------------------------
void H5FDdsmSteerer::BeginHideHDF5Errors()
{
  // Prevent HDF5 to print out handled errors, first save old error handler
  H5Eget_auto(H5E_DEFAULT, &this->QueryCache_errfunc, &this->QueryCache_errdata);
  // Turn off error handling
  H5Eset_auto(H5E_DEFAULT, NULL, NULL);
}
//----------------------------------------------------------------------------
void H5FDdsmSteerer::EndHideHDF5Errors()
{
  // Restore previous error handler
  H5Eset_auto(H5E_DEFAULT, this->QueryCache_errfunc, this->QueryCache_errdata);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::IsObjectPresent(H5FDdsmConstString name, int &present)
{
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  bool usecache = this->QueryActive();
  if (!usecache) {
    if (this->BeginQuery()!=H5FD_DSM_SUCCESS) return H5FD_DSM_FAIL;
  }
  this->BeginHideHDF5Errors();
  //
  hid_t datasetId = H5Dopen(this->QueryCache_interactionGroupId, name, H5P_DEFAULT);
  if (datasetId < 0) {
    present = 0;
    ret = H5FD_DSM_FAIL;
  } else {
    present = 1;
  }
  H5Dclose(datasetId);
  
  // Clean up
  if (!usecache) {
    if (this->EndQuery()!=H5FD_DSM_SUCCESS) ret = H5FD_DSM_FAIL;
  }
  this->EndHideHDF5Errors();
  return(ret);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::GetScalar(H5FDdsmConstString name, H5FDdsmInt32 memType, void *data)
{
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  bool usecache = this->QueryActive();
  if (!usecache) {
    if (this->BeginQuery()!=H5FD_DSM_SUCCESS) return H5FD_DSM_FAIL;
  }
  this->BeginHideHDF5Errors();

  hid_t attributeId = H5Aopen(this->QueryCache_interactionGroupId, name, H5P_DEFAULT);
  if (attributeId < 0) {
    ret = H5FD_DSM_FAIL;
  } else {
    if (H5Aread(attributeId, memType, data) < 0) {
      H5Eprint(H5E_DEFAULT, stderr);
      ret = H5FD_DSM_FAIL;
    }
  }
  // Clean up
  if (!usecache) {
    if (this->EndQuery()!=H5FD_DSM_SUCCESS) ret = H5FD_DSM_FAIL;
  }
  this->EndHideHDF5Errors();
  return(ret);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::GetVector(H5FDdsmConstString name, H5FDdsmInt32 memType, H5FDdsmInt32 numberOfElements, void *data)
{
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  bool usecache = this->QueryActive();
  if (!usecache) {
    if (this->BeginQuery()!=H5FD_DSM_SUCCESS) return H5FD_DSM_FAIL;
  }
  this->BeginHideHDF5Errors();

  hsize_t arraySize = numberOfElements;
  hid_t  memspaceId = H5Screate_simple(1, &arraySize, NULL);
  hid_t   datasetId = H5Dopen(this->QueryCache_interactionGroupId, name, H5P_DEFAULT);
  if (datasetId < 0) {
    ret = H5FD_DSM_FAIL;
  } else {
    if (H5Dread(datasetId, memType, memspaceId, H5S_ALL, H5P_DEFAULT, data) < 0) {
      H5Eprint(H5E_DEFAULT, stderr);
      ret = H5FD_DSM_FAIL;
    }
  }
  H5Dclose(datasetId);
  H5Sclose(memspaceId);

  // Clean up
  if (!usecache) {
    if (this->EndQuery()!=H5FD_DSM_SUCCESS) ret = H5FD_DSM_FAIL;
  }
  this->EndHideHDF5Errors();
  return(ret);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::GetHandle(H5FDdsmConstString name, hid_t *handle)
{
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  bool usecache = this->QueryActive();
  if (!usecache) {
    H5FDdsmError("This function cannot be used without first calling H5FD_dsm_steering_begin_query()");
    return H5FD_DSM_FAIL;
  }

  *handle = H5Dopen(this->QueryCache_interactionGroupId, name, H5P_DEFAULT);
  if (*handle < 0) {
    ret = H5FD_DSM_FAIL;
  } 
  return(ret);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::FreeHandle(hid_t handle)
{
  H5FDdsmInt32 ret = H5FD_DSM_SUCCESS;
  bool usecache = this->QueryActive();
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
H5FDdsmInt32 H5FDdsmSteerer::DsmDump()
{
  H5FDdsmDump *dsmDump = new H5FDdsmDump();
  dsmDump->SetDsmBuffer(this->DsmBuffer);
  dsmDump->SetFileName("DSM.h5");
  dsmDump->DumpLight();
  delete dsmDump;
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::CreateInteractionGroup()
{
  // Open DSM in RDWR
  hid_t fapl = H5Pcreate(H5P_FILE_ACCESS);
  H5Pset_fapl_dsm(fapl, MPI_COMM_WORLD, this->DsmBuffer);
  this->FileId = H5Fopen("dsm", H5F_ACC_RDWR, fapl);
  H5Pclose(fapl);
  this->InteractionGroupId = H5Gcreate(this->FileId, "Interactions", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  if (this->InteractionGroupId != H5I_BADID) {
    return(H5FD_DSM_SUCCESS);
  } else {
    return(H5FD_DSM_FAIL);
  }
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::WriteInteractions(H5FDdsmConstString name, H5FDdsmInt32 numberOfElements, int *data)
{
  if (numberOfElements > 1) {
    hsize_t arraySize = numberOfElements;
    hid_t memspace = H5Screate_simple(1, &arraySize, NULL);
    hid_t dataset = H5Dcreate(this->InteractionGroupId, name, H5T_NATIVE_INT, memspace,
        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (H5Dwrite(dataset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, data) < 0) {
      return(H5FD_DSM_FAIL);
    }
    H5Sclose(memspace);
    H5Dclose(dataset);
  } else {
    if (numberOfElements) {
      hid_t memspace = H5Screate(H5S_SCALAR);
      hid_t attribute = H5Acreate(this->InteractionGroupId, name, H5T_NATIVE_INT,
          memspace, H5P_DEFAULT, H5P_DEFAULT);
      if (this->DsmBuffer->GetComm()->GetId() == 0) {
        if (H5Awrite(attribute, H5T_NATIVE_INT, data) < 0) {
          return(H5FD_DSM_FAIL);
        }
      }
      H5Aclose(attribute);
      H5Sclose(memspace);
    }
  }
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::WriteInteractions(H5FDdsmConstString name, H5FDdsmInt32 numberOfElements, double *data)
{
  if (numberOfElements > 1) {
    hsize_t arraySize = numberOfElements;
    hid_t memspace = H5Screate_simple(1, &arraySize, NULL);
    hid_t dataset = H5Dcreate(this->InteractionGroupId, name, H5T_NATIVE_DOUBLE, memspace,
        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (H5Dwrite(dataset, H5T_NATIVE_DOUBLE, memspace, H5S_ALL, H5P_DEFAULT, data) < 0) {
      return(H5FD_DSM_FAIL);
    }
    H5Sclose(memspace);
    H5Dclose(dataset);
  } else {
    if (numberOfElements) {
      hid_t memspace = H5Screate(H5S_SCALAR);
      hid_t attribute = H5Acreate(this->InteractionGroupId, name, H5T_NATIVE_DOUBLE,
          memspace, H5P_DEFAULT, H5P_DEFAULT);
      if (this->DsmBuffer->GetComm()->GetId() == 0) {
        if (H5Awrite(attribute, H5T_NATIVE_DOUBLE, data) < 0) {
          return(H5FD_DSM_FAIL);
        }
      }
      H5Aclose(attribute);
      H5Sclose(memspace);
    }
  }
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::CloseInteractionGroup()
{
  H5Gclose(this->InteractionGroupId);
  H5Fclose(this->FileId);
  this->FileId = H5I_BADID;
  this->InteractionGroupId = H5I_BADID;
  return(H5FD_DSM_SUCCESS);
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmSteerer::CheckCommand(H5FDdsmConstString command)
{
  std::string stringCommand = command;

  if (stringCommand == "pause") {
    H5FDdsmDebug("Receiving ready...");
    this->DsmBuffer->GetComm()->RemoteCommRecvReady();
    H5FDdsmDebug("Ready received");
    return(H5FD_DSM_SUCCESS);
  }
  else if (stringCommand == "play") {
    // nothing special
    return(H5FD_DSM_SUCCESS);
  }
  else if (stringCommand == "restart") {
    return(H5FD_DSM_SUCCESS);
  }
  else if (stringCommand == "notSend") {
    return(H5FD_DSM_SUCCESS);
  }
  else if (stringCommand == "disk") {
    H5FDdsmDebug("Setting WriteToDSM to 0");
    this->WriteToDSM = 0;
    return(H5FD_DSM_SUCCESS);
  }
  else if (stringCommand == "dsm") {
    H5FDdsmDebug("Setting WriteToDSM to 1");
    this->WriteToDSM = 1;
    return(H5FD_DSM_SUCCESS);
  }
  return(H5FD_DSM_FAIL);
}
