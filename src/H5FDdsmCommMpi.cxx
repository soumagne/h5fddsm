/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmCommMpi.cxx

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

/*=========================================================================
  This code is derived from an earlier work and is distributed
  with permission from, and thanks to ...
=========================================================================*/

/*******************************************************************/
/*                               XDMF                              */
/*                   eXtensible Data Model and Format              */
/*                                                                 */
/*                                                                 */
/*  Author:                                                        */
/*     Jerry A. Clarke                                             */
/*     clarke@arl.army.mil                                         */
/*     US Army Research Laboratory                                 */
/*     Aberdeen Proving Ground, MD                                 */
/*                                                                 */
/*     Copyright @ 2007 US Army Research Laboratory                */
/*     All Rights Reserved                                         */
/*     See Copyright.txt or http://www.arl.hpc.mil/ice for details */
/*                                                                 */
/*     This software is distributed WITHOUT ANY WARRANTY; without  */
/*     even the implied warranty of MERCHANTABILITY or FITNESS     */
/*     FOR A PARTICULAR PURPOSE.  See the above copyright notice   */
/*     for more information.                                       */
/*                                                                 */
/*******************************************************************/
#include "H5FDdsmCommMpi.h"
#include "H5FDdsmMsg.h"

#include <cstring>

//----------------------------------------------------------------------------
H5FDdsmCommMpi::H5FDdsmCommMpi()
{
  this->InterComm = MPI_COMM_NULL;
  this->CommChannel = H5FD_DSM_INTRA_COMM;
}

//----------------------------------------------------------------------------
H5FDdsmCommMpi::~H5FDdsmCommMpi()
{
}

//----------------------------------------------------------------------------
void
H5FDdsmCommMpi::SetDsmMasterHostName(const char *hostName)
{
  strcpy(this->DsmMasterHostName, hostName);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::Init()
{
  if (H5FDdsmComm::Init() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  H5FDdsmDebug("CommMpi initialized");
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::Send(H5FDdsmMsg *Msg)
{
  H5FDdsmInt32   status;

  if(H5FDdsmComm::Send(Msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->CommChannel == H5FD_DSM_INTER_COMM) {
    H5FDdsmDebug("(" << this->Id << ") Sending to remote DSM " << Msg->Length << " bytes to " << Msg->Dest << " Tag = " << H5FDdsmTagToString(Msg->Tag));
    status = MPI_Send(Msg->Data, Msg->Length, MPI_UNSIGNED_CHAR, Msg->Dest, Msg->Tag, this->InterComm);
  }
  else {
    H5FDdsmDebug("(" << this->Id << ") Sending " << Msg->Length << " bytes to " << Msg->Dest << " Tag = " << H5FDdsmTagToString(Msg->Tag));
    status = MPI_Send(Msg->Data, Msg->Length, MPI_UNSIGNED_CHAR, Msg->Dest, Msg->Tag, this->IntraComm);
  }
  if(status != MPI_SUCCESS){
    H5FDdsmError("Id = " << this->Id << " MPI_Send failed to send " << Msg->Length << " Bytes to " << Msg->Dest);
    return(H5FD_DSM_FAIL);
  }
  H5FDdsmDebug("(" << this->Id << ") Sent " << Msg->Length << " bytes to " << Msg->Dest);

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::Receive(H5FDdsmMsg *Msg, H5FDdsmInt32 Channel)
{
  int            MessageLength;
  H5FDdsmInt32   status;
  H5FDdsmInt32   source = MPI_ANY_SOURCE;
  H5FDdsmInt32   receiveChannel = Channel;
  MPI_Status  SendRecvStatus;

  if (H5FDdsmComm::Receive(Msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  if (Msg->Source >= 0) source = Msg->Source;

  if (!receiveChannel) receiveChannel = this->CommChannel;

  if (receiveChannel == H5FD_DSM_INTER_COMM) {
    H5FDdsmDebug("(" << this->Id << ") Receiving from remote DSM " << Msg->Length << " bytes from " << source << " Tag = " << H5FDdsmTagToString(Msg->Tag));
    status = MPI_Recv(Msg->Data, Msg->Length, MPI_UNSIGNED_CHAR, source, Msg->Tag, this->InterComm, &SendRecvStatus);
  }
  else {
    H5FDdsmDebug("(" << this->Id << ") Receiving " << Msg->Length << " bytes from " << source << " Tag = " << H5FDdsmTagToString(Msg->Tag));
    status = MPI_Recv(Msg->Data, Msg->Length, MPI_UNSIGNED_CHAR, source, Msg->Tag, this->IntraComm, &SendRecvStatus);
  }
  if(status != MPI_SUCCESS){
    H5FDdsmError("Id = " << this->Id << " MPI_Recv failed to receive " << Msg->Length << " Bytes from " << Msg->Source);
    H5FDdsmError("MPI Error Code = " << SendRecvStatus.MPI_ERROR);
    return(H5FD_DSM_FAIL);
  }
  status = MPI_Get_count(&SendRecvStatus, MPI_UNSIGNED_CHAR, &MessageLength);
  H5FDdsmDebug("(" << this->Id << ") Received " << MessageLength << " bytes from " << SendRecvStatus.MPI_SOURCE);
  Msg->SetSource(SendRecvStatus.MPI_SOURCE);
  Msg->SetLength(MessageLength);
  if(status != MPI_SUCCESS){
    H5FDdsmError("MPI_Get_count failed ");
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::Probe(H5FDdsmMsg *Msg)
{
  int         nid, flag=0;
  MPI_Status  Status;

  if (H5FDdsmComm::Probe(Msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  MPI_Iprobe(MPI_ANY_SOURCE, Msg->Tag, this->IntraComm, &flag, &Status);
  H5FDdsmDebug("MPI_Iprobe " << H5FDdsmTagToString(Msg->Tag));
  if(flag){
    nid = Status.MPI_SOURCE;
    Msg->SetSource(nid);
    return(H5FD_DSM_SUCCESS);
  }
  return(H5FD_DSM_FAIL);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::Put(H5FDdsmMsg *DataMsg)
{
  if (H5FDdsmComm::Put(DataMsg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::Get(H5FDdsmMsg *DataMsg)
{
  if (H5FDdsmComm::Get(DataMsg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::WindowSync()
{
  if (H5FDdsmComm::WindowSync() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::OpenPort()
{
  if (H5FDdsmComm::OpenPort() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  if (this->Id == 0) {
    MPI_Open_port(MPI_INFO_NULL, this->DsmMasterHostName);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::ClosePort()
{
  if (H5FDdsmComm::ClosePort() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  if (this->Id == 0) {
    MPI_Close_port(this->DsmMasterHostName);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::Accept(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize)
{
  if (H5FDdsmComm::Accept(storagePointer, storageSize) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->UseStaticInterComm) {
    H5FDdsmInt32 global_size;
    MPI_Comm_size(MPI_COMM_WORLD, &global_size);
    MPI_Intercomm_create(this->IntraComm, 0, MPI_COMM_WORLD, global_size - (global_size - this->IntraSize), H5FD_DSM_DEFAULT_TAG, &this->InterComm);
  } else {
    // this->DsmMasterHostName internally used on root
    MPI_Comm_accept(this->DsmMasterHostName, MPI_INFO_NULL, 0, this->IntraComm, &this->InterComm);
  }

  MPI_Comm_remote_size(this->InterComm, &this->InterSize);
  H5FDdsmDebug("Server InterComm size: " << this->InterSize);
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::Connect()
{
  H5FDdsmInt32 isConnected = H5FD_DSM_FAIL;
  H5FDdsmInt32 status;

  if(H5FDdsmComm::Connect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->UseStaticInterComm) {
    status = MPI_Intercomm_create(this->IntraComm, 0, MPI_COMM_WORLD, 0, H5FD_DSM_DEFAULT_TAG, &this->InterComm);
    if (status == MPI_SUCCESS) {
      H5FDdsmDebug("Id = " << this->Id << " MPI_Intercomm_create returned SUCCESS");
      isConnected = H5FD_DSM_SUCCESS;
    }
  } else {
    //
    // Set Error handler to return so that a connection failure doesn't terminate the app
    MPI_Errhandler_set(this->IntraComm, MPI_ERRORS_RETURN);
    status = MPI_Comm_connect(this->DsmMasterHostName, MPI_INFO_NULL, 0, this->IntraComm, &this->InterComm);
    if (status == MPI_SUCCESS) {
      H5FDdsmDebug("Id = " << this->Id << " MPI_Comm_connect returned SUCCESS");
      isConnected = H5FD_DSM_SUCCESS;
    } else {
      char error_string[1024];
      int length_of_error_string;
      MPI_Error_string(status, error_string, &length_of_error_string);
      H5FDdsmDebug("\nMPI_Comm_connect failed with error : \n" << error_string << "\n\n");
    }
    // reset to MPI_ERRORS_ARE_FATAL for normal debug purposes
    MPI_Errhandler_set(this->IntraComm, MPI_ERRORS_ARE_FATAL);
  }

  if (isConnected == H5FD_DSM_SUCCESS) {
    this->CommChannel = H5FD_DSM_INTER_COMM;

    MPI_Comm_remote_size(this->InterComm, &this->InterSize);
    H5FDdsmDebug("Client InterComm size: " << this->InterSize);
    return(H5FD_DSM_SUCCESS);
  } else {
    return(H5FD_DSM_FAIL);
  }
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::Disconnect()
{
  if(H5FDdsmComm::Disconnect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->InterComm != MPI_COMM_NULL) {
    if (this->UseStaticInterComm) {
      MPI_Comm_free(&this->InterComm);
    } else {
      MPI_Comm_disconnect(&this->InterComm);
    }
    this->InterComm = MPI_COMM_NULL;
  }
  this->CommChannel = H5FD_DSM_INTRA_COMM;
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::RecvReady()
{
  H5FDdsmByte ready[6];

  if (H5FDdsmComm::RecvReady() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    MPI_Status status;
    if (MPI_Recv(ready, sizeof(ready), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &status) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of ready failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("Recv ready: " << ready);
  }
  this->Barrier();

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::SendReady()
{
  H5FDdsmByte ready[6] = "ready";

  if (H5FDdsmComm::SendReady() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    H5FDdsmDebug("Send ready: " << ready);
    if (MPI_Send(ready, sizeof(ready), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Send of ready failed");
      return(H5FD_DSM_FAIL);
    }
  }
  this->Barrier();

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::RecvInfo(H5FDdsmInfo *dsmInfo)
{
  if (H5FDdsmComm::RecvInfo(dsmInfo) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    MPI_Status status;
    H5FDdsmDebug("Receiving DSM Info...");
    if (MPI_Recv(dsmInfo, sizeof(H5FDdsmInfo), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &status) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of Info failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("Recv DSM Info Completed");
  }
  if (MPI_Bcast(dsmInfo, sizeof(H5FDdsmInfo), MPI_UNSIGNED_CHAR, 0, this->IntraComm) != MPI_SUCCESS){
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of Info failed");
    return(H5FD_DSM_FAIL);
  }

  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::SendInfo(H5FDdsmInfo *dsmInfo)
{
  if (H5FDdsmComm::SendInfo(dsmInfo) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    H5FDdsmDebug("Sending DSM Info...");
    if (MPI_Send(dsmInfo, sizeof(H5FDdsmInfo), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Send of Info failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("Send DSM Info Completed");
  }
  this->Barrier();
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::SendXML(H5FDdsmString file, H5FDdsmInt32 dest)
{
  MPI_Comm communicator;

  if (H5FDdsmComm::SendXML(file, dest) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (this->Id == 0) {
    if (this->CommChannel == H5FD_DSM_INTER_COMM) {
      communicator = this->InterComm;
    }
    else {
      communicator = this->IntraComm;
    }
    //
    H5FDdsmInt32 length = (H5FDdsmInt32)strlen(file) + 1;
    H5FDdsmDebug("Send XML to DSM Buffer object - FilePath: " << file);
    if (MPI_Send(file, length, MPI_CHAR, dest, H5FD_DSM_XML_TAG, communicator) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Send of XML file failed");
      return(H5FD_DSM_FAIL);
    }
  }
  H5FDdsmDebug("Send DSM XML Completed");
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::RecvXML(H5FDdsmString *file)
{
  MPI_Comm communicator;
  H5FDdsmInt32 length;

  if(H5FDdsmComm::RecvXML(file) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (this->CommChannel == H5FD_DSM_INTER_COMM) {
    communicator = this->InterComm;
  }
  else {
    communicator = this->IntraComm;
  }
  //
  MPI_Status status;
  MPI_Probe(0, H5FD_DSM_XML_TAG, communicator, &status);
  MPI_Get_count(&status, MPI_CHAR, &length);
  *file = new char[length];
  if (MPI_Recv(*file, length, MPI_CHAR, 0, H5FD_DSM_XML_TAG, communicator, &status) != MPI_SUCCESS){
    H5FDdsmError("Id = " << this->Id << " MPI_Recv of XML file failed");
    return(H5FD_DSM_FAIL);
  }
  H5FDdsmDebug("Recv DSM XML: " << *file);
  return(H5FD_DSM_SUCCESS);
}
