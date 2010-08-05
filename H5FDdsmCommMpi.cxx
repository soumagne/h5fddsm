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

H5FDdsmCommMpi::H5FDdsmCommMpi() {
    this->Comm = MPI_COMM_WORLD;
    this->CommType = H5FD_DSM_COMM_MPI;
    this->InterComm = MPI_COMM_NULL;
    this->CommChannel = H5FD_DSM_COMM_CHANNEL_LOCAL;
}

H5FDdsmCommMpi::~H5FDdsmCommMpi() {}

void
H5FDdsmCommMpi::SetDsmMasterHostName(const char *hostName) {
    strcpy(this->DsmMasterHostName, hostName);
}

H5FDdsmInt32
H5FDdsmCommMpi::DupComm(MPI_Comm Source){
    MPI_Comm    NewComm;

    MPI_Comm_dup(Source, &NewComm);
    return(this->SetComm(NewComm));
}

H5FDdsmInt32
H5FDdsmCommMpi::Init(){
    int size, rank;

    if(MPI_Comm_size(this->Comm, &size) != MPI_SUCCESS) return(H5FD_DSM_FAIL);
    if(MPI_Comm_rank(this->Comm, &rank) != MPI_SUCCESS) return(H5FD_DSM_FAIL);

    this->SetId(rank);
    this->SetTotalSize(size);

    H5FDdsmDebug("CommMpi initialized");
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmCommMpi::Check(H5FDdsmMsg *Msg){
    int         nid, flag=0;
    MPI_Status  Status;

    if(H5FDdsmComm::Check(Msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
     MPI_Iprobe(MPI_ANY_SOURCE, Msg->Tag, this->Comm, &flag, &Status);
     H5FDdsmDebug("MPI_Iprobe " << H5FDdsmTagToString(Msg->Tag) << ", " << this->Comm);
    if(flag){
        nid = Status.MPI_SOURCE;
        Msg->SetSource(nid);
        return(H5FD_DSM_SUCCESS);
    }
    return(H5FD_DSM_FAIL);
}

H5FDdsmInt32
H5FDdsmCommMpi::Receive(H5FDdsmMsg *Msg){
    int            MessageLength;
    H5FDdsmInt32   status;
    H5FDdsmInt32   source = MPI_ANY_SOURCE;
    MPI_Status  SendRecvStatus;

    if(H5FDdsmComm::Receive(Msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
    if(Msg->Source >= 0) source = Msg->Source;

    if (this->CommChannel == H5FD_DSM_COMM_CHANNEL_REMOTE) {
      H5FDdsmDebug("(" << this->Id << ") Receiving from remote DSM " << Msg->Length << " bytes from " << source << " Tag = " << H5FDdsmTagToString(Msg->Tag));
      status = MPI_Recv(Msg->Data, Msg->Length, MPI_UNSIGNED_CHAR, source, Msg->Tag, this->InterComm, &SendRecvStatus);
    }
    else {
      H5FDdsmDebug("(" << this->Id << ") Receiving " << Msg->Length << " bytes from " << source << " Tag = " << H5FDdsmTagToString(Msg->Tag));
      status = MPI_Recv(Msg->Data, Msg->Length, MPI_UNSIGNED_CHAR, source, Msg->Tag, this->Comm, &SendRecvStatus);
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

H5FDdsmInt32
H5FDdsmCommMpi::Send(H5FDdsmMsg *Msg){
    H5FDdsmInt32   status;

    if(H5FDdsmComm::Send(Msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

    H5FDdsmFloat64 startTime = MPI_Wtime();

    if (this->CommChannel == H5FD_DSM_COMM_CHANNEL_REMOTE) {
      H5FDdsmDebug("(" << this->Id << ") Sending to remote DSM " << Msg->Length << " bytes to " << Msg->Dest << " Tag = " << H5FDdsmTagToString(Msg->Tag));
      status = MPI_Send(Msg->Data, Msg->Length, MPI_UNSIGNED_CHAR, Msg->Dest, Msg->Tag, this->InterComm);
    }
    else {
      H5FDdsmDebug("(" << this->Id << ") Sending " << Msg->Length << " bytes to " << Msg->Dest << " Tag = " << H5FDdsmTagToString(Msg->Tag));
      status = MPI_Send(Msg->Data, Msg->Length, MPI_UNSIGNED_CHAR, Msg->Dest, Msg->Tag, this->Comm);
    }
    if(status != MPI_SUCCESS){
        H5FDdsmError("Id = " << this->Id << " MPI_Send failed to send " << Msg->Length << " Bytes to " << Msg->Dest);
        return(H5FD_DSM_FAIL);
    }

    H5FDdsmFloat64 endTime = MPI_Wtime();
    if ((Msg->Tag == H5FD_DSM_PUT_DATA_TAG) && (Msg->Length > 1024.f)) {
      //
      // This value is only valid when transferring big chunks of data when local
      // buffers are saturated. It does not measure the end-to-end tranfer rate.
      this->TransferRate = Msg->Length/((endTime - startTime)*1024*1024);
      // cout << "(" << this->Id << ") Sent " << Msg->Length << " bytes to "
      //     << Msg->Dest << " at " << this->TransferRate << " MB/s" << endl;
      //
    }
    H5FDdsmDebug("(" << this->Id << ") Sent " << Msg->Length << " bytes to " << Msg->Dest);

    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmCommMpi::Barrier() {

  if(H5FDdsmComm::Barrier() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  MPI_Barrier(this->Comm);

  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmCommMpi::OpenPort() {

  if(H5FDdsmComm::OpenPort() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  if (this->Id == 0) {
    MPI_Open_port(MPI_INFO_NULL, this->DsmMasterHostName);
  }
  this->Barrier();
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmCommMpi::ClosePort() {

  if(H5FDdsmComm::ClosePort() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  if (this->Id == 0) {
    MPI_Close_port(this->DsmMasterHostName);
  }
  this->Barrier();
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmCommMpi::RemoteCommAccept() {

  if(H5FDdsmComm::RemoteCommAccept() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  // this->DsmMasterHostName internally used on root
  MPI_Comm_accept(this->DsmMasterHostName, MPI_INFO_NULL, 0, this->Comm, &this->InterComm);
  this->CommChannel = H5FD_DSM_COMM_CHANNEL_REMOTE;

  if (this->Id == 0) {
    MPI_Status status;
    if (MPI_Recv(&this->InterSize, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &status) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of InterSize failed");
      return(H5FD_DSM_FAIL);
    }
    if (MPI_Send(&this->TotalSize, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Send of TotalSize failed");
      return(H5FD_DSM_FAIL);
    }
  }
  if (MPI_Bcast(&this->InterSize, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, this->Comm) != MPI_SUCCESS){
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of InterSize failed");
    return(H5FD_DSM_FAIL);
  }
  H5FDdsmDebug("InterComm size: " << this->InterSize);
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmCommMpi::RemoteCommConnect() {

  if(H5FDdsmComm::RemoteCommConnect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  MPI_Comm_connect(this->DsmMasterHostName, MPI_INFO_NULL, 0, this->Comm, &this->InterComm);
  this->CommChannel = H5FD_DSM_COMM_CHANNEL_REMOTE;

  if (this->Id == 0) {
    MPI_Status status;
    if (MPI_Send(&this->TotalSize, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Send of TotalSize failed");
      return(H5FD_DSM_FAIL);
    }
    if (MPI_Recv(&this->InterSize, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &status) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of InterSize failed");
      return(H5FD_DSM_FAIL);
    }
  }
  if (MPI_Bcast(&this->InterSize, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, this->Comm) != MPI_SUCCESS){
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of InterSize failed");
    return(H5FD_DSM_FAIL);
  }
  H5FDdsmDebug("InterComm size: " << this->InterSize);
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmCommMpi::RemoteCommDisconnect() {

  if(H5FDdsmComm::RemoteCommDisconnect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  this->Barrier();
  if (this->InterComm) {
    MPI_Comm_disconnect(&this->InterComm);
    this->InterComm = MPI_COMM_NULL;
  }
  this->CommChannel = H5FD_DSM_COMM_CHANNEL_LOCAL;
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmCommMpi::RemoteCommRecvReady() {

  if (H5FDdsmComm::RemoteCommRecvReady() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  H5FDdsmByte ready[6];

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

H5FDdsmInt32
H5FDdsmCommMpi::RemoteCommSendReady() {

  if (H5FDdsmComm::RemoteCommSendReady() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  H5FDdsmByte ready[6] = "ready";

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

H5FDdsmInt32
H5FDdsmCommMpi::RemoteCommRecvInfo(H5FDdsmInt64 *length, H5FDdsmInt64 *totalLength,
    H5FDdsmInt32 *startServerId, H5FDdsmInt32 *endServerId) {

  if(H5FDdsmComm::RemoteCommRecvInfo(length, totalLength, startServerId, endServerId) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    MPI_Status status;
    if (MPI_Recv(length, sizeof(H5FDdsmInt64), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &status) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of length failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("Recv DSM length: " << *length);
  }
  if (MPI_Bcast(length, sizeof(H5FDdsmInt64), MPI_UNSIGNED_CHAR, 0, this->Comm) != MPI_SUCCESS){
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of length failed");
    return(H5FD_DSM_FAIL);
  }

  if (this->Id == 0) {
    MPI_Status status;
    if (MPI_Recv(totalLength, sizeof(H5FDdsmInt64), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &status) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of totalLength failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("Recv DSM totalLength: " << *totalLength);
  }
  if (MPI_Bcast(totalLength, sizeof(H5FDdsmInt64), MPI_UNSIGNED_CHAR, 0, this->Comm) != MPI_SUCCESS){
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of totalLength failed");
    return(H5FD_DSM_FAIL);
  }

  if (this->Id == 0) {
    MPI_Status status;
    if (MPI_Recv(startServerId, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &status) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of startServerId failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("Recv DSM startServerId: " << *startServerId);
  }
  if (MPI_Bcast(startServerId, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, this->Comm) != MPI_SUCCESS){
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of startServerId failed");
    return(H5FD_DSM_FAIL);
  }

  if (this->Id == 0) {
    MPI_Status status;
    if (MPI_Recv(endServerId, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm, &status) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of endServerId failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("Recv DSM endServerId: " << *endServerId);
  }
  if (MPI_Bcast(endServerId, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, this->Comm) != MPI_SUCCESS){
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of endServerId failed");
    return(H5FD_DSM_FAIL);
  }
  H5FDdsmDebug("Recv DSM Info Completed");

  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmCommMpi::RemoteCommSendInfo(H5FDdsmInt64 *length, H5FDdsmInt64 *totalLength,
    H5FDdsmInt32 *startServerId, H5FDdsmInt32 *endServerId) {

  if(H5FDdsmComm::RemoteCommSendInfo(length, totalLength, startServerId, endServerId) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->Id == 0) {
    // Length
    H5FDdsmDebug("Send DSM length: " << *length);
    if (MPI_Send(length, sizeof(H5FDdsmInt64), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Send of length failed");
      return(H5FD_DSM_FAIL);
    }
    // TotalLength
    H5FDdsmDebug("Send DSM totalLength: " << *totalLength);
    if (MPI_Send(totalLength, sizeof(H5FDdsmInt64), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Send of totalLength failed");
      return(H5FD_DSM_FAIL);
    }

    // StartServerId
    H5FDdsmDebug("Send DSM startServerId: " << *startServerId);
    if (MPI_Send(startServerId, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Send of startServerId failed");
      return(H5FD_DSM_FAIL);
    }

    // EndServerId
    H5FDdsmDebug("Send DSM endServerId: " << *endServerId);
    if (MPI_Send(endServerId, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG, this->InterComm) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Send of endServerId failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("Send DSM Info Completed");
  }
  this->Barrier();
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmCommMpi::RemoteCommSendXML(H5FDdsmString file, H5FDdsmInt32 dest)
{
  MPI_Comm communicator;
  if(H5FDdsmComm::RemoteCommSendXML(file, dest) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (this->Id == 0) {
    if (this->CommChannel == H5FD_DSM_COMM_CHANNEL_REMOTE) {
      communicator = this->InterComm;
    }
    else {
      communicator = this->Comm;
    }
    //
    H5FDdsmInt32 length = strlen(file) + 1;
    H5FDdsmDebug("Send XML to DSM Buffer object - FilePath: " << file);
    if (MPI_Send(file, length, MPI_CHAR, dest, H5FD_DSM_XML_TAG, communicator) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Send of XML file failed");
      return(H5FD_DSM_FAIL);
    }
  }
  H5FDdsmDebug("Send DSM XML Completed");
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmCommMpi::RemoteCommRecvXML(H5FDdsmString *file)
{
  MPI_Comm communicator;
  H5FDdsmInt32 length;

  if(H5FDdsmComm::RemoteCommRecvXML(file) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (this->CommChannel == H5FD_DSM_COMM_CHANNEL_REMOTE) {
    communicator = this->InterComm;
  }
  else {
    communicator = this->Comm;
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

H5FDdsmInt32
H5FDdsmCommMpi::RemoteCommSendSteeringCmd(H5FDdsmString cmd)
{
  if (H5FDdsmComm::RemoteCommSendSteeringCmd(cmd) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (this->Id == 0) {
    H5FDdsmInt32 length = strlen(cmd) + 1;
    if (MPI_Send(cmd, length, MPI_CHAR, 0, H5FD_DSM_STEERING_TAG, this->InterComm) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Send of steering cmd failed");
      return(H5FD_DSM_FAIL);
    }
  }
  this->Barrier();
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmCommMpi::RemoteCommRecvSteeringCmd(H5FDdsmString *cmd)
{
  H5FDdsmInt32 length; // string is null terminated on send
  //
  if (H5FDdsmComm::RemoteCommRecvSteeringCmd(cmd) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  //
  if (this->Id == 0) {
    MPI_Status status;
    MPI_Probe(0, H5FD_DSM_STEERING_TAG, this->InterComm, &status);
    MPI_Get_count(&status, MPI_CHAR, &length);
    *cmd = new char[length];
    if (MPI_Recv(*cmd, length, MPI_CHAR, 0, H5FD_DSM_STEERING_TAG, this->InterComm, &status) != MPI_SUCCESS){
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of steering cmd failed");
      return(H5FD_DSM_FAIL);
    }
  }
  if (MPI_Bcast(&length, sizeof(H5FDdsmInt32), MPI_UNSIGNED_CHAR, 0, this->Comm) != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of length failed");
    return(H5FD_DSM_FAIL);
  }
  if (this->Id != 0) *cmd = new char[length];
  if (MPI_Bcast(*cmd, length, MPI_CHAR, 0, this->Comm) != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Bcast of steering cmd failed");
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmCommMpi::HasStillData()
{
  H5FDdsmInt32 ret = H5FD_DSM_TRUE;
  static H5FDdsmInt32 chanCleared = 0;

  chanCleared++;
  if (chanCleared == this->InterSize) {
    H5FDdsmDebug("Channels cleared: " << chanCleared << "/" << this->InterSize);
    chanCleared = 0;
    ret = H5FD_DSM_FALSE;
  }

  return(ret);
}
