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
  this->InterCommType = H5FD_DSM_COMM_MPI;
  this->InterComm = MPI_COMM_NULL;
  this->SetDsmMasterHostName("");
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
H5FDdsmCommMpi::Send(H5FDdsmMsg *msg)
{
  H5FDdsmInt32   status;
  MPI_Comm       comm;

  if (H5FDdsmComm::Send(msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  comm = (msg->Communicator == H5FD_DSM_INTER_COMM) ? this->InterComm : this->IntraComm;

  H5FDdsmDebug("(" << this->Id << ") Sending " << msg->Length << " bytes to "
      << msg->Dest << " Tag = " << H5FDdsmTagToString(msg->Tag));

  status = MPI_Send(msg->Data, msg->Length, MPI_UNSIGNED_CHAR, msg->Dest,
      msg->Tag, comm);

  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Send failed to send " << msg->Length
        << " Bytes to " << msg->Dest);
    return(H5FD_DSM_FAIL);
  }
  H5FDdsmDebug("(" << this->Id << ") Sent " << msg->Length << " bytes to " << msg->Dest);
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::Receive(H5FDdsmMsg *msg)
{
  int            message_length;
  H5FDdsmInt32   status;
  H5FDdsmInt32   source = MPI_ANY_SOURCE;
  MPI_Status     mpi_status;
  MPI_Comm       comm;

  if (H5FDdsmComm::Receive(msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (msg->Source >= 0) source = msg->Source;
  comm = (msg->Communicator == H5FD_DSM_INTER_COMM) ? this->InterComm : this->IntraComm;

  H5FDdsmDebug("(" << this->Id << ") Receiving " << msg->Length << " bytes from "
      << source << " Tag = " << H5FDdsmTagToString(msg->Tag));

  status = MPI_Recv(msg->Data, msg->Length, MPI_UNSIGNED_CHAR, source, msg->Tag,
      comm, &mpi_status);

  if (status != MPI_SUCCESS) {
    H5FDdsmError("Id = " << this->Id << " MPI_Recv failed to receive " << msg->Length
        << " Bytes from " << msg->Source);
    H5FDdsmError("MPI Error Code = " << mpi_status.MPI_ERROR);
    return(H5FD_DSM_FAIL);
  }

  status = MPI_Get_count(&mpi_status, MPI_UNSIGNED_CHAR, &message_length);
  H5FDdsmDebug("(" << this->Id << ") Received " << message_length << " bytes from "
      << mpi_status.MPI_SOURCE);
  msg->SetSource(mpi_status.MPI_SOURCE);
  msg->SetLength(message_length);

  if (status != MPI_SUCCESS) {
    H5FDdsmError("MPI_Get_count failed");
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::Probe(H5FDdsmMsg *msg)
{
  int          nid;
  int          flag = H5FD_DSM_FALSE;
  MPI_Status   status;
  MPI_Comm     comm = this->IntraComm;
  H5FDdsmInt32 commTag = H5FD_DSM_ANY_COMM;

  if (H5FDdsmComm::Probe(msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  H5FDdsmDebug("MPI_Iprobe " << H5FDdsmTagToString(msg->Tag));
  while (!flag) {
    MPI_Iprobe(MPI_ANY_SOURCE, msg->Tag, comm, &flag, &status);
    if (!flag && (this->InterComm != MPI_COMM_NULL) && (comm == this->IntraComm)) {
      comm = this->InterComm;
    }
  }

  if (flag) {
    nid = status.MPI_SOURCE;
    msg->SetSource(nid);
    commTag = (comm == this->IntraComm) ? H5FD_DSM_INTRA_COMM : H5FD_DSM_INTER_COMM;
    msg->SetCommunicator(commTag);
    H5FDdsmDebug("MPI_Iprobe found pending messages on comm " << H5FDdsmCommToString(commTag));
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
  if (H5FDdsmComm::Accept(storagePointer, storageSize) != H5FD_DSM_SUCCESS) {
    return(H5FD_DSM_FAIL);
  }

  if (this->UseStaticInterComm) {
    H5FDdsmInt32 global_size;
    // Sync server and client here
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Comm_size(MPI_COMM_WORLD, &global_size);
    MPI_Intercomm_create(this->IntraComm, 0, MPI_COMM_WORLD, global_size -
        (global_size - this->IntraSize), H5FD_DSM_DEFAULT_TAG, &this->InterComm);
  } else {
    // this->DsmMasterHostName internally used on root
    MPI_Comm_accept(this->DsmMasterHostName, MPI_INFO_NULL, 0, this->IntraComm,
        &this->InterComm);
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
  char error_string[1024];
  int length_of_error_string;

  if (H5FDdsmComm::Connect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->UseStaticInterComm) {
    // Sync server and client here
    MPI_Barrier(MPI_COMM_WORLD);
    // Set Error handler to return so that a connection failure doesn't terminate the app
    MPI_Errhandler_set(this->IntraComm, MPI_ERRORS_RETURN);
    status = MPI_Intercomm_create(this->IntraComm, 0, MPI_COMM_WORLD, 0,
        H5FD_DSM_DEFAULT_TAG, &this->InterComm);
    // reset to MPI_ERRORS_ARE_FATAL for normal debug purposes
    MPI_Errhandler_set(this->IntraComm, MPI_ERRORS_ARE_FATAL);
    if (status == MPI_SUCCESS) {
      H5FDdsmDebug("Id = " << this->Id << " MPI_Intercomm_create returned SUCCESS");
      isConnected = H5FD_DSM_SUCCESS;
    } else {
      MPI_Error_string(status, error_string, &length_of_error_string);
      H5FDdsmDebug("\nMPI_Intercomm_create failed with error : \n" << error_string << "\n\n");
    }
  } else {
    // Set Error handler to return so that a connection failure doesn't terminate the app
    MPI_Errhandler_set(this->IntraComm, MPI_ERRORS_RETURN);
    status = MPI_Comm_connect(this->DsmMasterHostName, MPI_INFO_NULL, 0,
        this->IntraComm, &this->InterComm);
    if (status == MPI_SUCCESS) {
      H5FDdsmDebug("Id = " << this->Id << " MPI_Comm_connect returned SUCCESS");
      isConnected = H5FD_DSM_SUCCESS;
    } else {
      MPI_Error_string(status, error_string, &length_of_error_string);
      H5FDdsmDebug("\nMPI_Comm_connect failed with error : \n" << error_string << "\n\n");
    }
    // reset to MPI_ERRORS_ARE_FATAL for normal debug purposes
    MPI_Errhandler_set(this->IntraComm, MPI_ERRORS_ARE_FATAL);
  }

  if (isConnected == H5FD_DSM_SUCCESS) {
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
  if (H5FDdsmComm::Disconnect() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->InterComm != MPI_COMM_NULL) {
    if (this->UseStaticInterComm) {
      MPI_Comm_free(&this->InterComm);
    } else {
      MPI_Comm_disconnect(&this->InterComm);
    }
    this->InterComm = MPI_COMM_NULL;
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::RemoteBarrier()
{
  if (H5FDdsmComm::RemoteBarrier() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (this->InterComm != MPI_COMM_NULL) MPI_Barrier(this->InterComm);
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
    if (MPI_Recv(ready, sizeof(ready), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG,
        this->InterComm, &status) != MPI_SUCCESS) {
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
    if (MPI_Send(ready, sizeof(ready), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG,
        this->InterComm) != MPI_SUCCESS) {
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
    if (MPI_Recv(dsmInfo, sizeof(H5FDdsmInfo), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG,
        this->InterComm, &status) != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Recv of Info failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("Recv DSM Info Completed");
  }
  if (MPI_Bcast(dsmInfo, sizeof(H5FDdsmInfo), MPI_UNSIGNED_CHAR, 0, this->IntraComm)
      != MPI_SUCCESS) {
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
    if (MPI_Send(dsmInfo, sizeof(H5FDdsmInfo), MPI_UNSIGNED_CHAR, 0, H5FD_DSM_EXCHANGE_TAG,
        this->InterComm) != MPI_SUCCESS) {
      H5FDdsmError("Id = " << this->Id << " MPI_Send of Info failed");
      return(H5FD_DSM_FAIL);
    }
    H5FDdsmDebug("Send DSM Info Completed");
  }
  this->Barrier();
  return(H5FD_DSM_SUCCESS);
}
