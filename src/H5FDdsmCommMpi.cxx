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

  H5FDdsmDebugLevel(1, "CommMpi initialized");
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::Send(H5FDdsmMsg *msg)
{
  if (H5FDdsmComm::Send(msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (msg->Communicator == H5FD_DSM_INTER_COMM) {
    if (this->MpiSend(msg, this->InterComm) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::Receive(H5FDdsmMsg *msg)
{
  if (H5FDdsmComm::Receive(msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (msg->Communicator == H5FD_DSM_INTER_COMM) {
    if (this->MpiReceive(msg, this->InterComm) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::Probe(H5FDdsmMsg *msg)
{
  if (H5FDdsmComm::Probe(msg) == H5FD_DSM_SUCCESS) return(H5FD_DSM_SUCCESS);

  if (msg->Communicator == H5FD_DSM_INTER_COMM) {
    if (this->MpiProbe(msg, this->InterComm) == H5FD_DSM_SUCCESS) return(H5FD_DSM_SUCCESS);
  }
  return(H5FD_DSM_FAIL);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::Barrier(H5FDdsmInt32 comm)
{
  if (H5FDdsmComm::Barrier(comm) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (comm == H5FD_DSM_INTER_COMM) {
    if (this->MpiBarrier(this->InterComm) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
    H5FDdsmDebugLevel(3, "(" << this->Id << ") Left Barrier on " << H5FDdsmCommToString(comm));
  }
  return(H5FD_DSM_SUCCESS);
}

//----------------------------------------------------------------------------
H5FDdsmInt32
H5FDdsmCommMpi::Broadcast(H5FDdsmMsg *msg)
{
  if (H5FDdsmComm::Broadcast(msg) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

  if (msg->Communicator == H5FD_DSM_INTER_COMM) {
    if (this->MpiBroadcast(msg, this->InterComm) != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);
  }
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
H5FDdsmCommMpi::Accept()
{
  if (H5FDdsmComm::Accept() != H5FD_DSM_SUCCESS) return(H5FD_DSM_FAIL);

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
  H5FDdsmDebugLevel(2, "Server InterComm size: " << this->InterSize);
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
      H5FDdsmDebugLevel(2, "Id = " << this->Id << " MPI_Intercomm_create returned SUCCESS");
      isConnected = H5FD_DSM_SUCCESS;
    } else {
      MPI_Error_string(status, error_string, &length_of_error_string);
      H5FDdsmDebugLevel(2, "\nMPI_Intercomm_create failed with error : \n" << error_string << "\n\n");
    }
  } else {
    // Set Error handler to return so that a connection failure doesn't terminate the app
    MPI_Errhandler_set(this->IntraComm, MPI_ERRORS_RETURN);
    status = MPI_Comm_connect(this->DsmMasterHostName, MPI_INFO_NULL, 0,
        this->IntraComm, &this->InterComm);
    if (status == MPI_SUCCESS) {
      H5FDdsmDebugLevel(2, "Id = " << this->Id << " MPI_Comm_connect returned SUCCESS");
      isConnected = H5FD_DSM_SUCCESS;
    } else {
      MPI_Error_string(status, error_string, &length_of_error_string);
      H5FDdsmDebugLevel(2, "\nMPI_Comm_connect failed with error : \n" << error_string << "\n\n");
    }
    // reset to MPI_ERRORS_ARE_FATAL for normal debug purposes
    MPI_Errhandler_set(this->IntraComm, MPI_ERRORS_ARE_FATAL);
  }

  if (isConnected == H5FD_DSM_SUCCESS) {
    MPI_Comm_remote_size(this->InterComm, &this->InterSize);
    H5FDdsmDebugLevel(2, "Client InterComm size: " << this->InterSize);
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
      this->MpiCommFree(&this->InterComm);
    } else {
      MPI_Comm_disconnect(&this->InterComm);
    }
    this->InterComm = MPI_COMM_NULL;
  }
  return(H5FD_DSM_SUCCESS);
}
