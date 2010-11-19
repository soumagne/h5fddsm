/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmComm.cxx

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
#include "H5FDdsmComm.h"
#include "H5FDdsmMsg.h"


H5FDdsmComm::H5FDdsmComm() {
}

H5FDdsmComm::~H5FDdsmComm() {
}

H5FDdsmInt32
H5FDdsmComm::Init(){
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::Check(H5FDdsmMsg *Msg){
    if(Msg->Tag <= 0) Msg->Tag = H5FD_DSM_DEFAULT_TAG;
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::Receive(H5FDdsmMsg *Msg){
    if(Msg->Tag <= 0) Msg->Tag = H5FD_DSM_DEFAULT_TAG;
    if(Msg->Length <= 0 ){
        H5FDdsmError("Cannot Receive Message of Length = " << Msg->Length);
        return(H5FD_DSM_FAIL);
    }
    if(Msg->Data <= 0 ){
        H5FDdsmError("Cannot Receive Message into Data Buffer = " << Msg->Length);
        return(H5FD_DSM_FAIL);
    }
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::Send(H5FDdsmMsg *Msg){
    if(Msg->Tag <= 0) Msg->Tag = H5FD_DSM_DEFAULT_TAG;
    if(Msg->Length <= 0 ){
        H5FDdsmError("Cannot Send Message of Length = " << Msg->Length);
        return(H5FD_DSM_FAIL);
    }
    if(Msg->Data <= 0 ){
        H5FDdsmError("Cannot Send Message from Data Buffer = " << Msg->Length);
        return(H5FD_DSM_FAIL);
    }
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::GetData(H5FDdsmMsg *DataMsg){
    if(DataMsg->Tag <= 0) DataMsg->Tag = H5FD_DSM_DEFAULT_TAG;
    if(DataMsg->Length <= 0 ){
        H5FDdsmError("Cannot Get Message of Length = " << DataMsg->Length);
        return(H5FD_DSM_FAIL);
    }
    if(DataMsg->Data <= 0 ){
        H5FDdsmError("Cannot Get Message into Data Buffer = " << DataMsg->Length);
        return(H5FD_DSM_FAIL);
    }
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::PutData(H5FDdsmMsg *DataMsg){
    if(DataMsg->Tag <= 0) DataMsg->Tag = H5FD_DSM_DEFAULT_TAG;
    if(DataMsg->Length <= 0 ){
        H5FDdsmError("Cannot Put Message of Length = " << DataMsg->Length);
        return(H5FD_DSM_FAIL);
    }
    if(DataMsg->Data <= 0 ){
        H5FDdsmError("Cannot Put Message from Data Buffer = " << DataMsg->Length);
        return(H5FD_DSM_FAIL);
    }
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::Barrier(){
    return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::OpenPort(){
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::ClosePort(){
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::RemoteCommAccept(void *storagePointer, H5FDdsmInt64 storageSize){
  if (!storagePointer) {
    H5FDdsmError("Null Data Pointer passed to CommAccept");
    return(H5FD_DSM_FAIL);
  }
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::RemoteCommConnect(){
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::RemoteCommDisconnect(){
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::RemoteCommSync(){
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::RemoteCommChannelSynced(H5FDdsmInt32 *sem){
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::RemoteCommRecvReady(){
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::RemoteCommSendReady(){
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::RemoteCommRecvInfo(H5FDdsmInt64 *length, H5FDdsmInt64 *totalLength,
    H5FDdsmInt32 *startServerId, H5FDdsmInt32 *endServerId){
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::RemoteCommSendInfo(H5FDdsmInt64 *length, H5FDdsmInt64 *totalLength,
    H5FDdsmInt32 *startServerId, H5FDdsmInt32 *endServerId){
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::RemoteCommSendXML(H5FDdsmString file, H5FDdsmInt32 dest){
  return(H5FD_DSM_SUCCESS);
}

H5FDdsmInt32
H5FDdsmComm::RemoteCommRecvXML(H5FDdsmString *file){
  return(H5FD_DSM_SUCCESS);
}
