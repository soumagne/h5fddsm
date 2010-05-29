/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmComm.h

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
#ifndef __H5FDdsmComm_h
#define __H5FDdsmComm_h

#include "H5FDdsmObject.h"


//! Base comm object for Distributed Shared Memory implementation

// Macros to choose communication system to use
#define H5FD_DSM_COMM_MPI        0x10
#define H5FD_DSM_COMM_SOCKET     0x11

// Macros to switch between remote/local channels
#define H5FD_DSM_COMM_CHANNEL_LOCAL  0x20
#define H5FD_DSM_COMM_CHANNEL_REMOTE 0x21

class H5FDdsmMsg;

class H5FDdsm_EXPORT H5FDdsmComm : public H5FDdsmObject {

public:
  H5FDdsmComm();
  virtual ~H5FDdsmComm();

//! Id 
    H5FDdsmGetValueMacro(Id, H5FDdsmInt32);
    H5FDdsmSetValueMacro(Id, H5FDdsmInt32);

//! Total 
    H5FDdsmGetValueMacro(TotalSize, H5FDdsmInt32);
    H5FDdsmSetValueMacro(TotalSize, H5FDdsmInt32);

    //! CommType
    H5FDdsmGetValueMacro(CommType, H5FDdsmInt32);
    H5FDdsmSetValueMacro(CommType, H5FDdsmInt32);

    //! CommChannel
    H5FDdsmGetValueMacro(CommChannel, H5FDdsmInt32);
    H5FDdsmSetValueMacro(CommChannel, H5FDdsmInt32);

    virtual H5FDdsmInt32   Init();
    virtual H5FDdsmInt32   Send(H5FDdsmMsg *Msg);
    virtual H5FDdsmInt32   Receive(H5FDdsmMsg *Msg);
    virtual H5FDdsmInt32   Check(H5FDdsmMsg *Msg);
    virtual H5FDdsmInt32   Barrier();

    // H5FDdsmInt32   Send() { return(this->Send(this->Msg)); };
    // H5FDdsmInt32   Receive() { return(this->Receive(this->Msg)); };
    // H5FDdsmInt32   Check() { return(this->Check(this->Msg)); };

    virtual H5FDdsmInt32   OpenPort();
    virtual H5FDdsmInt32   ClosePort();
    virtual H5FDdsmInt32   RemoteCommAccept();
    virtual H5FDdsmInt32   RemoteCommConnect();
    virtual H5FDdsmInt32   RemoteCommDisconnect();

    virtual H5FDdsmInt32   RemoteCommRecvReady();
    virtual H5FDdsmInt32   RemoteCommSendReady();

    virtual H5FDdsmInt32   RemoteCommRecvInfo(H5FDdsmInt64 *length, H5FDdsmInt64 *totalLength,
        H5FDdsmInt32 *startServerId, H5FDdsmInt32 *endServerId);
    virtual H5FDdsmInt32   RemoteCommSendInfo(H5FDdsmInt64 *length, H5FDdsmInt64 *totalLength,
        H5FDdsmInt32 *startServerId, H5FDdsmInt32 *endServerId);

    virtual H5FDdsmInt32   RemoteCommSendXML(H5FDdsmString file, H5FDdsmInt32 dest);
    virtual H5FDdsmInt32   RemoteCommRecvXML(H5FDdsmString *file);

    virtual H5FDdsmInt32   HasStillData();

protected:
    H5FDdsmInt32       Id;
    H5FDdsmInt32       TotalSize;
    H5FDdsmInt32       CommType;
    H5FDdsmInt32       CommChannel;
};

#endif // __H5FDdsmComm_h
