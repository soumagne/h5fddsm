/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmCommMpi.h

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
#ifndef __H5FDdsmCommMpi_h
#define __H5FDdsmCommMpi_h

#include "H5FDdsmComm.h"

class H5FDdsm_EXPORT H5FDdsmCommMpi : public H5FDdsmComm {

public:
  H5FDdsmCommMpi();
  virtual ~H5FDdsmCommMpi();

  H5FDdsmGetStringMacro(DsmMasterHostName);
  void SetDsmMasterHostName(H5FDdsmConstString hostName);

  H5FDdsmInt32   Init();
  H5FDdsmInt32   Send(H5FDdsmMsg *Msg);
  H5FDdsmInt32   Receive(H5FDdsmMsg *Msg, H5FDdsmInt32 Channel=0);
  // Additional methods for one sided communications
  H5FDdsmInt32   PutData(H5FDdsmMsg *DataMsg);
  H5FDdsmInt32   GetData(H5FDdsmMsg *DataMsg);
  //
  H5FDdsmInt32   Probe(H5FDdsmMsg *Msg);
  H5FDdsmInt32   Barrier();

  H5FDdsmInt32   OpenPort();
  H5FDdsmInt32   ClosePort();
  H5FDdsmInt32   RemoteCommAccept(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize);
  H5FDdsmInt32   RemoteCommConnect();
  H5FDdsmInt32   RemoteCommDisconnect();

  H5FDdsmInt32   RemoteCommSync();
  H5FDdsmInt32   RemoteCommRecvReady();
  H5FDdsmInt32   RemoteCommSendReady();

  H5FDdsmInt32   RemoteCommRecvInfo(H5FDdsmInfo *dsmInfo);
  H5FDdsmInt32   RemoteCommSendInfo(H5FDdsmInfo *dsmInfo);

  H5FDdsmInt32   RemoteCommSendXML(H5FDdsmString file, H5FDdsmInt32 dest);
  H5FDdsmInt32   RemoteCommRecvXML(H5FDdsmString *file);

protected:
  MPI_Comm       InterComm;
  MPI_Win        Win;
  H5FDdsmByte    DsmMasterHostName[MPI_MAX_PORT_NAME];
};

#endif // __H5FDdsmCommMpi_h
