/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmCommDmapp.h

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

#ifndef __H5FDdsmCommDmapp_h
#define __H5FDdsmCommDmapp_h

#include "H5FDdsmCommMpi.h"

struct H5FDdsmCommDmappInternals;

class H5FDdsm_EXPORT H5FDdsmCommDmapp : public H5FDdsmCommMpi {

public:
  H5FDdsmCommDmapp();
  virtual ~H5FDdsmCommDmapp();

  H5FDdsmInt32   Init();

  // Additional methods for one sided communications
  H5FDdsmInt32   WinCreateData(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm);
  H5FDdsmInt32   PutData(H5FDdsmMsg *msg);
  H5FDdsmInt32   GetData(H5FDdsmMsg *msg);
  H5FDdsmInt32   WindowSyncData();

  // Notification
  H5FDdsmInt32   WinCreateNotification(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm);
  H5FDdsmInt32   PutNotification(H5FDdsmMsg *msg);
  H5FDdsmInt32   GetNotification(H5FDdsmMsg *msg);

  // Lock
  H5FDdsmInt32   WinCreateLock(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm);
  H5FDdsmInt32   PutLock(H5FDdsmMsg *msg);
  H5FDdsmInt32   GetLock(H5FDdsmMsg *msg);

protected:
  // DMAPP wrappers
  H5FDdsmInt32   DmappWinCreateRemote(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize);
  H5FDdsmInt32   DmappWinFree();
  H5FDdsmInt32   DmappGatherIntraInstIds();
  H5FDdsmInt32   DmappGatherIntraMdhEntries();

  H5FDdsmCommDmappInternals *CommDmappInternals;
  H5FDdsmBoolean   IsDmappInitialized;
  H5FDdsmBoolean   UseBlockingComm;
};

#endif // __H5FDdsmCommDmapp_h
