/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmCommMpiRma.h

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

#ifndef __H5FDdsmCommMpiRma_h
#define __H5FDdsmCommMpiRma_h

#include "H5FDdsmCommMpi.h"

class H5VLdso_EXPORT H5FDdsmCommMpiRma : public H5FDdsmCommMpi {

public:
  H5FDdsmCommMpiRma();
  virtual ~H5FDdsmCommMpiRma();

  H5FDdsmInt32   Init();

  // Additional methods for one sided communications
  H5FDdsmInt32   WinCreateData(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm);
  H5FDdsmInt32   PutData(H5FDdsmMsg *msg);
  H5FDdsmInt32   GetData(H5FDdsmMsg *msg);

  // Notification
  H5FDdsmInt32   WinCreateNotification(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm);
  H5FDdsmInt32   PutNotification(H5FDdsmMsg *msg);
  H5FDdsmInt32   GetNotification(H5FDdsmMsg *msg);

  // Lock
  H5FDdsmInt32   WinCreateLock(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, H5FDdsmInt32 comm);
  H5FDdsmInt32   PutLock(H5FDdsmMsg *msg);
  H5FDdsmInt32   GetLock(H5FDdsmMsg *msg);

protected:
  // RMA wrappers
  H5FDdsmInt32   MpiWinCreateRemote(H5FDdsmPointer storagePointer, H5FDdsmUInt64 storageSize, MPI_Win *win);

  MPI_Win        InterWin;
  MPI_Win        NotificationWin;
  MPI_Win        LockWin;

private:
};

#endif // __H5FDdsmCommMpiRma_h
