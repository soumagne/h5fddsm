/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmLock.h

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

#ifndef __H5FDdsmLock_h
#define __H5FDdsmLock_h

#include "H5FDdsmCondition.h"
#include "H5FDdsmComm.h"
#include <queue>

class H5FDdsm_EXPORT H5FDdsmLock : public H5FDdsmObject {

public:
  H5FDdsmLock();
  virtual ~H5FDdsmLock();

  // Description:
  // Request to acquire the lock from the client app (colour=1)
  H5FDdsmBoolean Lock(H5FDdsmInt32 channel);

  // Description:
  // Release the lock from the client app, if another has requested it, the lock is passed
  // on and the ID of the new lock owner is returned
  H5FDdsmInt32 Unlock(H5FDdsmInt32 channel);

  //
  H5FDdsmGetIndexValueMacro(UnlockedFlag, H5FDdsmBoolean);
  H5FDdsmSetIndexValueMacro(UnlockedFlag, H5FDdsmBoolean);

  // Server nodes are master nodes and honour all lock controls, 
  // but client nodes only have to obey what the master does, 
  // so some checks are ignored if not Master
  H5FDdsmSetValueMacro(Master, H5FDdsmBoolean);

  // For Debug messages
  H5FDdsmSetValueMacro(Rank, H5FDdsmInt32);

  void SetServerSychronizationCount(H5FDdsmInt32 count) {
    this->SychronizationCount[H5FD_DSM_SERVER_ID] = count;
  };
  void SetClientSychronizationCount(H5FDdsmInt32 count) {
    this->SychronizationCount[H5FD_DSM_CLIENT_ID] = count;
  };

protected:
  H5FDdsmInt32             Rank;
  H5FDdsmBoolean           Master;
  std::queue<H5FDdsmInt32> LockQueue;
  // only one thread actually modifies thes, but they might be read by another thread
  // so we'll declare them volatile just as a precaution
  volatile H5FDdsmInt32     LockOwner;
  volatile H5FDdsmInt32     LockCount[H5FD_DSM_NUM_CONNECTION_IDS];
  volatile H5FDdsmBoolean   UnlockedFlag[H5FD_DSM_NUM_CONNECTION_IDS];
  volatile H5FDdsmInt32     SychronizationCount[H5FD_DSM_NUM_CONNECTION_IDS];
  volatile H5FDdsmInt32     SychronizationCountInternal[H5FD_DSM_NUM_CONNECTION_IDS];
};

#endif // __H5FDdsmLock_h
