/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmLock.cxx

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

#include "H5FDdsmLock.h"

//----------------------------------------------------------------------------
// Declare extra debug info 
#undef H5FDdsmDebugLevel
#ifdef H5FDdsm_DEBUG_GLOBAL
#define H5FDdsmDebugLevel(level, x,y) \
{ if (this->DebugLevel >= level) { \
  std::cout << "H5FD_DSM Debug Level " << level << ": " << x << " " << this->Rank << " : " << y << std::endl; \
  } \
}
#else
#define H5FDdsmDebugLevel(level, x,y) \
{ if (this->Debug && this->DebugLevel >= level) { \
  std::cout << "H5FD_DSM Debug Level " << level << ": " << x << " " << this->Rank << " : " << y << std::endl; \
  } \
}
#endif

#define H5FD_DSM_CHANNEL_MACRO(def, value) \
  if (value==def) return #def;

H5FDdsmConstString H5FDdsmChannelToString(H5FDdsmInt32 channel) {
  H5FD_DSM_CHANNEL_MACRO(H5FD_DSM_SERVER_ID, channel)
  H5FD_DSM_CHANNEL_MACRO(H5FD_DSM_CLIENT_ID, channel)
  return "Channel UNDEFINED/UNRECOGNIZED";
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
H5FDdsmLock::H5FDdsmLock()
{
  this->Rank                = -1; 
  this->Master              = H5FD_DSM_FALSE;
  this->LockOwner           = -1;
  for (int i = 0; i < H5FD_DSM_NUM_CONNECTION_IDS; i++) {
    this->LockCount[i]                    = 0;
    this->UnlockedFlag[i]                 = H5FD_DSM_FALSE;
    this->SychronizationCount[i]          = 0;
    this->SychronizationCountInternal[i]  = 0;
  }
}
//----------------------------------------------------------------------------
H5FDdsmLock::~H5FDdsmLock()
{
  if (this->LockOwner!=-1) {
    H5FDdsmError("deleting without releasing lock");
  }
}
//----------------------------------------------------------------------------
H5FDdsmBoolean H5FDdsmLock::Lock(H5FDdsmInt32 channel)
{
  // if nobody owns the lock, we can try to take it
  if (this->LockOwner==-1) { 
    // when operating in synchronous mode, we are not allowed to take the lock 
    // unless the other side has just released it, a ping-pong mode
    if (!this->Master || this->SychronizationCountInternal[channel]<=0) {
      this->LockOwner                            = channel; 
      this->LockCount[channel]                   = 1;
      this->SychronizationCountInternal[channel] = this->SychronizationCount[channel];
      H5FDdsmDebugLevel(1,H5FDdsmChannelToString(channel), "Takes lock");
    }
    else {
      H5FDdsmDebugLevel(1,H5FDdsmChannelToString(channel)," Denied lock : SychronizationCount " << SychronizationCountInternal[channel]);
      this->LockQueue.push(channel);
      return H5FD_DSM_FALSE; 
    }
  }
  // if we already own it, increase lock count
  else if (this->LockOwner==channel) { 
    this->LockCount[channel]++;
    H5FDdsmDebugLevel(1,H5FDdsmChannelToString(channel)," lock count > " << this->LockCount[channel]);
  }
  // if someone else owns it, join the queue
  else { 
    this->LockQueue.push(channel);
    return H5FD_DSM_FALSE; 
  }
  return H5FD_DSM_TRUE; 
}
//----------------------------------------------------------------------------
H5FDdsmInt32 H5FDdsmLock::Unlock(H5FDdsmInt32 channel)
{
  if (this->LockOwner != channel) {
    H5FDdsmError(H5FDdsmChannelToString(channel) << ": Cannot unlock - owner is " << this->LockOwner);
    return(-1);
  }
  // if we already own it multiple times, decrease lock count
  else if (--this->LockCount[channel] > 0) {
    H5FDdsmDebugLevel(1,H5FDdsmChannelToString(channel)," lock count < " << this->LockCount[channel]);
  }
  // if this is the final unlock call, actually do an unlock operation
  else {
    //
    // reset lock flags
    //
    this->LockOwner             =-1;
    this->LockCount[channel]    = 0;
    //
    // When we release the lock, everyone else has their sync count decremented 
    // so they might be allowed to take the lock next
    //
    for (int i=0; i<H5FD_DSM_NUM_CONNECTION_IDS; i++) {
      if (i!=channel) {
        this->SychronizationCountInternal[i]--;
      }
    }
    //
    // check if we need to pass the lock on to another
    //
    if (!this->LockQueue.empty()) {
      H5FDdsmDebugLevel(1,H5FDdsmChannelToString(channel),"Handing lock over to " << H5FDdsmChannelToString(this->LockQueue.front()));
      if (this->Lock(this->LockQueue.front())) {
        H5FDdsmDebugLevel(1,H5FDdsmChannelToString(channel),"Handed lock over to " << H5FDdsmChannelToString(this->LockQueue.front()));
        this->LockQueue.pop();
      }
    }
  }
  return this->LockOwner;
}
