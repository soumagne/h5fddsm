/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmMutex.h

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

#ifndef __H5FDdsmMutex_h
#define __H5FDdsmMutex_h

#include "H5FDdsmObject.h"

#ifdef _WIN32
  #include <windows.h>
  typedef CRITICAL_SECTION H5FDdsmMutexType;
#else
  #include <pthread.h>
  typedef pthread_mutex_t H5FDdsmMutexType;
#endif

class H5VLdso_EXPORT H5FDdsmMutex : public H5FDdsmObject {

public:
  H5FDdsmMutex();
  virtual ~H5FDdsmMutex();

  // Description:
  // Lock the Mutex, this is blocking
  void Lock();

  // Description:
  // Try to Lock the Mutex, this is non-blocking, returns true if locked, false otherwise.
  // if the return is false, it means another thread has the lock already.
  H5FDdsmBoolean TryLock();

  // Description:
  // Unlock the Mutex
  void Unlock();

protected:
  friend class H5FDdsmCondition;
  H5FDdsmMutexType Mutex;
};

#endif // __H5FDdsmMutex_h
