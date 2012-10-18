/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmThread.h

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

#ifndef __H5FDdsmThread_h
#define __H5FDdsmThread_h

#include "H5FDdsmObject.h"

#ifdef _WIN32
  #include <windows.h>
  typedef LPTHREAD_START_ROUTINE H5FDdsmThreadFunctionType;
  typedef HANDLE H5FDdsmThreadIDType;
  typedef DWORD H5FDdsmThreadReturnType;
  #define H5VLdso_THREAD_RETURN_TYPE H5FDdsmThreadReturnType WINAPI
#else
  #include <pthread.h>
  typedef pthread_t H5FDdsmThreadIDType;
  typedef void *(*H5FDdsmThreadFunctionType)(void *);
  typedef void *H5FDdsmThreadReturnType;
  #define H5VLdso_THREAD_RETURN_TYPE H5FDdsmThreadReturnType
#endif

class H5VLdso_EXPORT H5FDdsmThread : public H5FDdsmObject {

public:
  H5FDdsmThread();
  virtual ~H5FDdsmThread();

  // Description:
  // Create a new thread for the given function.
  void SpawnThread(H5FDdsmThreadFunctionType f, void *data);

  // Description:
  // Wait for thread completion.
  void JoinThread();

  // Description:
  // Terminate the thread.
  void TerminateThread();

protected:
  H5FDdsmThreadIDType ThreadID;
};

#endif // __H5FDdsmThread_h
