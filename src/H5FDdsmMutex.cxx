/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmMutex.cxx

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

#include "H5FDdsmMutex.h"

//----------------------------------------------------------------------------
H5FDdsmMutex::H5FDdsmMutex()
{
#ifdef _WIN32
  InitializeCriticalSection(&this->Mutex);
#else
  pthread_mutex_init(&this->Mutex, NULL);
#endif
}

//----------------------------------------------------------------------------
H5FDdsmMutex::~H5FDdsmMutex()
{
#ifdef _WIN32
  DeleteCriticalSection(&this->Mutex);
#else
  pthread_mutex_destroy(&this->Mutex);
#endif
}

//----------------------------------------------------------------------------
void
H5FDdsmMutex::Lock()
{
#ifdef _WIN32
  EnterCriticalSection(&this->Mutex);
#else
  pthread_mutex_lock(&this->Mutex);
#endif
}

//----------------------------------------------------------------------------
H5FDdsmBoolean
H5FDdsmMutex::TryLock()
{
  int owned;
#ifdef _WIN32
  owned = TryEnterCriticalSection(&this->Mutex);
  return(owned != 0);
#else
  owned = pthread_mutex_trylock(&this->Mutex);
  return(owned == 0);
#endif
}

//----------------------------------------------------------------------------
void
H5FDdsmMutex::Unlock()
{
#ifdef _WIN32
  LeaveCriticalSection(&this->Mutex);
#else
  pthread_mutex_unlock(&this->Mutex);
#endif
}
