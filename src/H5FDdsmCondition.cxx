/*=========================================================================

  Project                 : H5FDdsm
  Module                  : H5FDdsmCondition.cxx

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

#include "H5FDdsmCondition.h"

//----------------------------------------------------------------------------
H5FDdsmCondition::H5FDdsmCondition()
{
#ifdef _WIN32
#if (WINVER < _WIN32_WINNT_LONGHORN)
  this->Condition = CreateEvent(NULL, TRUE, FALSE, NULL);
#else
  InitializeConditionVariable(&this->Condition);
#endif
#else
  pthread_cond_init(&this->Condition, NULL);
#endif
}

//----------------------------------------------------------------------------
H5FDdsmCondition::~H5FDdsmCondition()
{
#ifdef _WIN32
#if (WINVER < _WIN32_WINNT_LONGHORN)
  CloseHandle(this->Condition);
#endif
#else
  pthread_cond_destroy(&this->Condition);
#endif
}

//----------------------------------------------------------------------------
void H5FDdsmCondition::SetName(const char *name)
{
  this->ConditionName = name;
}

//----------------------------------------------------------------------------
void
H5FDdsmCondition::Wait()
{
  this->Wait_(this->ConditionMutex);
}
//----------------------------------------------------------------------------
void
H5FDdsmCondition::Signal()
{
#ifdef _WIN32
#if (WINVER < _WIN32_WINNT_LONGHORN)
  SetEvent(this->Condition);
#else
  WakeConditionVariable(&this->Condition);
#endif
#else
  pthread_cond_signal(&this->Condition);
#endif
  H5FDdsmDebugLevel(1,"         : Sent signal for condition " << this->ConditionName.c_str());
}

//----------------------------------------------------------------------------
void
H5FDdsmCondition::Wait_(H5FDdsmMutex &mutex)
{
  H5FDdsmDebugLevel(1,"         : Waiting for condition " << this->ConditionName.c_str());
#ifdef _WIN32
#if (WINVER < _WIN32_WINNT_LONGHORN)
  WaitForSingleObject(this->Condition, INFINITE);
  ResetEvent(this->Condition);
#else
  SleepConditionVariableCS(&this->Condition, &mutex.Mutex, INFINITE);
#endif
#else
  pthread_cond_wait(&this->Condition, &mutex.Mutex);
#endif
  H5FDdsmDebugLevel(1,"         : Finished waiting for condition " << this->ConditionName.c_str());
}
//----------------------------------------------------------------------------
void
H5FDdsmCondition::LockMutex()
{
  this->ConditionMutex.Lock();
}
//----------------------------------------------------------------------------
void
H5FDdsmCondition::UnlockMutex()
{
  this->ConditionMutex.Unlock();
}
